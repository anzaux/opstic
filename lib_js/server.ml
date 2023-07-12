open! Types
open! Monad

let ( let* ) = Monad.bind

let hash_find ~descr h k =
  match Hashtbl.find_opt h k with
  | Some x -> return x
  | None -> error_with descr

type path_spec = { path : path; path_kind : path_kind; path_role : role }

type service_spec = {
  service_id : ServiceId.t;
  path_specs : (path, path_spec) Hashtbl.t;
  my_role : role;
  other_roles : role list;
  parse_session_id : payload -> session_id;
      (* path, header 等も含めたい 生の http request という abstractionがあれば…*)
}

type response = { response_role : role; response_body : payload }

type request = {
  request_pathspec : path_spec;
  request_body : payload;
  request_resolv : response waiting;
}

(* greeting_with_id: 最初だけ / greeting: 最初でも途中でもありうる *)
type greeting_queue = request ConcurrentQueue.t
type invitation_queue = (session_id * request) ConcurrentQueue.t
type request_queue = request ConcurrentQueue.t
type response_queue = response ConcurrentQueue.t
type queue = { request_queue : request_queue; response_queue : response_queue }

type session = {
  session_id : session_id;
  queues : (role, queue) Hashtbl.t;
  service_ref : service;
}

and service = {
  spec : service_spec;
  greetings : (path, greeting_queue) Hashtbl.t;
  invitations : (path, invitation_queue) Hashtbl.t;
  sessions : (session_id, session) Hashtbl.t;
  server_ref : t;
}

and t = {
  services : (ServiceId.t, service) Hashtbl.t;
  service_path : (path, service_id) Hashtbl.t;
}

module Util = struct
  let get_service t service_id =
    hash_find t.services service_id
      ~descr:(Format.asprintf "No entry point: %a" ServiceId.pp service_id)

  let get_service_from_path t path =
    let* service_id =
      hash_find
        ~descr:(Format.asprintf "No path %a" Path.pp path)
        t.service_path path
    in
    hash_find t.services service_id
      ~descr:(Format.asprintf "No entry point: %a" ServiceId.pp service_id)

  let get_greeting_queue_ service path =
    hash_find service.greetings path
      ~descr:
        (Format.asprintf "Service %a Role %a is not accepting peer" ServiceId.pp
           service.spec.service_id Path.pp path)

  let get_greeting_queue t service_id path =
    let* service = get_service t service_id in
    get_greeting_queue_ service path

  let get_invitation_queue_ service path =
    hash_find service.invitations path
      ~descr:
        (Format.asprintf "Service %a Role %a is not accepting peer" ServiceId.pp
           service.spec.service_id Path.pp path)

  let get_invitation_queue t service_id path =
    let* service = get_service t service_id in
    get_invitation_queue_ service path

  let get_session t service_id session_id =
    let* service = get_service t service_id in
    hash_find service.sessions session_id
      ~descr:
        (Format.asprintf "No session id %a for service %a" SessionId.pp
           session_id ServiceId.pp service_id)

  let get_queue_ session role =
    hash_find session.queues role
      ~descr:
        (Format.asprintf "No role %a for session %a (service %a)" Role.pp role
           SessionId.pp session.session_id ServiceId.pp
           session.service_ref.spec.service_id)

  let get_queue t service_id session_id role =
    let* session = get_session t service_id session_id in
    get_queue_ session role
end

let create_server () : t =
  { services = Hashtbl.create 42; service_path = Hashtbl.create 42 }

let register_service (server : t) ~spec =
  let sv =
    {
      spec;
      greetings = Hashtbl.create 42;
      invitations = Hashtbl.create 42;
      sessions = Hashtbl.create 42;
      server_ref = server;
    }
  in
  let register_path path =
    Hashtbl.replace server.service_path path.path sv.spec.service_id;
    match path.path_kind with
    | `Greeting ->
        Hashtbl.replace sv.greetings path.path (ConcurrentQueue.create ())
    | `Invitation ->
        Hashtbl.replace sv.invitations path.path (ConcurrentQueue.create ())
    | `Established -> ()
  in
  Hashtbl.to_seq_values spec.path_specs |> Seq.iter register_path;
  Hashtbl.replace server.services spec.service_id sv;
  ()

let create_session service session_id =
  let roles = service.spec.other_roles in
  let queues = Hashtbl.create (List.length roles) in
  roles
  |> List.iter (fun role ->
         Hashtbl.replace queues role
           {
             request_queue = ConcurrentQueue.create ();
             response_queue = ConcurrentQueue.create ();
           });
  { session_id; queues; service_ref = service }

let new_session service session_id =
  let session = create_session service session_id in
  Hashtbl.replace service.sessions session_id session;
  session

let handle_entry server ~path (request : payload) : payload io =
  let promise, resolv = Monad.create_promise () in
  let resolv = function
    | Ok response -> resolv (Ok response.response_body)
    | Error err -> resolv (Error err)
  in
  let path = Path.create path in
  let* service = Util.get_service_from_path server path in
  let path_spec = Hashtbl.find service.spec.path_specs path in
  let role = path_spec.path_role in
  let request =
    {
      request_pathspec = path_spec;
      request_body = request;
      request_resolv = resolv;
    }
  in
  let service_id = service.spec.service_id in
  match path_spec.path_kind with
  | `Greeting ->
      let* gqueue = Util.get_greeting_queue server service_id path in
      ConcurrentQueue.enqueue gqueue request;
      promise
  | `Established ->
      let session_id = service.spec.parse_session_id request.request_body in
      let* queue = Util.get_queue server service_id session_id role in
      ConcurrentQueue.enqueue queue.request_queue request;
      promise
  | `Invitation ->
      let session_id = service.spec.parse_session_id request.request_body in
      let session = new_session service session_id in
      let* iqueue = Util.get_invitation_queue server service_id path in
      ConcurrentQueue.enqueue iqueue (session.session_id, request);
      promise

let fresh_session_id () = SessionId.create (Int64.to_string (Random.bits64 ()))

let create_new_session_from_greeting ~service ~role request =
  let session_id = fresh_session_id () in
  let session = new_session service session_id in
  match Hashtbl.find_opt session.queues role with
  | Some queue ->
      ConcurrentQueue.enqueue queue.request_queue request;
      Ok (session_id, request)
  | None -> Error (Monad.mpst_error "accept_from_paths: impossible")

let accept_at_paths service pathspecs =
  let new_session_queue_from_path pathspec =
    match pathspec.path_kind with
    | `Greeting ->
        let* queue = Util.get_greeting_queue_ service pathspec.path in
        ConcurrentQueue.wrap queue
          (create_new_session_from_greeting ~service ~role:pathspec.path_role)
        |> return
    | `Invitation ->
        let* queue = Util.get_invitation_queue_ service pathspec.path in
        ConcurrentQueue.wrap queue (fun x -> Ok x) |> return
    | `Established ->
        error_with
          (Format.asprintf "Path %a is for established session" Path.pp
             pathspec.path)
  in
  let* wqs = mapM new_session_queue_from_path pathspecs in
  ConcurrentQueue.dequeue_one_of_wrapped wqs

let receive_at_paths session pathspecs =
  let get_queue pathspec =
    match pathspec.path_kind with
    | `Established ->
        let* queue = Util.get_queue_ session pathspec.path_role in
        return queue.request_queue
    | `Greeting -> Util.get_greeting_queue_ session.service_ref pathspec.path
    | `Invitation ->
        error_with
          (Format.asprintf "Path %a is for initial reception (invitation)"
             Path.pp pathspec.path)
  in
  let* queues = mapM get_queue pathspecs in
  ConcurrentQueue.dequeue_one_of queues

let kill_session_ session err =
  Hashtbl.remove session.service_ref.sessions session.session_id;
  session.queues
  |> Hashtbl.iter (fun _ queue ->
         ConcurrentQueue.kill queue.request_queue err;
         ConcurrentQueue.kill queue.response_queue err)

let kill_session service session_id err =
  match Hashtbl.find_opt service.sessions session_id with
  | None -> ()
  | Some session -> kill_session_ session err

(* let fresh_session_id () = SessionId.create (Int64.to_string (Random.bits64 ())) *)

(* let enqueue_greeting session role greeting =
   let* peer = Util.get_queue_ session role in
   ConcurrentQueue.enqueue peer.request_queue greeting.greeting_request;
   ConcurrentQueue.add_waiter peer.response_queue greeting.greeting_response;
   return () *)

(* let init_session service : session io =
   let queues =
     service.spec.greeting_paths
     |> List.map (fun path ->
            let spec = Hashtbl.find service.spec.path_specs path in
            Hashtbl.find service.greetings spec.path_role)
   in
   let* greeting = ConcurrentQueue.dequeue_one_of queues in
   match greeting with
   | Greeting greeting ->
       let role = greeting.greeting_request.request_role in
       let session_id = fresh_session_id () in
       let session = new_session service session_id in
       let* () = enqueue_greeting session role greeting in
       return session
   | GreetingWithId session_id ->
       Util.get_session service.server_ref service.spec.service_id session_id *)
