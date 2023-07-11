open! Types
open! Monad

let ( let* ) = Monad.bind

let hash_find ~descr h k =
  match Hashtbl.find_opt h k with
  | Some x -> return x
  | None -> error_with descr

type path_spec = { path : string; path_kind : path_kind; path_role : Role.t }

type service_spec = {
  service_id : ServiceId.t;
  path_specs : (string, path_spec) Hashtbl.t;
  greeting_paths : string list;
  my_role : Role.t;
  other_roles : Role.t list;
  parse_session_id : payload -> SessionId.t;
}

type http_response = { response_role : Role.t; response_body : payload }

type http_request = {
  request_sessionid : session_id option;
  request_path : string;
  request_role : Role.t;
  request_body : payload;
  request_response_resolv : http_response waiting;
}

type greeting0 = {
  greeting_request : http_request;
  greeting_response : http_response waiting;
}

type greeting_queue = http_request ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = http_response ConcurrentQueue.t
type queue = { request_queue : request_queue; response_queue : response_queue }

type session = {
  session_id : session_id;
  queues : (Role.t, queue) Hashtbl.t;
  service_ref : service;
}

and service = {
  spec : service_spec;
  greetings : (Role.t, greeting_queue) Hashtbl.t;
  sessions : (SessionId.t, session) Hashtbl.t;
  server_ref : t;
}

and t = {
  services : (ServiceId.t, service) Hashtbl.t;
  service_path : (string, service_id) Hashtbl.t;
}

module Util = struct
  let get_service t service_id =
    hash_find t.services service_id
      ~descr:(Format.asprintf "No entry point: %a" ServiceId.pp service_id)

  let get_service_from_path t path =
    let* service_id =
      hash_find ~descr:(Format.asprintf "No path %s" path) t.service_path path
    in
    hash_find t.services service_id
      ~descr:(Format.asprintf "No entry point: %a" ServiceId.pp service_id)

  let get_greeting_queue_ service role =
    hash_find service.greetings role
      ~descr:
        (Format.asprintf "Service %a Role %a is not accepting peer" ServiceId.pp
           service.spec.service_id Role.pp role)

  let get_greeting_queue t service_id role =
    let* service = get_service t service_id in
    get_greeting_queue_ service role

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
      sessions = Hashtbl.create 42;
      server_ref = server;
    }
  in
  let register_role sv role =
    Hashtbl.replace sv.greetings role (ConcurrentQueue.create ())
  in
  spec.other_roles |> List.iter (register_role sv);
  let register_path path =
    Hashtbl.replace server.service_path path sv.spec.service_id
  in
  Hashtbl.to_seq_keys spec.path_specs |> Seq.iter register_path;
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
  let* service = Util.get_service_from_path server path in
  let path_spec = Hashtbl.find service.spec.path_specs path in
  let role = path_spec.path_role in
  let request =
    {
      request_sessionid = None;
      request_path = path;
      request_role = role;
      request_body = request;
      request_response_resolv = resolv;
    }
  in
  let service_id = service.spec.service_id in
  match path_spec.path_kind with
  | `Greeting ->
      let* gqueue = Util.get_greeting_queue server service_id role in
      ConcurrentQueue.enqueue gqueue request;
      promise
  | `Established ->
      let session_id = service.spec.parse_session_id request.request_body in
      let request = { request with request_sessionid = Some session_id } in
      let* queue = Util.get_queue server service_id session_id role in
      ConcurrentQueue.enqueue queue.request_queue request;
      promise
  | `GreetingWithId ->
      let session_id = service.spec.parse_session_id request.request_body in
      let request = { request with request_sessionid = Some session_id } in
      let (_ : session) = new_session service session_id in
      let* gqueue = Util.get_greeting_queue server service_id role in
      ConcurrentQueue.enqueue gqueue request;
      promise

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
