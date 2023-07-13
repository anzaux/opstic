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
  parse_session_id : payload -> session_id io;
}

type response = payload

type request = {
  request_pathspec : path_spec;
  request_body : payload;
  request_raw : express_req_type;
  request_resolv : response waiting;
}

type greeting_queue = request ConcurrentQueue.t
type request_queue = request ConcurrentQueue.t
type response_queue = response ConcurrentQueue.t
type queues = { request_queue : request_queue; response_queue : response_queue }

type session = {
  session_id : session_id;
  queues : (role, queues) Hashtbl.t;
  service_ref : service;
}

and invitation_queue = (session * request) ConcurrentQueue.t

and service = {
  spec : service_spec;
  greeting_queues : (path, greeting_queue) Hashtbl.t;
  invitation_queues : (path, invitation_queue) Hashtbl.t;
  sessions : (session_id, session) Hashtbl.t;
  server_ref : server;
}

and server = {
  services : (ServiceId.t, service) Hashtbl.t;
  service_path : (path, service_id) Hashtbl.t;
}

let greeting_queue_of_service service ~path =
  assert (path.path_kind = `Greeting);
  Hashtbl.find service.greeting_queues path.path

module Session = struct
  type t = session

  let id t = t.session_id
  let queues t role = Hashtbl.find t.queues role
  let service t = t.service_ref

  let kill session err =
    Hashtbl.remove session.service_ref.sessions session.session_id;
    session.queues
    |> Hashtbl.iter (fun _ queue ->
           ConcurrentQueue.kill queue.request_queue err;
           ConcurrentQueue.kill queue.response_queue err)

  let create service session_id =
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

  let wait_at_paths session pathspecs =
    let session_queue pathspec =
      match pathspec.path_kind with
      | `Established ->
          let queues = queues session pathspec.path_role in
          queues.request_queue
      | `Greeting ->
          assert (
            ConcurrentQueue.is_empty
              (queues session pathspec.path_role).request_queue);
          greeting_queue_of_service session.service_ref ~path:pathspec
      | `Invitation ->
          failwith
            (Format.asprintf "Path %a is for initial reception (invitation)"
               Path.pp pathspec.path)
    in
    let queues = List.map session_queue pathspecs in
    ConcurrentQueue.dequeue_one_of queues
end

module Service = struct
  type t = service

  let id t = t.spec.service_id
  let spec t = t.spec
  let greeting_queue = greeting_queue_of_service

  let invitation_queue t ~path =
    assert (path.path_kind = `Invitation);
    Hashtbl.find t.invitation_queues path.path

  let get_session t session_id =
    hash_find t.sessions session_id
      ~descr:
        (Format.asprintf "No session id %a for service %a" SessionId.pp
           session_id ServiceId.pp t.spec.service_id)

  let server t = t.server_ref

  let new_session service session_id =
    let session = Session.create service session_id in
    Hashtbl.replace service.sessions session_id session;
    session

  let fresh_session_id () =
    SessionId.create (Int64.to_string (Random.bits64 ()))

  let new_session_from_greeting ~service ~role request =
    let session_id = fresh_session_id () in
    let session = new_session service session_id in
    let queues = Session.queues session role in
    ConcurrentQueue.enqueue queues.request_queue request;
    Ok (session, request)

  let wait_at_paths service pathspecs =
    let wrap_queue pathspec =
      match pathspec.path_kind with
      | `Greeting ->
          greeting_queue service ~path:pathspec
          |> ConcurrentQueue.wrap
               ~wrapper:
                 (new_session_from_greeting ~service ~role:pathspec.path_role)
      | `Invitation ->
          invitation_queue service ~path:pathspec |> ConcurrentQueue.nowrap
      | `Established ->
          failwith
            (Format.asprintf "Path %a is for established session" Path.pp
               pathspec.path)
    in
    let wqs = List.map wrap_queue pathspecs in
    ConcurrentQueue.dequeue_one_of_wrapped wqs
end

module Server = struct
  type nonrec t = server

  let create () : t =
    { services = Hashtbl.create 42; service_path = Hashtbl.create 42 }

  let service t service_id = Hashtbl.find t.services service_id

  let get_service t ~path =
    let* service_id =
      hash_find
        ~descr:(Format.asprintf "No path %a" Path.pp path)
        t.service_path path
    in
    hash_find t.services service_id
      ~descr:(Format.asprintf "No entry point: %a" ServiceId.pp service_id)

  let register_service (server : t) ~spec =
    let sv =
      {
        spec;
        greeting_queues = Hashtbl.create 42;
        invitation_queues = Hashtbl.create 42;
        sessions = Hashtbl.create 42;
        server_ref = server;
      }
    in
    let register_path path =
      Hashtbl.replace server.service_path path.path sv.spec.service_id;
      match path.path_kind with
      | `Greeting ->
          Hashtbl.replace sv.greeting_queues path.path
            (ConcurrentQueue.create ())
      | `Invitation ->
          Hashtbl.replace sv.invitation_queues path.path
            (ConcurrentQueue.create ())
      | `Established -> ()
    in
    Hashtbl.to_seq_values spec.path_specs |> Seq.iter register_path;
    Hashtbl.replace server.services spec.service_id sv;
    ()

  let handle_request t ~path (request : payload) (req_raw : express_req_type) :
      payload io =
    let promise, resolv = Monad.create_promise () in
    let path = Path.create path in
    let* sv = get_service t ~path in
    let path_spec = Hashtbl.find sv.spec.path_specs path in
    let role = path_spec.path_role in
    let request =
      {
        request_pathspec = path_spec;
        request_body = request;
        request_raw = req_raw;
        request_resolv = resolv;
      }
    in
    match path_spec.path_kind with
    | `Greeting ->
        let gqueue = Service.greeting_queue sv ~path:path_spec in
        ConcurrentQueue.enqueue gqueue request;
        promise
    | `Established ->
        let* session_id = sv.spec.parse_session_id request.request_body in
        let* session = Service.get_session sv session_id in
        let queue = Session.queues session role in
        ConcurrentQueue.enqueue queue.request_queue request;
        promise
    | `Invitation ->
        let* session_id = sv.spec.parse_session_id request.request_body in
        let session = Service.new_session sv session_id in
        let iqueue = Service.invitation_queue sv ~path:path_spec in
        ConcurrentQueue.enqueue iqueue (session, request);
        promise
end
