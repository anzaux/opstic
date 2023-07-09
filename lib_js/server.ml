open! Types
open! ServerIo

type dyn = Dyn.t

let ( let* ) = ServerIo.bind

let hash_find ~descr h k =
  match Hashtbl.find_opt h k with
  | Some x -> return x
  | None -> error_with descr

type path_kind = [ `Greeting | `GreetingWithId | `Established ]

type path_spec = {
  path : string;
  path_kind : path_kind;
  path_role : Role.t;
  path_request_parse : payload -> (ConversationId.t * string * dyn) option;
  path_response_unparse : ConversationId.t * string * Dyn.t -> payload;
}

type entrypoint_spec = {
  entrypoint_id : EntrypointId.t;
  path_specs : (string, path_spec) Hashtbl.t;
  greeting_paths : string list;
  my_role : Role.t;
  other_roles : Role.t list;
}

type http_request = {
  request_path : string;
  request_role : Role.t;
  request_label : string;
  request_body : dyn;
  request_body_raw : payload;
  request_onerror : ServerIo.error -> unit;
}

type http_response = {
  response_role : Role.t;
  response_label : string;
  response_body : Dyn.t;
  response_body_raw : payload;
}

type greeting0 = {
  greeting_request : http_request;
  greeting_response : http_response waiting;
}

type greeting = GreetingWithId of ConversationId.t | Greeting of greeting0
type greeting_queue = greeting ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = http_response ConcurrentQueue.t

type queuepair = {
  request_queue : request_queue;
  response_queue : response_queue;
}

type session = {
  conversation_id : conversation_id;
  queues : (Role.t, queuepair) Hashtbl.t;
  entrypoint_ref : entrypoint;
}

and entrypoint = {
  spec : entrypoint_spec;
  greetings : (Role.t, greeting_queue) Hashtbl.t;
  sessions : (ConversationId.t, session) Hashtbl.t;
  server_ref : t;
}

and t = { entrypoints : (EntrypointId.t, entrypoint) Hashtbl.t }

module Util = struct
  let get_entrypoint t entrypoint_id =
    hash_find t.entrypoints entrypoint_id
      ~descr:
        (Format.asprintf "No entry point: %a" EntrypointId.pp entrypoint_id)

  let get_greeting_queue_ entrypoint role =
    hash_find entrypoint.greetings role
      ~descr:
        (Format.asprintf "Entrypoint %a Role %a is not accepting peer"
           EntrypointId.pp entrypoint.spec.entrypoint_id Role.pp role)

  let get_greeting_queue t entrypoint_id role =
    let* entrypoint = get_entrypoint t entrypoint_id in
    get_greeting_queue_ entrypoint role

  let get_session t entrypoint_id conversation_id =
    let* entrypoint = get_entrypoint t entrypoint_id in
    hash_find entrypoint.sessions conversation_id
      ~descr:
        (Format.asprintf "No session id %a for entrypoint %a" ConversationId.pp
           conversation_id EntrypointId.pp entrypoint_id)

  let get_queuepair_ session role =
    (* let* session = get_session t entrypoint_id conversation_id in *)
    hash_find session.queues role
      ~descr:
        (Format.asprintf "No role %a for conversation %a (entrypoint %a)"
           Role.pp role ConversationId.pp session.conversation_id
           EntrypointId.pp session.entrypoint_ref.spec.entrypoint_id)

  let get_queuepair t entrypoint_id conversation_id role =
    let* session = get_session t entrypoint_id conversation_id in
    get_queuepair_ session role

  (* let dequeue_greeting entrypoint role =
     let* greeting_queue = get_greeting_queue_ entrypoint role in
     ConcurrentQueue.dequeue greeting_queue *)
end

let create_server () : t = { entrypoints = Hashtbl.create 42 }

let register_entrypoint (server : t) ~spec =
  let ent =
    {
      spec;
      greetings = Hashtbl.create 42;
      sessions = Hashtbl.create 42;
      server_ref = server;
    }
  in
  let register ent role =
    Hashtbl.replace ent.greetings role (ConcurrentQueue.create ())
  in
  spec.other_roles |> List.iter (register ent);
  Hashtbl.replace server.entrypoints spec.entrypoint_id ent;
  ent

let create_session entrypoint conversation_id =
  let roles = entrypoint.spec.other_roles in
  let queues = Hashtbl.create (List.length roles) in
  roles
  |> List.iter (fun role ->
         Hashtbl.replace queues role
           {
             request_queue = ConcurrentQueue.create ();
             response_queue = ConcurrentQueue.create ();
           });
  { conversation_id; queues; entrypoint_ref = entrypoint }

let new_session entrypoint conversation_id =
  let session = create_session entrypoint conversation_id in
  Hashtbl.replace entrypoint.sessions conversation_id session;
  session

let handle_entry server ~entrypoint_id ~path (request : payload) : payload io =
  let promise, resolv = ServerIo.create_promise () in
  let resolv = function
    | Ok response -> resolv (Ok response.response_body_raw)
    | Error err -> resolv (Error err)
  in
  let* entrypoint = Util.get_entrypoint server entrypoint_id in
  let path_spec = Hashtbl.find entrypoint.spec.path_specs path in
  let conversation_id, label, payload =
    match path_spec.path_request_parse request with
    | Some (conversation_id, label, payload) -> (conversation_id, label, payload)
    | None -> assert false
  in
  let role = path_spec.path_role in
  let request =
    {
      request_path = path;
      request_role = role;
      request_label = label;
      request_body = payload;
      request_body_raw = request;
      request_onerror = (fun err -> resolv (Error err));
    }
  in
  match path_spec.path_kind with
  | `Established ->
      let* peer =
        Util.get_queuepair server entrypoint_id conversation_id role
      in
      ConcurrentQueue.enqueue peer.request_queue request;
      ConcurrentQueue.add_waiter peer.response_queue resolv;
      promise
  | `Greeting ->
      let* queue = Util.get_greeting_queue server entrypoint_id role in
      ConcurrentQueue.enqueue queue
        (Greeting { greeting_request = request; greeting_response = resolv });
      promise
  | `GreetingWithId ->
      let* () =
        let (_ : session) = new_session entrypoint conversation_id in
        return ()
      in
      let* () =
        let* peer =
          Util.get_queuepair server entrypoint_id conversation_id role
        in
        ConcurrentQueue.enqueue peer.request_queue request;
        ConcurrentQueue.add_waiter peer.response_queue resolv;
        return ()
      in
      let* () =
        let* queue = Util.get_greeting_queue server entrypoint_id role in
        ConcurrentQueue.enqueue queue (GreetingWithId conversation_id);
        return ()
      in
      promise

let kill_session_ session err =
  Hashtbl.remove session.entrypoint_ref.sessions session.conversation_id;
  session.queues
  |> Hashtbl.iter (fun _ peer ->
         ConcurrentQueue.kill peer.request_queue err;
         ConcurrentQueue.kill peer.response_queue err)

let kill_session entrypoint conversation_id err =
  match Hashtbl.find_opt entrypoint.sessions conversation_id with
  | None -> ()
  | Some session -> kill_session_ session err

let fresh_conversation_id () =
  ConversationId.create (Int64.to_string (Random.bits64 ()))

let enqueue_greeting session role greeting =
  let* peer = Util.get_queuepair_ session role in
  ConcurrentQueue.enqueue peer.request_queue greeting.greeting_request;
  ConcurrentQueue.add_waiter peer.response_queue greeting.greeting_response;
  return ()

let init_session entrypoint : session io =
  let queues =
    entrypoint.spec.greeting_paths
    |> List.map (fun path ->
           let spec = Hashtbl.find entrypoint.spec.path_specs path in
           Hashtbl.find entrypoint.greetings spec.path_role)
  in
  let* greeting = ConcurrentQueue.dequeue_one_of queues in
  match greeting with
  | Greeting greeting ->
      let role = greeting.greeting_request.request_role in
      let conversation_id = fresh_conversation_id () in
      let session = new_session entrypoint conversation_id in
      let* () = enqueue_greeting session role greeting in
      return session
  | GreetingWithId conversation_id ->
      Util.get_session entrypoint.server_ref entrypoint.spec.entrypoint_id
        conversation_id
