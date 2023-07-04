open! Types
open! ServerIo

let ( let* ) = ServerIo.bind

let hash_find ~descr h k =
  match Hashtbl.find_opt h k with
  | Some x -> return x
  | None -> error_with descr

type greeting = {
  greeting_conversation_id : ConversationId.t option;
  greeting_request : http_request;
  greeting_response : http_response waiting;
}

type greeting_queue = greeting ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = http_response ConcurrentQueue.t
type peer = { request_queue : request_queue; response_queue : response_queue }

type session = {
  conversation_id : ConversationId.t;
  peers : (Role.t, peer) Hashtbl.t;
  entrypoint_ref : entrypoint;
}

and entrypoint = {
  entrypoint_id : EntrypointId.t;
  my_role : Role.t;
  other_roles : Role.t list;
  greeting : (Role.t, greeting_queue) Hashtbl.t;
  established : (ConversationId.t, session) Hashtbl.t;
}

type t = { entrypoints : (EntrypointId.t, entrypoint) Hashtbl.t }

module Util = struct
  let get_entrypoint t entrypoint_id =
    hash_find t.entrypoints entrypoint_id
      ~descr:
        (Format.asprintf "No entry point: %a" EntrypointId.pp entrypoint_id)

  let get_greeting_queue_ entrypoint role =
    hash_find entrypoint.greeting role
      ~descr:
        (Format.asprintf "Entrypoint %a Role %a is not accepting peer"
           EntrypointId.pp entrypoint.entrypoint_id Role.pp role)

  let get_greeting_queue t entrypoint_id role =
    let* entrypoint = get_entrypoint t entrypoint_id in
    get_greeting_queue_ entrypoint role

  let get_session t entrypoint_id conversation_id =
    let* entrypoint = get_entrypoint t entrypoint_id in
    hash_find entrypoint.established conversation_id
      ~descr:
        (Format.asprintf "No session id %a for entrypoint %a" ConversationId.pp
           conversation_id EntrypointId.pp entrypoint_id)

  let get_peer_ session role =
    (* let* session = get_session t entrypoint_id conversation_id in *)
    hash_find session.peers role
      ~descr:
        (Format.asprintf "No role %a for conversation %a (entrypoint %a)"
           Role.pp role ConversationId.pp session.conversation_id
           EntrypointId.pp session.entrypoint_ref.entrypoint_id)

  let get_peer t entrypoint_id conversation_id role =
    let* session = get_session t entrypoint_id conversation_id in
    get_peer_ session role

  let dequeue_greeting entrypoint role =
    let* greeting_queue = get_greeting_queue_ entrypoint role in
    ConcurrentQueue.dequeue greeting_queue

  let enqueue_greeting session role greeting =
    let* peer = get_peer_ session role in
    ConcurrentQueue.enqueue peer.request_queue greeting.greeting_request;
    ConcurrentQueue.add_waiter peer.response_queue greeting.greeting_response;
    return ()
end

let create_server () : t = { entrypoints = Hashtbl.create 42 }

let register_entrypoint (server : t) ~id ~my_role ~other_roles =
  let ent =
    {
      entrypoint_id = id;
      my_role;
      other_roles;
      greeting = Hashtbl.create 42;
      established = Hashtbl.create 42;
    }
  in
  let register ent role =
    Hashtbl.replace ent.greeting role (ConcurrentQueue.create ())
  in
  List.iter (register ent) other_roles;
  Hashtbl.replace server.entrypoints id ent;
  ent

let handle_entry server ~entrypoint_id ~path ~role
    ~(kind :
       [ `Greeting
       | `GreetingWithId of conversation_id
       | `Established of conversation_id ]) (request : payload) :
    http_response io =
  let promise, resolv = ServerIo.create_promise () in
  let request =
    {
      request_path = path;
      request_body = request;
      request_onerror = (fun err -> resolv (Error err));
    }
  in
  match kind with
  | `Established conversation_id ->
      let* peer = Util.get_peer server entrypoint_id conversation_id role in
      ConcurrentQueue.enqueue peer.request_queue request;
      ConcurrentQueue.add_waiter peer.response_queue resolv;
      promise
  | `Greeting | `GreetingWithId _ ->
      let conversation_id_opt =
        match kind with
        | `Greeting -> None
        | `GreetingWithId x -> Some x
        | `Established _ -> assert false
      in
      let* queue = Util.get_greeting_queue server entrypoint_id role in
      ConcurrentQueue.enqueue queue
        {
          greeting_conversation_id = conversation_id_opt;
          greeting_request = request;
          greeting_response = resolv;
        };
      promise

let kill_session entrypoint conversation_id err =
  match Hashtbl.find_opt entrypoint.established conversation_id with
  | None -> ()
  | Some session ->
      Hashtbl.remove entrypoint.established conversation_id;
      session.peers
      |> Hashtbl.iter (fun _ peer ->
             ConcurrentQueue.kill peer.request_queue err;
             ConcurrentQueue.kill peer.response_queue err)

let create_session entrypoint conversation_id =
  let roles = entrypoint.other_roles in
  let peers = Hashtbl.create (List.length roles) in
  roles
  |> List.iter (fun role ->
         Hashtbl.replace peers role
           {
             request_queue = ConcurrentQueue.create ();
             response_queue = ConcurrentQueue.create ();
           });
  { conversation_id; peers; entrypoint_ref = entrypoint }

let new_session entrypoint conversation_id =
  let session = create_session entrypoint conversation_id in
  Hashtbl.replace entrypoint.established conversation_id session;
  session

let accept_greeting (kind : [ `Greeting | `GreetingWithId ]) entrypoint session
    role : unit io =
  if kind = `GreetingWithId then Kxclib.Log0.warn "";
  let* greeting = Util.dequeue_greeting entrypoint role in
  Util.enqueue_greeting session role greeting

let fresh_conversation_id () =
  ConversationId.create (Int64.to_string (Random.bits64 ()))

let check_conversation_id kind greeting =
  match (kind, greeting.greeting_conversation_id) with
  | `Greeting, None -> fresh_conversation_id ()
  | `Greeting, Some conversation_id ->
      Kxclib.Log0.error "Conversation id given";
      conversation_id
  | `GreetingWithId, Some conversation_id -> conversation_id
  | `GreetingWithId, None ->
      Kxclib.Log0.error "Conversation id not given";
      fresh_conversation_id ()

let new_session_from_greeting (kind : [ `Greeting | `GreetingWithId ])
    entrypoint role : session io =
  let* greeting = Util.dequeue_greeting entrypoint role in
  let conversation_id = check_conversation_id kind greeting in
  let session = new_session entrypoint conversation_id in
  let* () = Util.enqueue_greeting session role greeting in
  return session

(* module Mpst = struct
     let _create_http_session ~conversation_id ~role ~(request_count : int)
         ?(request_queue = ConcurrentQueue.create ())
         ?(response_queue = ConcurrentQueue.create ()) () =
       {
         conversation_id;
         role;
         request_queue;
         response_queue;
         request_count;
         http_session_id = SessionId.create (Int64.to_string @@ Random.bits64 ());
       }

     let _establish_new_session server ~conversation_id ~role new_conn =
       let newsession =
         let request_queue = ConcurrentQueue.create ()
         and response_queue = ConcurrentQueue.create () in
         ConcurrentQueue.enqueue request_queue new_conn.http_request;
         ConcurrentQueue.add_waiter response_queue new_conn.http_response_waiting;
         _create_http_session ~conversation_id ~role ~request_queue ~response_queue
           ~request_count:1 ()
       in
       Hashtbl.replace server.established_sessions newsession.http_session_id
         newsession;
       (conversation_id, role, newsession.http_session_id)

     let rec accept_initial server ~entrypoint_id
         ~(kind : [ `AsLeader | `AsFollower ]) ~roles :
         (conversation_id * role * http_session_id) io =
       let* queue = Util.get_initial_queue server ~entrypoint_id in
       let* newconn = ConcurrentQueue.dequeue queue in
       handle_error ~handler:(fun err ->
           (* wait for next connection if error *)
           newconn.http_response_waiting (Error err);
           accept_initial server ~entrypoint_id ~kind ~roles)
       @@
       let role = newconn.incoming.incoming_role in
       let* () =
         if List.mem role roles then return ()
         else error_with (Format.asprintf "Role %a is not accepted" Role.pp role)
       in
       match kind with
       | `AsLeader ->
           let new_conversation_id =
             ConversationId.create (Int64.to_string @@ Random.bits64 ())
           in
           return
           @@ _establish_new_session server ~conversation_id:new_conversation_id
                ~role:newconn.incoming.incoming_role newconn
       | `AsFollower ->
           let* conversation_id =
             option_get newconn.incoming.incoming_conversation_id
               ~descr:"No conversation_id given"
           in
           return
           @@ _establish_new_session server ~role:newconn.incoming.incoming_role
                ~conversation_id newconn

     let rec accept_join server ~entrypoint_id ~role ~conversation_id
         ~(kind : [ `Fresh | `Correlation ]) : http_request io =
       let retry_if_error newconn f =
         handle_error
           ~handler:(fun err ->
             (* wait for next connection if error *)
             newconn.http_response_waiting (Error err);
             accept_join server ~entrypoint_id ~role ~conversation_id ~kind)
           (f ())
       in
       match kind with
       | `Fresh ->
           let* queue = Util.get_fresh_join_queue server ~entrypoint_id ~role in
           let* newconn = ConcurrentQueue.dequeue queue in
           retry_if_error newconn @@ fun () ->
           let conversation_id', role', _http_session_id =
             _establish_new_session server ~conversation_id ~role newconn
           in
           assert (conversation_id = conversation_id');
           assert (role = role');
           return newconn.http_request
       | `Correlation ->
           let* queue =
             Util.get_correlation_join_queue server ~entrypoint_id ~role
               ~conversation_id
           in
           let* newconn = ConcurrentQueue.dequeue queue in
           retry_if_error newconn @@ fun () ->
           let conversation_id', role', _http_session_id =
             _establish_new_session server ~conversation_id ~role newconn
           in
           assert (conversation_id = conversation_id');
           assert (role = role');
           return newconn.http_request

     let receive_from_client server ~http_session_id : http_request io =
       let* session =
         hash_find server.established_sessions http_session_id
           ~descr:
             (Format.asprintf "receive_from_client: http session id not found: %a"
                SessionId.pp http_session_id)
       in
       ConcurrentQueue.dequeue session.request_queue

     let send_to_client server ~http_session_id (msg : http_response) : unit io =
       let* session =
         try return @@ Hashtbl.find server.established_sessions http_session_id
         with Not_found ->
           error_with
             (Format.asprintf "send_to_client: http session id not found: %a"
                SessionId.pp http_session_id)
       in
       ConcurrentQueue.enqueue session.response_queue msg;
       return ()
   end *)

(* open! Kxclib

   let handle_request (server : t) (request : Json.jv) : Json.jv ServerIo.t =
     let open ServerIo in
     let ( let* ) = ServerIo.bind in
     let ( let+ ) m f = ServerIo.map f m in
     let access fld msg =
       match Jv.access [ `f fld ] request with
       | Some (`str x) -> return x
       | _ -> error_with msg
     in
     let* entrypoint_id =
       access "entrypoint" "entrypoint not given" |> map EntrypointId.create
     in
     let* role = access "role" "role not given" |> map Role.create in
     let conversation_id () =
       access "conversation_id" "conversation_id not given"
       |> map ConversationId.create
     in
     let http_session_id () =
       access "session_id" "session_id not given" |> map SessionId.create
     in
     let* mode_str = access "mode" "mode not given" in
     let* entrypoint_kind =
       let open Handler in
       match mode_str with
       | "start" -> return (StartLeader role)
       | "start_follower" ->
           let+ conversation_id = conversation_id () in
           StartFollower (role, conversation_id)
       | "join" -> return (Join role)
       | "join_correlation" ->
           let+ conversation_id = conversation_id () in
           JoinCorrelation (role, conversation_id)
       | "session" ->
           let+ http_session_id = http_session_id () in
           InSession http_session_id
       | _ -> error_with (Format.asprintf "wrong mode: %s" mode_str)
     in
     Handler.handle_entry server ~entrypoint_id ~entrypoint_kind ~request *)
