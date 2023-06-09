open! Kxclib

type payload = Json.jv

let failwith msg =
  Kxclib.Log0.error "Failure: %s" msg;
  failwith msg

let mpst_error msg =
  let open Prr in
  Jv.Error.v ~name:(Jstr.v "MPSTError") (Jstr.v msg)

module ServerIo : sig
  include Monadic with type 'x t = 'x Prr.Fut.or_error

  type error = Prr.Jv.Error.t

  val map : ('x -> 'y) -> 'x t -> 'y t
  val create_promise : unit -> 'a t * (('a, error) result -> unit)
  val error : error -> 'a t
  val error_with : string -> 'a t
  val handle_error : 'a t -> handler:(error -> 'a t) -> 'a t
end = struct
  open Prr

  type 'x t = 'x Fut.or_error

  let return x = Fut.ok x

  let bind (m : _ t) (af : _ -> _ t) =
    Fut.bind m @@ function Error e -> Fut.error e | Ok x -> af x

  type error = Jv.Error.t

  let map f m = Fut.map (function Error e -> Error e | Ok x -> Ok (f x)) m
  let create_promise = Fut.create
  let error err = Fut.error err

  let error_with msg =
    let err = mpst_error msg in
    error err

  let handle_error m ~handler =
    Fut.bind m (function Ok x -> return x | Error err -> handler err)
end

module ConversationId = Opstic_monadic.Id.Make ()
module SessionId = Opstic_monadic.Id.Make ()
module EntrypointId = Opstic_monadic.Id.Make ()
module Role = Opstic_monadic.Id.Make ()

type http_session_id = SessionId.t [@@deriving show]
type conversation_id = ConversationId.t [@@deriving show]
type entrypoint_id = EntrypointId.t [@@deriving show]
type role = Role.t [@@deriving show]
type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit

module Server : sig
  type 'a io = 'a ServerIo.t
  type t
  type http_request = payload
  type http_response = payload

  val create_server : unit -> t

  type protocol_kind = Leader | Follower [@@deriving eq, show]

  type protocol_spec = {
    entrypoint_id : entrypoint_id;
    kind : protocol_kind;
    my_role : role;
    other_roles : role list;
    initial_roles : role list;
    joining_roles : role list;
    joining_correlation_roles : role list;
  }
  [@@deriving show]

  val make_spec :
    entrypoint_id:string ->
    kind:protocol_kind ->
    my_role:string ->
    other_roles:string list ->
    initial_roles:string list ->
    ?joining_roles:string list ->
    ?joining_correlation_roles:string list ->
    unit ->
    protocol_spec

  val register_entrypoint : t -> spec:protocol_spec -> unit

  type entrypoint_kind =
    | StartLeader of role
    | StartFollower of role * conversation_id
    | Join of role
    | JoinCorrelation of role * conversation_id
    | InSession of http_session_id
  [@@deriving show]

  val handle_entry :
    t ->
    entrypoint_id:entrypoint_id ->
    entrypoint_kind:entrypoint_kind ->
    request:http_request ->
    http_response io

  val accept_initial :
    t ->
    entrypoint_id:entrypoint_id ->
    kind:[ `AsFollower | `AsLeader ] ->
    roles:Role.t list ->
    (conversation_id * role * http_session_id) io

  val accept_join :
    t ->
    entrypoint_id:entrypoint_id ->
    role:role ->
    conversation_id:conversation_id ->
    kind:[ `Correlation | `Fresh ] ->
    http_request io

  val receive_from_client :
    t -> http_session_id:http_session_id -> http_request io

  val send_to_client :
    t -> http_session_id:http_session_id -> http_response -> unit io
end = struct
  type 'a io = 'a ServerIo.t
  type http_request = payload
  type http_response = payload

  let return = ServerIo.return
  let error_with = ServerIo.error_with
  let ( let* ) = ServerIo.bind
  let handle_error = ServerIo.handle_error

  let hash_find ~descr h k =
    match Hashtbl.find_opt h k with
    | Some x -> return x
    | None -> error_with descr

  let option_get ~descr opt =
    match opt with Some x -> return x | None -> error_with descr

  type reqid = int

  type request_queue =
    [ `EmptyNoWait
    | `EmptyWaiting of http_request waiting
    | `Queued of (reqid * http_request) queue (* |q| > 0 *) ]

  type response_queue =
    [ `EmptyNoWait
    | `EmptyWaiting of (reqid * http_response waiting) queue (* |q| > 0 *)
    | `Queued of http_response queue (* |q| > 0 *) ]

  type http_session = {
    http_session_id : http_session_id;
    request_queue : request_queue;
    response_queue : response_queue;
    request_count : int;
    conversation_id : conversation_id;
    role : role;
  }
  [@@warning "-69"]

  type 'ident new_connection = {
    incoming : 'ident;
    http_request : http_request;
    http_response_waiting : http_response waiting;
  }

  type 'ident connection_queue0 =
    [ `EmptyNoWait
    | `EmptyWaiting of 'ident new_connection waiting queue (* |q| > 0 *)
    | `Queued of 'ident new_connection queue (* |q| > 0 *) ]

  type incoming = {
    incoming_role : role;
    incoming_conversation_id : conversation_id option;
  }

  type initial_queue = incoming connection_queue0
  type join_queue = unit connection_queue0

  type join_queues = {
    mutable join_fresh : join_queue option;
    join_correlation : (conversation_id, join_queue) Hashtbl.t option;
  }

  type t = {
    initial_queues : (entrypoint_id, initial_queue) Hashtbl.t;
    join_queues : (entrypoint_id * role, join_queues) Hashtbl.t;
    established_sessions : (http_session_id, http_session) Hashtbl.t;
  }

  let create_server () : t =
    {
      initial_queues = Hashtbl.create 42;
      join_queues = Hashtbl.create 42;
      established_sessions = Hashtbl.create 42;
    }

  type protocol_kind = Leader | Follower [@@deriving eq, show]

  type protocol_spec = {
    entrypoint_id : EntrypointId.t;
    kind : protocol_kind;
    my_role : Role.t;
    other_roles : Role.t list;
    initial_roles : Role.t list;
    joining_roles : Role.t list;
    joining_correlation_roles : Role.t list;
  }
  [@@deriving show]

  let make_spec ~entrypoint_id ~kind ~my_role ~other_roles ~initial_roles
      ?(joining_roles = []) ?(joining_correlation_roles = []) () =
    {
      entrypoint_id = EntrypointId.create entrypoint_id;
      kind;
      my_role = Role.create my_role;
      other_roles = List.map Role.create other_roles;
      initial_roles = List.map Role.create initial_roles;
      joining_roles = List.map Role.create joining_roles;
      joining_correlation_roles = List.map Role.create joining_correlation_roles;
    }

  let register_entrypoint (server : t) ~spec =
    let register role =
      let join_fresh =
        if List.mem role spec.joining_roles then Some `EmptyNoWait else None
      in
      let join_correlation =
        if List.mem role spec.joining_correlation_roles then
          Some (Hashtbl.create 42)
        else None
      in
      let join_queues = { join_fresh; join_correlation } in
      Hashtbl.replace server.join_queues (spec.entrypoint_id, role) join_queues
    in
    List.iter register spec.other_roles;
    Hashtbl.replace server.initial_queues spec.entrypoint_id `EmptyNoWait

  type entrypoint_kind =
    | StartLeader of role
    | StartFollower of role * conversation_id
    | Join of role
    | JoinCorrelation of role * conversation_id
    | InSession of http_session_id
  [@@deriving show]

  let _update_initial_queue server ~entrypoint_id f =
    let* queue =
      hash_find server.initial_queues entrypoint_id
        ~descr:("no such initial entrypoint:" ^ EntrypointId.show entrypoint_id)
    in
    f queue ~update:(fun queue ->
        Hashtbl.replace server.initial_queues entrypoint_id queue)

  let _get_join_queues server ~entrypoint_id ~role =
    hash_find server.join_queues (entrypoint_id, role)
      ~descr:
        (Format.asprintf "No joinable queue for entypoint_id: %a and role: %a"
           EntrypointId.pp entrypoint_id Role.pp role)

  let _update_fresh_join_queue server ~entrypoint_id ~role f =
    let* join_queues = _get_join_queues server ~entrypoint_id ~role in
    let* queue =
      option_get join_queues.join_fresh
        ~descr:
          (Format.asprintf
             "No joinable endpoint for entypoint_id: %a and role: %a"
             EntrypointId.pp entrypoint_id Role.pp role)
    in
    f queue ~update:(fun queue -> join_queues.join_fresh <- Some queue)

  let _update_correlation_join_queue server ~entrypoint_id ~role
      ~conversation_id f =
    let* join_queues = _get_join_queues server ~entrypoint_id ~role in
    let* hash =
      option_get join_queues.join_correlation
        ~descr:
          (Format.asprintf
             "No correlation-joinable endpoint for entypoint_id: %a and role: \
              %a"
             EntrypointId.pp entrypoint_id Role.pp role)
    in
    let update newq = Hashtbl.replace hash conversation_id newq in
    match Hashtbl.find_opt hash conversation_id with
    | Some queue -> f queue ~update
    | None -> f `EmptyNoWait ~update

  module Core = struct
    let handle_established_sessions server ~http_session_id
        ~(request : http_request) : http_response io =
      let* session =
        hash_find
          ~descr:("no such session:" ^ SessionId.show http_session_id)
          server.established_sessions http_session_id
      in
      let request_id = session.request_count in
      let inq =
        (* Handle the request by either (1) enqueueing it or (2) delivering directly to the (MPST) process *)
        match session.request_queue with
        | `EmptyNoWait | `Queued _ ->
            (* (1) The MPST process is not waiting for this http session yet. Prepare the incoming queue ... *)
            let q =
              match session.request_queue with
              | `Queued q -> q
              | _ -> Queue.empty
            in
            (* and enqueue the request in it *)
            `Queued (Queue.push (request_id, request) q)
        | `EmptyWaiting resolv_f ->
            (* (2) The MPST process is waiting. Deliver it directly *)
            resolv_f (Ok request);
            `EmptyNoWait
      in
      let promise, outq =
        (* Check the response queue and (1) make a promise if empty or (2) send back the message directly if some   *)
        match session.response_queue with
        | `EmptyNoWait | `EmptyWaiting _ ->
            (* (1) The queue is empty. Prepare the queue for `resolve` functions *)
            let q =
              match session.response_queue with
              | `EmptyWaiting q -> q
              | _ -> Queue.empty
            in
            (* making a promise for response, and *)
            let promise, resolv_f = ServerIo.create_promise () in
            (* update the queue state by enqueuing the resolve function *)
            (promise, `EmptyWaiting (Queue.push (request_id, resolv_f) q))
        | `Queued q ->
            (* (2) A message is available in the queue *)
            let retmsg, q =
              match Queue.pop q with
              | Some p -> p
              | None -> assert false (* |q| > 0 *)
            in
            (* make a resolved promsie *)
            (* and update the queue state *)
            let outq = if Queue.is_empty q then `EmptyNoWait else `Queued q in
            (return retmsg, outq)
      in
      Hashtbl.replace server.established_sessions http_session_id
        {
          session with
          request_queue = inq;
          response_queue = outq;
          request_count = request_id + 1;
        };
      promise

    let handle_pending_connection (type a) ~(incoming : a)
        ~(queue : a connection_queue0) ~(request : http_request) ~update :
        http_response io =
      let promise, resp_resolv_f = ServerIo.create_promise () in
      let newconn =
        {
          incoming;
          http_request = request;
          http_response_waiting = resp_resolv_f;
        }
      in
      match queue with
      | `EmptyNoWait | `Queued _ ->
          let q = match queue with `Queued q -> q | _ -> Queue.empty in
          let q = Queue.push newconn q in
          update (`Queued q);
          promise
      | `EmptyWaiting q ->
          let conn_resolv_f, q =
            match Queue.pop q with
            | Some x -> x
            | None -> assert false (* |q|>0 *)
          in
          let queue : a connection_queue0 =
            if Queue.is_empty q then `EmptyNoWait else `EmptyWaiting q
          in
          update queue;
          conn_resolv_f (Ok newconn);
          promise

    let handle_entry server ~entrypoint_id ~entrypoint_kind ~request =
      match entrypoint_kind with
      | InSession http_session_id ->
          handle_established_sessions server ~http_session_id ~request
      | StartLeader role ->
          _update_initial_queue server ~entrypoint_id (fun queue ->
              handle_pending_connection
                ~incoming:
                  { incoming_role = role; incoming_conversation_id = None }
                ~queue ~request)
      | StartFollower (role, conversation_id) ->
          _update_initial_queue server ~entrypoint_id (fun queue ~update ->
              handle_pending_connection
                ~incoming:
                  {
                    incoming_role = role;
                    incoming_conversation_id = Some conversation_id;
                  }
                ~queue ~request ~update)
      | Join role ->
          _update_fresh_join_queue server ~entrypoint_id ~role
          @@ fun queue ~update ->
          handle_pending_connection ~incoming:() ~queue ~request ~update
      | JoinCorrelation (role, conversation_id) ->
          _update_correlation_join_queue server ~entrypoint_id ~role
            ~conversation_id
          @@ fun queue ~update ->
          handle_pending_connection ~incoming:() ~queue ~request ~update
  end

  let handle_entry = Core.handle_entry

  let _create_http_session ~conversation_id ~role ~(request_count : int)
      ?(request_queue = `EmptyNoWait) ?(response_queue = `EmptyNoWait) () =
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
      _create_http_session ~conversation_id ~role
        ~request_queue:
          (`Queued (Queue.push (0, new_conn.http_request) Queue.empty))
        ~response_queue:
          (`EmptyWaiting
            (Queue.push (0, new_conn.http_response_waiting) Queue.empty))
        ~request_count:1 ()
    in
    Hashtbl.replace server.established_sessions newsession.http_session_id
      newsession;
    (conversation_id, role, newsession.http_session_id)

  let _mpst_accept (type a) (queue : a connection_queue0) :
      a new_connection io * a connection_queue0 =
    match queue with
    | `EmptyNoWait | `EmptyWaiting _ ->
        let q = match queue with `EmptyWaiting q -> q | _ -> Queue.empty in
        let promise, resolv_f = ServerIo.create_promise () in
        let queue = `EmptyWaiting (Queue.push resolv_f q) in
        (promise, queue)
    | `Queued q ->
        let new_conn, q =
          match Queue.pop q with
          | Some x -> x
          | None -> assert false (* |q|>0 *)
        in
        let queue = if Queue.is_empty q then `EmptyNoWait else `Queued q in
        (return new_conn, queue)

  let rec accept_initial server ~entrypoint_id
      ~(kind : [ `AsLeader | `AsFollower ]) ~roles :
      (conversation_id * role * http_session_id) io =
    _update_initial_queue server ~entrypoint_id @@ fun queue ~update ->
    let promise, queue = _mpst_accept queue in
    update queue;
    let* newconn = promise in
    handle_error ~handler:(fun err ->
        (* wait for next connection if error *)
        newconn.http_response_waiting (Error err);
        accept_initial server ~entrypoint_id ~kind ~roles (* XXX stack? *))
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
        _update_fresh_join_queue server ~entrypoint_id ~role
        @@ fun queue ~update ->
        let promise, queue = _mpst_accept queue in
        update queue;
        let* newconn = promise in
        retry_if_error newconn @@ fun () ->
        let conversation_id', role', _http_session_id =
          _establish_new_session server ~conversation_id ~role newconn
        in
        assert (conversation_id = conversation_id');
        assert (role = role');
        return newconn.http_request
    | `Correlation ->
        _update_correlation_join_queue server ~entrypoint_id ~role
          ~conversation_id
        @@ fun queue ~update ->
        let promise, queue = _mpst_accept queue in
        update queue;
        let* newconn = promise in
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
    let promise, inq =
      match session.request_queue with
      | `EmptyNoWait ->
          (* No client is waiting;
             make a promise, enqueuing its resolver in the queue,
             and return the promise *)
          let promise, resolv_f = ServerIo.create_promise () in
          (promise, `EmptyWaiting resolv_f)
      | `Queued q ->
          (* An HTTP request is in the queue; *)
          let (_reqid, req), q =
            match Queue.pop q with
            | None -> assert false (* |q| > 0 *)
            | Some (e, q) -> (e, q)
          in
          (* return it and update the queue state accordingly *)
          let inq = if Queue.is_empty q then `EmptyNoWait else `Queued q in
          (return req, inq)
      | `EmptyWaiting _ as inq ->
          ( error_with
              "receive_from_client: impossible: process is already waiting. \
               Possible linearity checking failure?",
            inq )
    in
    Hashtbl.replace server.established_sessions http_session_id
      { session with request_queue = inq };
    promise

  let send_to_client server ~http_session_id (msg : http_response) : unit io =
    let* session =
      try return @@ Hashtbl.find server.established_sessions http_session_id
      with Not_found ->
        error_with
          (Format.asprintf "send_to_client: http session id not found: %a"
             SessionId.pp http_session_id)
    in
    let outq =
      match session.response_queue with
      | `EmptyNoWait -> `Queued (Queue.push msg Queue.empty)
      | `Queued q -> `Queued (Queue.push msg q)
      | `EmptyWaiting q ->
          (* The queue is empty and there is a client waiting response. Deliver it directly. *)
          let (_reqid, f), q =
            match Queue.pop q (* |q|>0 *) with
            | None -> assert false
            | Some x -> x
          in
          (* send back the response to the client *)
          f (Ok msg);
          if Queue.is_empty q then `EmptyNoWait else `EmptyWaiting q
    in
    Hashtbl.replace server.established_sessions http_session_id
      { session with response_queue = outq };
    return ()
end

let handle_request (server : Server.t) (request : Json.jv) : Json.jv ServerIo.t
    =
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
    match mode_str with
    | "start" -> return (Server.StartLeader role)
    | "start_follower" ->
        let+ conversation_id = conversation_id () in
        Server.StartFollower (role, conversation_id)
    | "join" -> return (Server.Join role)
    | "join_correlation" ->
        let+ conversation_id = conversation_id () in
        Server.JoinCorrelation (role, conversation_id)
    | "session" ->
        let+ http_session_id = http_session_id () in
        Server.InSession http_session_id
    | _ -> error_with (Format.asprintf "wrong mode: %s" mode_str)
  in
  Server.handle_entry server ~entrypoint_id ~entrypoint_kind ~request

type nonrec t = {
  server_ref : Server.t;
  protocol : Server.protocol_spec;
  conversation_id : conversation_id;
  self_role : role;
  role_http_session_id : (role, http_session_id option) Hashtbl.t;
}
[@@warning "-69"]

module ServerEndpoint :
  Opstic_monadic.Endpoint
    with type t = t
     and type 'x io = 'x ServerIo.t
     and type payload = payload = struct
  type 'x io = 'x ServerIo.t

  type nonrec t = t = {
    server_ref : Server.t;
    protocol : Server.protocol_spec;
    conversation_id : conversation_id;
    self_role : role;
    role_http_session_id : (role, http_session_id option) Hashtbl.t;
  }
  [@@warning "-69"]

  open ServerIo

  let ( let* ) = ServerIo.bind

  type nonrec payload = payload

  let get_session_id ~ctx t role =
    match Hashtbl.find t.role_http_session_id role with
    | Some s -> return s
    | None ->
        error_with
          (Format.asprintf "%s: Session is not established for role: %a" ctx
             Role.pp role)
    | exception Not_found ->
        error_with
          (Format.asprintf "%s: impossible: No such role: %a" ctx Role.pp role)

  let send t ~connection ~role ~label ~payload : unit io =
    assert (connection = Opstic.Connected);
    let role = Role.create role in
    let* http_session_id = get_session_id t role ~ctx:"send" in
    let msg : Json.jv = `obj [ ("label", `str label); ("payload", payload) ] in
    Server.send_to_client t.server_ref ~http_session_id msg

  let receive t ~(connection : Opstic.connection) ~role =
    let role = Role.create role in
    let* payload =
      match connection with
      | Connected ->
          let* http_session_id = get_session_id t role ~ctx:"receive" in
          Server.receive_from_client t.server_ref ~http_session_id
      | Join | JoinCorrelation ->
          let kind = if connection = Join then `Fresh else `Correlation in
          Server.accept_join t.server_ref
            ~entrypoint_id:t.protocol.entrypoint_id ~role
            ~conversation_id:t.conversation_id ~kind
    in
    match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
    | `obj [ ("label", `str label); ("payload", payload) ] ->
        return (label, payload)
    | _ -> error_with "bad payload"

  let close _ =
    (* TODO purge all sessions from server *)
    ()
end

module Mpst_js = struct
  open ServerIo

  let ( let* ) = ServerIo.bind

  module Mpst_js = Opstic_monadic.Make (ServerIo) (ServerEndpoint)

  let accept_start_session :
      (* FIXME bad API style *)
      Server.t ->
      spec:Server.protocol_spec ->
      witness:'wit ->
      ('wit -> ([> ] as 'b) Mpst_js.Comm.inp) ->
      'b ServerIo.t =
   fun server ~spec ~witness call ->
    let* conversation_id, role, http_session_id =
      Server.accept_initial server ~entrypoint_id:spec.entrypoint_id
        ~kind:`AsLeader ~roles:spec.other_roles
    in
    let role_http_session_id = Hashtbl.create 42 in
    spec.other_roles
    |> List.iter (fun role -> Hashtbl.replace role_http_session_id role None);
    Hashtbl.replace role_http_session_id role (Some http_session_id);
    let t =
      {
        server_ref = server;
        protocol = spec;
        conversation_id;
        self_role = spec.my_role;
        role_http_session_id;
      }
    in
    let inp = call witness in
    (* FIXME use Comm.receive instead *)
    let* payload = Server.receive_from_client server ~http_session_id in
    let* payload =
      match Jv.access [ `f "message" ] payload with
      | Some x -> return x
      | _ ->
          error_with
            (Format.asprintf "no message body in request: %a" Json.pp_lit
               payload)
    in
    match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
    | `obj [ ("label", `str label); ("payload", v) ] ->
        let (Choice c) = Hashtbl.find inp.inp_choices label in
        let v = c.choice_marshal v in
        return
          (c.choice_variant.make_var (*fun x -> `lab x*)
             ( v,
               {
                 ep_raw = t;
                 ep_witness = Opstic_monadic.Lin.create c.choice_next_wit;
               } ))
    | _ -> error_with (Format.asprintf "bad payload: %a" Json.pp_lit payload)

  include Mpst_js
end
