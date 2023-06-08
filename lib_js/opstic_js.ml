open! Kxclib

type payload = Json.jv

module ServerIo : Monadic with type 'x t = 'x Prr.Fut.or_error = struct
  type 'x t = 'x Prr.Fut.or_error

  let return x = Prr.Fut.ok x

  let bind (m : _ t) (af : _ -> _ t) =
    Prr.Fut.bind m @@ function Error e -> Prr.Fut.error e | Ok x -> af x
end

module ConversationId = Opstic.Id.Make ()
module SessionId = Opstic.Id.Make ()
module EntrypointId = Opstic.Id.Make ()
module Role = Opstic.Id.Make ()

type http_session_id = SessionId.t
type conversation_id = ConversationId.t
type entrypoint_id = EntrypointId.t
type role = Role.t
type http_request = payload
type http_response = payload
type 'a ok_or_error = ('a, Prr.Jv.Error.t) result
type 'a waiting = 'a ok_or_error -> unit

module Server
    (* : sig
         type t
         type http_session
         type 'a io = 'a ServerIo.t

         type entrypoint_kind =
           | StartLeader
           | StartFollower of conversation_id
           | Join
           | JoinCorrelation of conversation_id
           | InSession of http_session_id

         type request_kind =
           | AcceptAsLeader
           | AcceptAsFollower
           | AcceptJoin of conversation_id
           | AcceptJoinCorrelation of conversation_id

         val receive_from_client :
           t -> http_session_id:http_session_id -> http_request io

         val send_to_client :
           t -> http_session_id:http_session_id -> http_response -> unit

         val accept_conversation :
           t ->
           entrypoint_id:entrypoint_id ->
           role:role ->
           accept_kind:request_kind ->
           http_session io

         val create_server : unit -> t

         val handle_entry :
           t ->
           entrypoint_id:entrypoint_id ->
           role:role ->
           entrypoint_kind:entrypoint_kind ->
           request:http_response ->
           http_response io

         type protocol_kind = Leader | Follower

         type protocol_spec = {
           entrypoint_id : EntrypointId.t;
           kind : protocol_kind;
           my_role : Role.t list;
           other_roles : Role.t list;
           initial_roles : Role.t list;
           joining_roles : Role.t list;
           joining_correlation_roles : Role.t list;
         }

         val register_entrypoint : t -> spec:protocol_spec -> unit
       end *) =
struct
  type 'a io = 'a ServerIo.t

  let return = ServerIo.return
  let ( >>= ) = ServerIo.bind

  type 'ident new_connection = {
    identifiers : 'ident;
    http_request : http_request;
    http_response_waiting : http_response waiting;
  }

  type 'ident entrypoint_queue0 =
    [ `EmptyNoWait
    | `EmptyWaiting of 'ident new_connection waiting queue (* |q| > 0 *)
    | `Queued of 'ident new_connection queue (* |q| > 0 *) ]

  type initial_entrypoint_queue =
    (role * conversation_id option) entrypoint_queue0

  type join_entrypoint_queue = unit entrypoint_queue0
  type join_correlation_entrypoint_queue = conversation_id entrypoint_queue0

  type join_entrypoint = {
    mutable join : join_entrypoint_queue option;
    join_correlation :
      (conversation_id, join_correlation_entrypoint_queue) Hashtbl.t option;
  }

  type entrypoint_kind =
    | StartLeader of role
    | StartFollower of role * conversation_id
    | Join of role
    | JoinCorrelation of role * conversation_id
    | InSession of http_session_id

  type reqid = int

  type request_queue =
    [ `EmptyNoWait
    | `EmptyWaiting of http_request waiting
    | `Queued of (reqid * http_request) queue (* |q| > 0 *) ]

  type response_queue =
    [ `EmptyNoWait
    | `EmptyWaiting of (reqid * http_response waiting) queue (* |q| > 0 *)
    | `Queued of http_response queue ]

  type http_session = {
    http_session_id : http_session_id;
    conversation_id : conversation_id;
    role : role;
    request_queue : request_queue;
    response_queue : response_queue;
    request_count : int;
  }

  type t = {
    mutable initial_endtrypoint :
      (entrypoint_id, initial_entrypoint_queue) Hashtbl.t;
    join_entrypoints : (entrypoint_id * role, join_entrypoint) Hashtbl.t;
    established_sessions : (http_session_id, http_session) Hashtbl.t;
  }

  type request_kind =
    | AcceptAsLeader
    | AcceptAsFollower
    | AcceptJoin of conversation_id
    | AcceptJoinCorrelation of conversation_id

  let create_server () : t =
    {
      initial_endtrypoint = Hashtbl.create 42;
      join_entrypoints = Hashtbl.create 42;
      established_sessions = Hashtbl.create 42;
    }

  type protocol_kind = Leader | Follower

  type protocol_spec = {
    entrypoint_id : EntrypointId.t;
    kind : protocol_kind;
    my_role : Role.t;
    other_roles : Role.t list;
    initial_roles : Role.t list;
    joining_roles : Role.t list;
    joining_correlation_roles : Role.t list;
  }

  let register_entrypoint (server : t) ~spec =
    let register role =
      let join =
        if List.mem role spec.joining_roles then Some `EmptyNoWait else None
      in
      let join_correlation =
        if List.mem role spec.joining_correlation_roles then
          Some (Hashtbl.create 42)
        else None
      in
      let join_entrypoints = { join; join_correlation } in
      Hashtbl.replace server.join_entrypoints (spec.entrypoint_id, role)
        join_entrypoints
    in
    List.iter register spec.other_roles;
    Hashtbl.replace server.initial_endtrypoint spec.entrypoint_id `EmptyNoWait

  let create_http_session ~conversation_id ~role ~(request_count : int)
      ?(request_queue = `EmptyNoWait) ?(response_queue = `EmptyNoWait) () =
    {
      conversation_id;
      role;
      request_queue;
      response_queue;
      request_count;
      http_session_id = SessionId.create (Int64.to_string @@ Random.bits64 ());
    }

  let _handle_pending_request (type a) ~(identifiers : a)
      ~(queue : a entrypoint_queue0) ~(request : http_request) :
      http_response io * a entrypoint_queue0 =
    match queue with
    | `EmptyNoWait | `Queued _ ->
        let q = match queue with `Queued q -> q | _ -> Queue.empty in
        let promise, resolv_f = Prr.Fut.create () in
        let q =
          Queue.push
            {
              identifiers;
              http_request = request;
              http_response_waiting = resolv_f;
            }
            q
        in
        (promise, `Queued q)
    | `EmptyWaiting q ->
        let resolv_f, q =
          match Queue.pop q with
          | Some x -> x
          | None -> assert false (* |q|>0 *)
        in
        let promise, resp_resolv_f = Prr.Fut.create () in
        resolv_f
          (Ok
             {
               identifiers;
               http_request = request;
               http_response_waiting = resp_resolv_f;
             });
        let queue : a entrypoint_queue0 =
          if Queue.is_empty q then `EmptyNoWait else `EmptyWaiting q
        in
        (promise, queue)

  let handle_established_sessions server ~http_session_id
      ~(request : http_request) : http_response io =
    let session =
      try Hashtbl.find server.established_sessions http_session_id
      with Not_found -> failwith "TODO: no such session"
    in
    let request_id = session.request_count in
    let inq =
      (* Handle the request by either (1) enqueueing it or (2) delivering directly to the (MPST) process *)
      match session.request_queue with
      | `EmptyNoWait | `Queued _ ->
          (* (1) The MPST process is not waiting. Prepare the incoming queue ... *)
          let q =
            match session.request_queue with `Queued q -> q | _ -> Queue.empty
          in
          (* and enqueue the request in it *)
          `Queued (Queue.push (request_id, request) q)
      | `EmptyWaiting f ->
          (* (2) The MPST process is waiting. Deliver it directly *)
          f (Ok request);
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
          let promise, resolv_f = Prr.Fut.create () in
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

  let handle_entry server ~entrypoint_id ~entrypoint_kind ~request =
    let option_get opt txt =
      match opt with Some x -> x | None -> failwith ("handle_entry: " ^ txt)
    in
    match entrypoint_kind with
    | InSession http_session_id ->
        handle_established_sessions server ~http_session_id ~request
    | StartLeader role ->
        let queue = Hashtbl.find server.initial_endtrypoint entrypoint_id in
        let promise, queue =
          _handle_pending_request ~identifiers:(role, None) ~queue ~request
        in
        Hashtbl.replace server.initial_endtrypoint entrypoint_id queue;
        promise
    | StartFollower (role, conversation_id) ->
        let queue = Hashtbl.find server.initial_endtrypoint entrypoint_id in
        let promise, queue =
          _handle_pending_request
            ~identifiers:(role, Some conversation_id)
            ~queue ~request
        in
        Hashtbl.replace server.initial_endtrypoint entrypoint_id queue;
        promise
    | Join role ->
        let entrypoint =
          Hashtbl.find server.join_entrypoints (entrypoint_id, role)
        in
        let queue = option_get entrypoint.join "no join queue" in
        let promise, queue =
          _handle_pending_request ~identifiers:() ~queue ~request
        in
        entrypoint.join <- Some queue;
        promise
    | JoinCorrelation (role, conversation_id) ->
        let entrypoint =
          Hashtbl.find server.join_entrypoints (entrypoint_id, role)
        in
        let t =
          option_get entrypoint.join_correlation "no join_correlation queue"
        in
        let queue =
          match Hashtbl.find_opt t conversation_id with
          | Some queue -> queue
          | None -> `EmptyNoWait
        in
        let promise, queue =
          _handle_pending_request ~identifiers:conversation_id ~queue ~request
        in
        Hashtbl.replace t conversation_id queue;
        promise
    | exception Not_found -> failwith "server: entrypoint not declared"

  let _establish_new_session server ~conversation_id ~role new_conn =
    let newsession =
      create_http_session ~conversation_id ~role
        ~request_queue:
          (`Queued (Queue.push (0, new_conn.http_request) Queue.empty))
        ~response_queue:
          (`EmptyWaiting
            (Queue.push (0, new_conn.http_response_waiting) Queue.empty))
        ~request_count:1 ()
    in
    Hashtbl.replace server.established_sessions newsession.http_session_id
      newsession;
    return (conversation_id, role, newsession.http_session_id)

  let _mpst_accept queue =
    match queue with
    | `EmptyNoWait | `EmptyWaiting _ ->
        let q = match queue with `EmptyWaiting q -> q | _ -> Queue.empty in
        let promise, resolv_f = Prr.Fut.create () in
        let queue = `EmptyWaiting (Queue.push resolv_f q) in
        (promise, queue)
    | `Queued q ->
        let new_conn, q =
          match Queue.pop q with Some x -> x | None -> assert false
        in
        let queue = if Queue.is_empty q then `EmptyNoWait else `Queued q in
        (return new_conn, queue)

  let accept_initial server ~entrypoint_id ~(kind : [ `AsLeader | `AsFollower ])
      : (conversation_id * role * http_session_id) io =
    (* let get_opt x msg = match x with Some x -> x | None -> failwith msg in *)
    match kind with
    | `AsLeader ->
        let queue = Hashtbl.find server.initial_endtrypoint entrypoint_id in
        let promise, queue = _mpst_accept queue in
        Hashtbl.replace server.initial_endtrypoint entrypoint_id queue;
        promise >>= fun newconn ->
        let new_conversation_id =
          ConversationId.create (Int64.to_string @@ Random.bits64 ())
        in
        _establish_new_session server ~conversation_id:new_conversation_id
          ~role:(fst newconn.identifiers) newconn
    | `AsFollower ->
        let queue = Hashtbl.find server.initial_endtrypoint entrypoint_id in
        let promise, queue = _mpst_accept queue in
        Hashtbl.replace server.initial_endtrypoint entrypoint_id queue;
        promise >>= fun newconn ->
        let conversation_id =
          match snd newconn.identifiers with
          | Some x -> x
          | None -> failwith "no conversation_id given"
        in
        _establish_new_session server ~role:(fst newconn.identifiers)
          ~conversation_id newconn

  let accept_join server ~entrypoint_id ~role ~conversation_id
      ~(kind : [ `Fresh | `Correlation ]) : http_request io =
    let get_opt x msg = match x with Some x -> x | None -> failwith msg in
    let entrypoint =
      Hashtbl.find server.join_entrypoints (entrypoint_id, role)
    in
    match kind with
    | `Fresh ->
        let queue = get_opt entrypoint.join "no join entrypoint" in
        let promise, queue = _mpst_accept queue in
        entrypoint.join <- Some queue;
        promise >>= fun newconn ->
        _establish_new_session server ~conversation_id ~role newconn
        >>= fun (conversation_id', role', _http_session_id) ->
        assert (conversation_id = conversation_id');
        assert (role = role');
        return newconn.http_request
    | `Correlation ->
        let tbl =
          get_opt entrypoint.join_correlation "no join_correlation entrypoint"
        in
        let queue =
          try Hashtbl.find tbl conversation_id with Not_found -> `EmptyNoWait
        in
        let promise, queue = _mpst_accept queue in
        Hashtbl.replace tbl conversation_id queue;
        promise >>= fun newconn ->
        assert (newconn.identifiers = conversation_id);
        _establish_new_session server ~conversation_id ~role newconn
        >>= fun (conversation_id', role', _http_session_id) ->
        assert (conversation_id = conversation_id');
        assert (role = role');
        return newconn.http_request

  let receive_from_client server ~http_session_id : http_request io =
    let session =
      try Hashtbl.find server.established_sessions http_session_id
      with Not_found -> failwith "TODO: mpst_receive: session not found"
    in
    let promise, inq =
      match session.request_queue with
      | `EmptyNoWait ->
          (* No client is waiting;
             make a promise, enqueuing its resolver in the queue,
             and return the promise *)
          let promise, resolv_f = Prr.Fut.create () in
          (promise, `EmptyWaiting resolv_f)
      | `Queued q ->
          (* An HTTP request is in the queue; *)
          let (_reqid, req), q =
            match Queue.pop q (* |q| > 0 *) with
            | None -> assert false
            | Some (e, q) -> (e, q)
          in
          (* return it *)
          (* and update the queue state accordingly *)
          let outq = if Queue.is_empty q then `EmptyNoWait else `Queued q in
          (return req, outq)
      | `EmptyWaiting _ ->
          failwith
            "process_receive.f: impossible: process is already waiting. Is \
             linearity check working?"
    in
    Hashtbl.replace server.established_sessions http_session_id
      { session with request_queue = inq };
    promise

  let send_to_client server ~http_session_id (msg : http_response) : unit =
    let session =
      try Hashtbl.find server.established_sessions http_session_id
      with Not_found -> failwith "TODO: mpst_send: session not found"
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
      { session with response_queue = outq }
end

let handle_request (server : Server.t) (request : Json.jv) : Json.jv ServerIo.t
    =
  let access fld msg =
    match Jv.access [ `f fld ] request with
    | Some (`str x) -> x
    | _ -> failwith msg
  in
  let entrypoint_id =
    EntrypointId.create @@ access "entrypoint" "entrypoint not given"
  in
  let role = Role.create @@ access "role" "role not given" in
  let conversation_id () =
    ConversationId.create
    @@ access "conversation_id" "conversation_id not given"
  in
  let http_session_id () =
    SessionId.create @@ access "session_id" "session_id not given"
  in
  let entrypoint_kind =
    match access "mode" "mode not given" with
    | "start" -> Server.StartLeader role
    | "start_follower" -> StartFollower (role, conversation_id ())
    | "join" -> Join role
    | "join_correlation" -> JoinCorrelation (role, conversation_id ())
    | "session" -> InSession (http_session_id ())
    | _ -> failwith "wrong mode"
  in
  Server.handle_entry server ~entrypoint_id ~entrypoint_kind ~request

module Server2
    (* : sig
         include Opstic.Endpoint


         val handle_request : Server.t -> http_response -> http_response io
       end
       with type 'x io = 'x ServerIo.t *) =
struct
  type 'x io = 'x ServerIo.t

  let return = ServerIo.return
  let ( >>= ) = ServerIo.bind

  module ServerEndpoint = struct
    type 'x io = 'x ServerIo.t

    type t = {
      server_ref : Server.t;
      protocol : Server.protocol_spec;
      conversation_id : conversation_id;
      self_role : role;
      role_http_session_id : (role, http_session_id option) Hashtbl.t;
    }

    type nonrec payload = payload

    let get_session_id t role =
      match Hashtbl.find t.role_http_session_id role with
      | Some s -> s
      | None ->
          failwith ("impossible: no session for role:" ^ Role.to_string role)
      | exception Not_found ->
          failwith ("impossible: no role:" ^ Role.to_string role)

    let send t ~connection ~role ~label ~payload : unit =
      assert (connection = Opstic.Connected);
      let role = Role.create role in
      let http_session_id = get_session_id t role in
      let msg : Json.jv =
        `obj [ ("label", `str label); ("payload", payload) ]
      in
      Server.send_to_client t.server_ref ~http_session_id msg

    let receive t ~(connection : Opstic.connection) ~role =
      let role = Role.create role in
      (match connection with
      | Connected ->
          let http_session_id = get_session_id t role in
          Server.receive_from_client t.server_ref ~http_session_id
      | Join | JoinCorrelation ->
          let kind = if connection = Join then `Fresh else `Correlation in
          Server.accept_join t.server_ref
            ~entrypoint_id:t.protocol.entrypoint_id ~role
            ~conversation_id:t.conversation_id ~kind)
      >>= fun payload ->
      match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
      | `obj [ ("label", `str label); ("payload", payload) ] ->
          ServerIo.return (label, payload)
      | _ -> failwith "bad payload"

    let close _ =
      (* TODO purge all sessions from server *)
      ()
  end

  module Mpst_js = Opstic.Make (ServerIo) (ServerEndpoint)

  let start_leader :
      Server.t -> spec:Server.protocol_spec -> witness:'wit -> 'wit Mpst_js.t io
      =
   fun server ~spec ~witness ->
    Server.register_entrypoint server ~spec;
    Server.accept_initial server ~entrypoint_id:spec.entrypoint_id
      ~kind:`AsLeader
    >>= fun (conversation_id, role, http_session_id) ->
    let role_http_session_id = Hashtbl.create 42 in
    spec.other_roles
    |> List.iter (fun role -> Hashtbl.replace role_http_session_id role None);
    Hashtbl.replace role_http_session_id role (Some http_session_id);
    let t =
      ServerEndpoint.
        {
          server_ref = server;
          protocol = spec;
          conversation_id;
          self_role = spec.my_role;
          role_http_session_id;
        }
    in
    return @@ Mpst_js.create t witness
end
