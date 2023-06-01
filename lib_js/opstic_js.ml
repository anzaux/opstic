open! Kxclib

module Payload = struct
  type payload = Prr.Jv.t
end

type 'a result_or_error = ('a, Prr.Jv.Error.t) result

module ServerIo : Monadic with type 'x t = 'x Prr.Fut.or_error = struct
  type 'x t = 'x Prr.Fut.or_error

  let return x = Prr.Fut.ok x

  let bind (m : _ t) (af : _ -> _ t) =
    Prr.Fut.bind m @@ function Error e -> Prr.Fut.error e | Ok x -> af x
end

module Mpst = Ojmpst.Make (Payload) (ServerIo)
open Payload

type sessionid = Js_of_ocaml.Js.js_string
type request = string * payload
type response = string * payload
type reqid = int

type request_queue =
  [ `EmptyNoWait
  | `EmptyWaiting of request result_or_error -> unit
  | `Queued of (reqid * request) queue (* |q| > 0 *) ]

type response_queue =
  [ `EmptyNoWait
  | `EmptyWaiting of
    (reqid * (response result_or_error -> unit)) queue (* |q| > 0 *)
  | `Queued of response queue ]

type session = {
  lastaccess : float option;
  req_count : int;
  request_queue : request_queue;
  response_queue : response_queue;
}

type server = { mutable sessions : (sessionid * session) list }

module Server = struct
  let create_server : server = { sessions = [] }

  let create_session () =
    {
      lastaccess = None;
      req_count = 0;
      request_queue = `EmptyNoWait;
      response_queue = `EmptyNoWait;
    }

  let update_session server ~sessionid (f : session -> session) =
    let f' session =
      let session =
        match session with Some st -> st | None -> create_session ()
      in
      let session = f session in
      Some session
    in
    server.sessions <- List.update_assoc sessionid f' server.sessions

  let process_post server sessionid (msg : string * payload) :
      (string * payload) Prr.Fut.or_error =
    (* The promise of the response *)
    let the_promise_for_response :
        (string * payload) Prr.Fut.or_error option ref =
      ref None
    in
    let doit session =
      let session =
        match session with None -> create_session () | Some s -> s
      in
      let request_id = session.req_count in
      let inq =
        (* Handle the request by either (1) enqueueing it or (2) delivering directly to the (MPST) process *)
        match session.request_queue with
        | `EmptyNoWait | `Queued _ ->
            (* (1) The MPST process is not waiting. Prepare the incoming queue ... *)
            let q =
              match session.request_queue with
              | `Queued q -> q
              | _ -> Queue.empty
            in
            (* and enqueue the request in it *)
            `Queued (Queue.push (request_id, msg) q)
        | `EmptyWaiting f ->
            (* (2) The MPST process is waiting. Deliver it directly *)
            f (Ok msg);
            `EmptyNoWait
      in
      let outq =
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
            let m, resolv_f = Prr.Fut.create () in
            the_promise_for_response := Some m;
            (* update the queue state by enqueuing the resolve function *)
            `EmptyWaiting (Queue.push (request_id, resolv_f) q)
        | `Queued q ->
            (* (2) A message is available in the queue *)
            let retmsg, q =
              match Queue.pop q with
              | Some p -> p
              | None -> assert false (* |q| > 0 *)
            in
            (* make a resolved promsie *)
            the_promise_for_response := Some (Prr.Fut.ok retmsg);
            (* and update the queue state *)
            if Queue.is_empty q then `EmptyNoWait else `Queued q
      in
      Some
        {
          session with
          request_queue = inq;
          response_queue = outq;
          req_count = request_id + 1;
        }
    in
    server.sessions <- List.update_assoc sessionid doit server.sessions;
    match !the_promise_for_response with
    | None ->
        (* this should not happen as we have made the promise during response check *)
        assert false
    | Some m -> m

  let mpst_receive server ~sessionid : (string * payload) ServerIo.t =
    let the_promise_to_receive = ref None in
    let doit session =
      let session =
        match session with None -> create_session () | Some s -> s
      in
      let inq' =
        match session.request_queue with
        | `EmptyNoWait ->
            (* No client is waiting;
               make a promise, enqueuing its resolver in the queue,
               and return the promise *)
            let m, resolv_f = Prr.Fut.create () in
            the_promise_to_receive := Some m;
            `EmptyWaiting resolv_f
        | `Queued q ->
            (* An HTTP request is in the queue; *)
            let (_reqid, req), q =
              match Queue.pop q (* |q| > 0 *) with
              | None -> assert false
              | Some (e, q) -> (e, q)
            in
            (* return it *)
            the_promise_to_receive := Some (Prr.Fut.ok req);
            (* and update the queue state accordingly *)
            if Queue.is_empty q then `EmptyNoWait else `Queued q
        | `EmptyWaiting _ ->
            failwith "process_receive.f: impossible: process is waiting?"
      in
      Some { session with request_queue = inq' }
    in
    server.sessions <- List.update_assoc sessionid doit server.sessions;
    match !the_promise_to_receive with
    | None ->
        (* this should not happen, we made it *)
        assert false
    | Some m -> m

  let mpst_send server ~sessionid (msg : response) : unit =
    let doit session =
      let session =
        match session with None -> create_session () | Some s -> s
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
      Some { session with response_queue = outq }
    in
    server.sessions <- List.update_assoc sessionid doit server.sessions
end

module ServerChannel = struct
  let make_server_channel server sessionid =
    Mpst.Endpoint.
      {
        send =
          (fun msg ->
            Server.mpst_send server ~sessionid msg;
            Prr.Fut.ok ());
        receive = (fun () -> Server.mpst_receive server ~sessionid);
        close = (fun () -> ());
      }
end
