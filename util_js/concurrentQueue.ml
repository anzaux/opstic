open! Kxclib

type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit

type 'a _t =
  | EmptyNotWaiting
  | EmptyWaiting of 'a waiting queue (* |q| > 0 *)
  | Queued of 'a queue (* |q| > 0 *)

type 'a t = 'a _t ref

let return = ServerIo.return
let create () = ref EmptyNotWaiting

let enqueue t v =
  let q =
    (* (1) enqueueing it or (2) deliver the value directly to the waiting thread *)
    match !t with
    | EmptyNotWaiting | Queued _ ->
        (* (1) No one is waiting for this value yet. Prepare the queue ... *)
        let q = match !t with Queued q -> q | _ -> Queue.empty in
        (* and enqueue the request in it *)
        Queued (Queue.push v q)
    | EmptyWaiting q ->
        (* (2) Someone is waiting. Deliver it directly *)
        let resolv_f, q =
          match Queue.pop q with
          | Some x -> x
          | None -> assert false (* |q|>0 *)
        in
        resolv_f (Ok v);
        if Queue.is_empty q then EmptyNotWaiting else EmptyWaiting q
  in
  t := q

let dequeue t =
  (* Check the response queue and (1) make a promise if empty or (2) send back the message directly if some   *)
  match !t with
  | EmptyNotWaiting | EmptyWaiting _ ->
      (* (1) The queue is empty. Make a promise for the dequeue, and *)
      let promise, resolv_f = ServerIo.create_promise () in
      (* Set up the internal queue for resolve functions *)
      let resq = match !t with EmptyWaiting q -> q | _ -> Queue.empty in
      (* Update the state by enqueuing the resolve function *)
      t := EmptyWaiting (Queue.push resolv_f resq);
      promise
  | Queued q ->
      (* (2) A message is available in the queue. Dequeue it *)
      let retmsg, q =
        match Queue.pop q with
        | Some p -> p
        | None -> assert false (* |q| > 0 *)
      in
      (* updating the queue state *)
      t := if Queue.is_empty q then EmptyNotWaiting else Queued q;
      (* and return it *)
      return retmsg

let add_waitor t resolv_f =
  match !t with
  | EmptyNotWaiting | EmptyWaiting _ ->
      let resq = match !t with EmptyWaiting q -> q | _ -> Queue.empty in
      (* Update the state by enqueuing the resolve function *)
      t := EmptyWaiting (Queue.push resolv_f resq)
  | Queued q ->
      let v, q =
        match Queue.pop q with Some x -> x | None -> assert false (* |q|>0 *)
      in
      t := if Queue.is_empty q then EmptyNotWaiting else Queued q;
      resolv_f (Ok v)
