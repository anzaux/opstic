open! Kxclib

type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit
type 'a maybe_waiting = 'a waiting option

type 'a _t =
  (* An empty queue content. No one is waiting. *)
  | EmptyNotWaiting
  (* A queue with contents. *)
  | Queued of 'a queue (* |q| > 0 *)
  (* Someone is waiting on the queue.
     NB: The waiter (reference cell) is possibly shared among queues (for `dequeue_one_of`).
     Once invoked, the waiter cell is set to None, to prevent being called again *)
  | EmptyWaiting of 'a maybe_waiting ref queue (* |q| > 0 *)

type 'a t = 'a _t ref

let return = ServerIo.return
let create () = ref EmptyNotWaiting

(* Dequeue a waiter if any. *)
let rec pop_waiter (q : 'a maybe_waiting ref queue) =
  match Queue.pop q with
  | Some (({ contents = Some f } as r), q) ->
      (* Set it to None to prevent further call on the shared waiter *)
      r := None;
      Some (f, q)
  | Some ({ contents = None }, q) -> pop_waiter q
  | None -> None

let enqueue t value =
  let q =
    (* (1) enqueueing it or (2) deliver the value directly to the waiting thread *)
    match !t with
    | EmptyNotWaiting | Queued _ ->
        (* (1) No one is waiting for this value yet. Prepare the queue ... *)
        let q = match !t with Queued q -> q | _ -> Queue.empty in
        (* and enqueue the request in it *)
        Queued (Queue.push value q)
    | EmptyWaiting q -> (
        (* (2) Someone is possibly waiting. *)
        match pop_waiter q with
        | Some (resolv_f, q) ->
            (* A waiter found. resolve it *)
            resolv_f (Ok value);
            if Queue.is_empty q then EmptyNotWaiting else EmptyWaiting q
        | None ->
            (* No waiter there, in fact. Enqueue it *)
            Queued (Queue.push value Queue.empty))
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
      t := EmptyWaiting (Queue.push (ref (Some resolv_f)) resq);
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

let add_shared_waiter t shared_resolv_f =
  match shared_resolv_f with
  | { contents = None } -> ()
  | { contents = Some resolv_f } -> (
      match !t with
      | EmptyNotWaiting | EmptyWaiting _ ->
          let resq = match !t with EmptyWaiting q -> q | _ -> Queue.empty in
          (* Update the state by enqueuing the resolve function *)
          t := EmptyWaiting (Queue.push shared_resolv_f resq)
      | Queued q ->
          let v, q =
            match Queue.pop q with
            | Some x -> x
            | None -> assert false (* |q|>0 *)
          in
          t := if Queue.is_empty q then EmptyNotWaiting else Queued q;
          shared_resolv_f := None;
          resolv_f (Ok v))

let add_waiter t resolv_f = add_shared_waiter t (ref (Some resolv_f))

let dequeue_one_of ts =
  let promise, resolv_f = ServerIo.create_promise () in
  let shared_resolv_f = ref (Some resolv_f) in
  List.iter (fun t -> add_shared_waiter t shared_resolv_f) ts;
  promise
