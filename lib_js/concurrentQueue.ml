open! Kxclib

exception QueueKilled of Monad.error

type 'a ok_or_error = ('a, Monad.error) result
type 'a waiting = 'a ok_or_error -> unit

type 'a waiter = {
  mutable waiter : 'a waiting option;
  invalidate : unit -> unit;
}

type 'a _t =
  (* An empty queue content. No one is waiting. *)
  | EmptyNotWaiting
  (* A queue with contents. *)
  | Queued of 'a queue (* |q| > 0 *)
  (* Someone is waiting on the queue.
     NB: The waiter (reference cell) is possibly shared among queues (for `dequeue_one_of`).
     Once invoked, the waiter cell is set to None, to prevent being called again *)
  | EmptyWaiting of 'a waiter queue (* |q| > 0 *)
  | Killed of Monad.error

type 'a t = 'a _t ref

(* type 'b wrapped =
   | Wrapped : { wrapped_queue : 'a t; wrapper : 'a -> 'b } -> 'b wrapped *)

let return = Monad.return
let create () = ref EmptyNotWaiting

let new_waiter ?invalidate f =
  let rec r =
    {
      waiter = Some f;
      invalidate =
        (fun () ->
          r.waiter <- None;
          match invalidate with Some f -> f () | None -> ());
    }
  in
  r

(* Dequeue a waiter if any. *)
let rec pop_waiter (q : 'a waiter queue) =
  match Queue.pop q with
  | Some ({ waiter = Some f; invalidate }, q) ->
      (* Set it to None to prevent further call on the shared waiter *)
      invalidate ();
      Some (f, q)
  | Some ({ waiter = None; _ }, q) -> pop_waiter q
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
    | Killed err -> raise (QueueKilled err)
  in
  t := q

let dequeue t =
  (* Check the response queue and (1) make a promise if empty or (2) send back the message directly if some   *)
  match !t with
  | EmptyNotWaiting | EmptyWaiting _ ->
      (* (1) The queue is empty. Make a promise for the dequeue, and *)
      let promise, resolv_f = Monad.create_promise () in
      (* Set up the internal queue for resolve functions *)
      let resq = match !t with EmptyWaiting q -> q | _ -> Queue.empty in
      (* Update the state by enqueuing the resolve function *)
      t := EmptyWaiting (Queue.push (new_waiter resolv_f) resq);
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
  | Killed err -> raise (QueueKilled err)

let add_shared_waiter t shared_resolv_f =
  match shared_resolv_f with
  | { waiter = None; _ } -> ()
  | { waiter = Some resolv_f; invalidate } -> (
      match !t with
      | EmptyNotWaiting | EmptyWaiting _ ->
          let resq = match !t with EmptyWaiting q -> q | _ -> Queue.empty in
          (* Update the state by enqueuing the resolve function *)
          t := EmptyWaiting (Queue.push shared_resolv_f resq)
      | Queued q ->
          invalidate ();
          let v, q =
            match Queue.pop q with
            | Some x -> x
            | None -> assert false (* |q|>0 *)
          in
          t := if Queue.is_empty q then EmptyNotWaiting else Queued q;
          resolv_f (Ok v)
      | Killed err -> raise (QueueKilled err))

let add_waiter t resolv_f = add_shared_waiter t (new_waiter resolv_f)

let dequeue_one_of ts =
  let promise, resolv_f = Monad.create_promise () in
  let shared_resolv_f = new_waiter resolv_f in
  List.iter (fun t -> add_shared_waiter t shared_resolv_f) ts;
  promise

(*
   let dequeue_one_of_wrapped ts =
     let invalidated = ref false in
     let invalidate = ref (fun () -> invalidated := true) in
     let promise, resolv_f = Monad.create_promise () in
     let proc (Wrapped { wrapped_queue = t; wrapper }) =
       if !invalidated then ()
       else
         let waiter =
           new_waiter (fun x ->
               !invalidate ();
               match x with
               | Ok x -> resolv_f (Ok (wrapper x))
               | Error err -> resolv_f (Error err))
         in
         let old = !invalidate in
         (invalidate :=
            fun () ->
              waiter.waiter <- None;
              old ());
         add_shared_waiter t waiter;
         ()
     in
     List.iter proc ts;
     promise

   let wrap t f = Wrapped { wrapped_queue = t; wrapper = f } *)

let kill t err =
  let t0 = !t in
  t := Killed err;
  match t0 with
  | EmptyWaiting q ->
      let rec loop q =
        match Queue.pop q with
        | Some ({ waiter = Some resolv; invalidate }, q) ->
            invalidate ();
            resolv (Error err);
            loop q
        | Some ({ waiter = None; _ }, q) -> loop q
        | None -> ()
      in
      loop q
  | EmptyNotWaiting | Killed _ -> ()
  | Queued _ ->
      Kxclib.Log0.warn "Non-empty concurrent queue is killed with error";
      ()
