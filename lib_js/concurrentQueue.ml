open! Kxclib

exception QueueKilled of Monad.error

type 'a ok_or_error = ('a, Monad.error) result
type 'a waiting = 'a ok_or_error -> unit

type _ waiter =
  | Waiter : {
      waiting : 'b waiting option ref;
      wrap : 'a -> 'b ok_or_error;
    }
      -> 'a waiter

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

type _ wrapped =
  | Wrapped : { queue : 'b t; wrap : 'b -> 'a ok_or_error } -> 'a wrapped

let return = Monad.return
let create () = ref EmptyNotWaiting
let invalidate_waiter (Waiter r) = r.waiting := None

let is_empty { contents = t } =
  match t with
  | EmptyNotWaiting | EmptyWaiting _ -> true
  | Queued _ -> false
  | Killed _ -> true

let wrap_waiting (type a b) (waiting : a waiting) (f : b -> a ok_or_error) :
    b waiting = function
  | Ok x -> waiting (f x)
  | Error err -> waiting (Error err)

let new_waiter f = Waiter { waiting = ref (Some f); wrap = (fun x -> Ok x) }
let new_shared_waiter ~wrap waiting = Waiter { waiting; wrap }

(* Dequeue a waiter if any. *)
let rec pop_waiter (q : 'a waiter queue) =
  match Queue.pop q with
  | Some ((Waiter { waiting = { contents = Some waiting }; wrap } as r), q) ->
      (* Set it to None to prevent further call on the shared waiter *)
      invalidate_waiter r;
      Some (wrap_waiting waiting wrap, q)
  | Some (Waiter { waiting = { contents = None }; _ }, q) -> pop_waiter q
  | None -> None

let enqueue (type a) (t : a t) (value : a) =
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

let dequeue (type a) (t : a t) : a Monad.t =
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
  | Waiter { waiting = { contents = None }; _ } -> ()
  | Waiter { waiting = { contents = Some waiting }; wrap } -> (
      match !t with
      | EmptyNotWaiting | EmptyWaiting _ ->
          let resq = match !t with EmptyWaiting q -> q | _ -> Queue.empty in
          (* Update the state by enqueuing the resolve function *)
          t := EmptyWaiting (Queue.push shared_resolv_f resq)
      | Queued q ->
          invalidate_waiter shared_resolv_f;
          let v, q =
            match Queue.pop q with
            | Some x -> x
            | None -> assert false (* |q|>0 *)
          in
          t := if Queue.is_empty q then EmptyNotWaiting else Queued q;
          wrap_waiting waiting wrap (Ok v)
      | Killed err -> raise (QueueKilled err))

let add_waiter t resolv_f = add_shared_waiter t (new_waiter resolv_f)

let dequeue_one_of ts =
  let promise, resolv_f = Monad.create_promise () in
  let shared_resolv_f = new_waiter resolv_f in
  List.iter (fun t -> add_shared_waiter t shared_resolv_f) ts;
  promise

let kill t err =
  let t0 = !t in
  t := Killed err;
  match t0 with
  | EmptyWaiting q ->
      let rec loop q =
        match Queue.pop q with
        | Some ((Waiter { waiting = { contents = Some resolv }; _ } as r), q) ->
            invalidate_waiter r;
            resolv (Error err);
            loop q
        | Some (Waiter { waiting = { contents = None }; _ }, q) -> loop q
        | None -> ()
      in
      loop q
  | EmptyNotWaiting | Killed _ -> ()
  | Queued _ ->
      Kxclib.Log0.warn "Non-empty concurrent queue is killed with error";
      ()

let wrap q ~wrapper = Wrapped { queue = q; wrap = wrapper }
let nowrap q = Wrapped { queue = q; wrap = (fun x -> Ok x) }

let dequeue_one_of_wrapped ts =
  let promise, resolv_f = Monad.create_promise () in
  let shared_resolv_f = ref (Some resolv_f) in
  List.iter
    (fun (Wrapped { queue; wrap }) ->
      let waiter = new_shared_waiter shared_resolv_f ~wrap in
      add_shared_waiter queue waiter)
    ts;
  promise
