type 'a t
type 'a ok_or_error = ('a, Monad.error) result
type 'a waiting = 'a ok_or_error -> unit
(* type 'a wrapper *)

exception QueueKilled of Monad.error

val create : unit -> 'a t
val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a Monad.t
val kill : 'a t -> Monad.error -> unit
val add_waiter : 'a t -> 'a waiting -> unit
val dequeue_one_of : 'a t list -> 'a Monad.t
(* val wrap : 'a t -> ('a -> 'b) -> 'b wrapper
val dequeue_one_of_wrapped : 'a wrapper list -> 'a Monad.t *)
