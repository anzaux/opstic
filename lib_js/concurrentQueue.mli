type 'a t
type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit

exception QueueKilled of ServerIo.error

val create : unit -> 'a t
val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a ServerIo.t
val kill : 'a t -> ServerIo.error -> unit
val add_waiter : 'a t -> 'a waiting -> unit
val dequeue_one_of : 'a t list -> 'a ServerIo.t
