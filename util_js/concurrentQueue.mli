type 'a t
type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit

val create : unit -> 'a t
val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a ServerIo.t
val add_waitor : 'a t -> 'a waiting -> unit
