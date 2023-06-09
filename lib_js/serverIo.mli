open! Kxclib
include Monadic with type 'x t = 'x Prr.Fut.or_error

type error = Prr.Jv.Error.t

val map : ('x -> 'y) -> 'x t -> 'y t
val create_promise : unit -> 'a t * (('a, error) result -> unit)
val error : error -> 'a t
val error_with : string -> 'a t
val handle_error : 'a t -> handler:(error -> 'a t) -> 'a t
