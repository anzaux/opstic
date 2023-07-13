open! Kxclib
include Monadic with type 'x t = 'x Prr.Fut.or_error

type error = Prr.Jv.Error.t

val map : ('x -> 'y) -> 'x t -> 'y t
val mapM : ('x -> 'y t) -> 'x list -> 'y list t
val create_promise : unit -> 'a t * (('a, error) result -> unit)
val error : error -> 'a t
val error_with : string -> 'a t
val then_ : (unit -> 'a t) -> (('a, error) result -> 'b t) -> 'b t
val handle_error : handler:(error -> 'a t) -> (unit -> 'a t) -> 'a t
val mpst_error : string -> error
val error_to_string : Prr.Jv.Error.t -> string
val error_to_string_full : Prr.Jv.Error.t -> string

module Common : sig
  type 'x io = 'x t
  val return : 'x -> 'x t
  val (let*) : 'x t -> ('x -> 'y t) -> 'y t
end