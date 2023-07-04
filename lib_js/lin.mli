type 'a t

exception LinearityViolation

val create : 'a -> 'a t
val get : 'a t -> 'a
