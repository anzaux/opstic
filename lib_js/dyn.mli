type t
type 'a tagger

val create : unit -> 'a tagger
val obj : 'a tagger -> t -> 'a
val repr : 'a tagger -> 'a -> t
