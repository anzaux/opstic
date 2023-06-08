module Make () : sig
  type t

  val to_string : t -> string
  val create : string -> t
end = struct
  type t = string

  let to_string x = x
  let create x = x
end
