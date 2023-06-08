module Make () : sig
  type t [@@deriving show]

  val to_string : t -> string
  val create : string -> t
end = struct
  type t = string [@@deriving show]

  let to_string x = x
  let create x = x
end
