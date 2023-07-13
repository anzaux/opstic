module type S = sig
  type t [@@deriving show, eq]

  val to_string : t -> string
  val create : string -> t
end

module Make () : S = struct
  type t = string [@@deriving show, eq]

  let to_string x = x
  let create x = x
end
