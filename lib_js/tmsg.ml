module Tmsg : sig
  type tagged
  type 'v tagger
  type ep_raw = Server.session
  type 'a branch = private ep_raw -> 'a

  val create : unit -> 'a tagger
  val obj : 'a tagger -> tagged -> 'a branch
  val repr : 'a tagger -> 'a branch -> tagged
end = struct
  type tagged = ..
  type ep_raw = Server.session
  type 'a branch = private ep_raw -> 'a
  type 'v tagger = { obj : tagged -> 'v branch; repr : 'v branch -> tagged }

  module Add (X : sig
    type t
  end) =
  struct
    type tagged += Tag of X.t branch

    let obj tagged =
      match tagged with Tag x -> x | _ -> failwith "Dyn: impossible"

    let repr v = Tag v
  end

  let create (type a) () : a tagger =
    let module M = Add (struct
      type t = a
    end) in
    { obj = M.obj; repr = M.repr }

  type t = tagged

  let obj tagger t = tagger.obj t
  let repr tagger v = tagger.repr v
end
