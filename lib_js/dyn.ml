type tagged = ..
type 'v tagger = { obj : tagged -> 'v; repr : 'v -> tagged }

module Add (X : sig
  type t
end) =
struct
  type tagged += Tag of X.t

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
