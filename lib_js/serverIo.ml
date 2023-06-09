open Prr

type 'x t = 'x Fut.or_error

let return x = Fut.ok x

let bind (m : _ t) (af : _ -> _ t) =
  Fut.bind m @@ function Error e -> Fut.error e | Ok x -> af x

type error = Jv.Error.t

let map f m = Fut.map (function Error e -> Error e | Ok x -> Ok (f x)) m
let create_promise = Fut.create
let error err = Fut.error err

let mpst_error msg =
  let open Prr in
  Jv.Error.v ~name:(Jstr.v "OpsticError") (Jstr.v msg)

let error_with msg =
  let err = mpst_error msg in
  error err

let handle_error m ~handler =
  Fut.bind m (function Ok x -> return x | Error err -> handler err)
