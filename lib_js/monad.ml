open Prr

exception AssertionError of string

type 'x t = 'x Fut.or_error

let return x = Fut.ok x

let mpst_error msg =
  let open Prr in
  Jv.Error.v ~name:(Jstr.v "OpsticError") (Jstr.v msg)

let bind (m : _ t) (af : _ -> _ t) =
  Fut.bind m @@ function
  | Error e -> Fut.error e
  | Ok x -> (
      try af x with exn -> Fut.error (mpst_error (Printexc.to_string exn)))

type error = Jv.Error.t

let map f m = Fut.map (function Error e -> Error e | Ok x -> Ok (f x)) m
let create_promise = Fut.create
let error err = Fut.error err

let rec mapM f = function
  | x :: xs ->
      bind (f x) (fun y -> bind (mapM f xs) (fun ys -> return (y :: ys)))
  | [] -> return []

let error_with msg =
  let err = mpst_error msg in
  error err

let handle_error ~handler f =
  let m = try f () with exn -> error_with (Printexc.to_string exn) in
  Fut.bind m (function Ok x -> return x | Error err -> handler err)
