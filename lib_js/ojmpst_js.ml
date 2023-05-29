open! Kxclib

module ServerIo = struct
  type 'x t = 'x Prr.Fut.or_error

  let return x = Prr.Fut.ok x

  let bind (m : _ t) (af : _ -> _ t) =
    Prr.Fut.bind m @@ function Error e -> Prr.Fut.error e | Ok x -> af x
end

module Mpst = Ojmpst.Make (ServerIo)

type payload = Json.jv

module ServerChannel = struct
  type t = {
    respond : payload -> unit;
    request_get : unit -> unit ServerIo.t;
    request_post : unit -> payload ServerIo.t;
  }

  let send ch (lab, payload) =
    ch.respond (`obj [ ("label", lab); ("payload", payload) ])

  let receive ch = 
    Prr.Fut.bind (ch.request_post ()) (fun x -> Prr.Fut.return x)
end
