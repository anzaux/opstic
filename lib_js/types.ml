open! Kxclib
open! Js_of_ocaml

type 'a ok_or_error = ('a, Monad.error) result
type 'a waiting = 'a ok_or_error -> unit
type payload = Kxclib.Json.jv

module SessionId = Id.Make ()
module ServiceId = Id.Make ()
module Role = Id.Make ()
module Path = Id.Make ()

type path_kind = [ `Greeting | `Invitation | `Established ]
type session_id = SessionId.t [@@deriving show, eq]
type service_id = ServiceId.t [@@deriving show, eq]
type role = Role.t [@@deriving show, eq]
type path = Path.t [@@deriving show, eq]
type 'a io = 'a Monad.t
type express_req_type = Prr.Jv.t
