open! Kxclib

type 'a ok_or_error = ('a, Monad.error) result
type 'a waiting = 'a ok_or_error -> unit
type payload = Json.jv

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
