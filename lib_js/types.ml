open! Kxclib

type 'a ok_or_error = ('a, Monad.error) result
type 'a waiting = 'a ok_or_error -> unit
type payload = Json.jv

module SessionId = Id.Make ()
module ServiceId = Id.Make ()
module Role = Id.Make ()

type path_kind = [ `Greeting | `GreetingWithId | `Established ]
type session_id = SessionId.t [@@deriving show]
type service_id = ServiceId.t [@@deriving show]
type role = Role.t [@@deriving show]
type 'a io = 'a Monad.t
