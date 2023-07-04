open! Kxclib

type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit
type payload = Json.jv

module ConversationId = Id.Make ()
module SessionId = Id.Make ()
module EntrypointId = Id.Make ()
module Role = Id.Make ()

type kind = [ `Greeting | `GreetingWithId | `Established ]
type http_session_id = SessionId.t [@@deriving show]
type conversation_id = ConversationId.t [@@deriving show]
type entrypoint_id = EntrypointId.t [@@deriving show]
type role = Role.t [@@deriving show]
type 'a io = 'a ServerIo.t

type http_request = {
  request_path : string;
  request_body : payload;
  request_onerror : ServerIo.error -> unit;
}

type http_response = payload
