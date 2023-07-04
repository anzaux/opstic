open! Types

type t

type greeting = {
  greeting_conversation_id : ConversationId.t option;
  greeting_request : http_request;
  greeting_response : payload waiting;
}

type greeting_queue = greeting ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = payload ConcurrentQueue.t
type peer = { request_queue : request_queue; response_queue : response_queue }

type session = {
  conversation_id : conversation_id;
  peers : (role, peer) Hashtbl.t;
  entrypoint_ref : entrypoint;
}

and entrypoint = {
  entrypoint_id : entrypoint_id;
  my_role : role;
  other_roles : role list;
  greeting : (role, greeting_queue) Hashtbl.t;
  established : (conversation_id, session) Hashtbl.t;
}

val create_server : unit -> t

val register_entrypoint :
  t -> id:entrypoint_id -> my_role:role -> other_roles:role list -> entrypoint

val handle_entry :
  t ->
  entrypoint_id:entrypoint_id ->
  path:string ->
  role:role ->
  kind:
    [ `Greeting
    | `GreetingWithId of conversation_id
    | `Established of conversation_id ] ->
  payload ->
  payload ServerIo.t

val new_session_from_greeting :
  [ `Greeting | `GreetingWithId ] -> entrypoint -> role -> session io

val kill_session : entrypoint -> conversation_id -> ServerIo.error -> unit

val accept_greeting :
  [ `Greeting | `GreetingWithId ] -> entrypoint -> session -> role -> unit io

module Util : sig
  val get_entrypoint : t -> entrypoint_id -> entrypoint ServerIo.t

  val get_greeting_queue :
    t -> entrypoint_id -> role -> greeting_queue ServerIo.t

  val get_session : t -> entrypoint_id -> conversation_id -> session ServerIo.t

  val get_peer :
    t -> entrypoint_id -> conversation_id -> role -> peer ServerIo.t
end
