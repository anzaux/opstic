open! Types

type t
type path_kind = [ `Established | `Greeting | `GreetingWithId ]

type path_spec = {
  path : string;
  path_kind : path_kind;
  path_role : Role.t;
  path_request_parse : payload -> (ConversationId.t * string * Dyn.t) option;
  path_response_unparse : ConversationId.t * string * Dyn.t -> payload;
}

and entrypoint_spec = {
  entrypoint_id : EntrypointId.t;
  path_specs : (string, path_spec) Hashtbl.t;
  greeting_paths : string list;
  my_role : Role.t;
  other_roles : Role.t list;
}

type http_request = {
  request_path : string;
  request_role : Role.t;
  request_label : string;
  request_body : Dyn.t;
  request_body_raw : payload;
  request_onerror : ServerIo.error -> unit;
}

type http_response = {
  response_role : Role.t;
  response_label : string;
  response_body : Dyn.t;
  response_body_raw : payload;
}

type greeting0 = {
  greeting_request : http_request;
  greeting_response : http_response waiting;
}

type greeting = GreetingWithId of ConversationId.t | Greeting of greeting0
type greeting_queue = greeting ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = http_response ConcurrentQueue.t

type queuepair = {
  request_queue : request_queue;
  response_queue : response_queue;
}

type session = {
  conversation_id : conversation_id;
  queues : (role, queuepair) Hashtbl.t;
  entrypoint_ref : entrypoint;
}

and entrypoint = {
  spec : entrypoint_spec;
  greetings : (Role.t, greeting_queue) Hashtbl.t;
  sessions : (ConversationId.t, session) Hashtbl.t;
  server_ref : t;
}

val create_server : unit -> t
val register_entrypoint : t -> spec:entrypoint_spec -> entrypoint

val handle_entry :
  t ->
  entrypoint_id:Types.entrypoint_id ->
  path:string ->
  payload ->
  payload Types.io

val init_session : entrypoint -> session io
val kill_session_ : session -> ServerIo.error -> unit
val kill_session : entrypoint -> conversation_id -> ServerIo.error -> unit

(* val accept_greeting :
   [ `Greeting | `GreetingWithId ] -> entrypoint -> session -> role -> unit io *)

module Util : sig
  val get_entrypoint : t -> entrypoint_id -> entrypoint ServerIo.t

  val get_greeting_queue :
    t -> entrypoint_id -> role -> greeting_queue ServerIo.t

  val get_session : t -> entrypoint_id -> conversation_id -> session ServerIo.t
  val get_queuepair_ : session -> role -> queuepair ServerIo.t

  val get_queuepair :
    t -> entrypoint_id -> conversation_id -> role -> queuepair ServerIo.t
end
