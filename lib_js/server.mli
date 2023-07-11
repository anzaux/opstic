open! Types

type t
type path_spec = { path : string; path_kind : path_kind; path_role : Role.t }

type service_spec = {
  service_id : ServiceId.t;
  path_specs : (string, path_spec) Hashtbl.t;
  greeting_paths : string list;
  my_role : Role.t;
  other_roles : Role.t list;
  parse_session_id : payload -> SessionId.t;
}

type http_response = { response_role : Role.t; response_body : payload }

type http_request = {
  request_sessionid : session_id option;
  request_path : string;
  request_role : Role.t;
  request_body : payload;
  request_response_resolv : http_response waiting;
}

type greeting0 = {
  greeting_request : http_request;
  greeting_response : http_response waiting;
}

type greeting_queue = http_request ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = http_response ConcurrentQueue.t
type queue = { request_queue : request_queue; response_queue : response_queue }

type session = {
  session_id : session_id;
  queues : (role, queue) Hashtbl.t;
  service_ref : service;
}

and service = {
  spec : service_spec;
  greetings : (Role.t, greeting_queue) Hashtbl.t;
  sessions : (SessionId.t, session) Hashtbl.t;
  server_ref : t;
}

val create_server : unit -> t
val register_service : t -> spec:service_spec -> unit
val handle_entry : t -> path:string -> payload -> payload io

(* val init_session : service -> session io *)
val kill_session_ : session -> Monad.error -> unit
val kill_session : service -> session_id -> Monad.error -> unit

module Util : sig
  val get_queue_ : session -> role -> queue Monad.t
end
