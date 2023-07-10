open! Types

type t
type path_kind = [ `Established | `Greeting | `GreetingWithId ]

type path_spec = {
  path : string;
  path_kind : path_kind;
  path_role : Role.t;
}

and entrypoint_spec = {
  service_id : ServiceId.t;
  path_specs : (string, path_spec) Hashtbl.t;
  greeting_paths : string list;
  my_role : Role.t;
  other_roles : Role.t list;
  get_converssation_id : payload -> SessionId.t;
}

type http_request = {
  request_path : string;
  request_role : Role.t;
  request_body : payload;
  request_onerror : Monad.error -> unit;
}

type http_response = { response_role : Role.t; response_body : payload }

type greeting0 = {
  greeting_request : http_request;
  greeting_response : http_response waiting;
}

type greeting = GreetingWithId of SessionId.t | Greeting of greeting0
type greeting_queue = greeting ConcurrentQueue.t
type request_queue = http_request ConcurrentQueue.t
type response_queue = http_response ConcurrentQueue.t

type queuepair = {
  request_queue : request_queue;
  response_queue : response_queue;
}

type session = {
  session_id : session_id;
  queues : (role, queuepair) Hashtbl.t;
  entrypoint_ref : entrypoint;
}

and entrypoint = {
  spec : entrypoint_spec;
  greetings : (Role.t, greeting_queue) Hashtbl.t;
  sessions : (SessionId.t, session) Hashtbl.t;
  server_ref : t;
}

val create_server : unit -> t
val register_entrypoint : t -> spec:entrypoint_spec -> entrypoint

val handle_entry :
  t ->
  service_id:Types.service_id ->
  path:string ->
  payload ->
  payload Types.io

val init_session : entrypoint -> session io
val kill_session_ : session -> Monad.error -> unit
val kill_session : entrypoint -> session_id -> Monad.error -> unit

(* val accept_greeting :
   [ `Greeting | `GreetingWithId ] -> entrypoint -> session -> role -> unit io *)

module Util : sig
  val get_entrypoint : t -> service_id -> entrypoint Monad.t

  val get_greeting_queue :
    t -> service_id -> role -> greeting_queue Monad.t

  val get_session : t -> service_id -> session_id -> session Monad.t
  val get_queuepair_ : session -> role -> queuepair Monad.t

  val get_queuepair :
    t -> service_id -> session_id -> role -> queuepair Monad.t
end
