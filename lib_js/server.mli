open! Types

type path_spec = { path : path; path_kind : path_kind; path_role : Role.t }

type service_spec = {
  service_id : ServiceId.t;
  path_specs : (path, path_spec) Hashtbl.t;
  my_role : Role.t;
  other_roles : Role.t list;
  parse_session_id : payload -> SessionId.t io;
}

type response = payload

type request = {
  request_pathspec : path_spec;
  request_body : payload;
  request_resolv : response waiting;
}

type greeting_queue = request ConcurrentQueue.t
type request_queue = request ConcurrentQueue.t
type response_queue = response ConcurrentQueue.t
type queues = { request_queue : request_queue; response_queue : response_queue }

type session = {
  session_id : session_id;
  queues : (role, queues) Hashtbl.t;
  service_ref : service;
}

and invitation_queue = (session * request) ConcurrentQueue.t

and service = {
  spec : service_spec;
  greetings : (path, greeting_queue) Hashtbl.t;
  invitations : (path, invitation_queue) Hashtbl.t;
  sessions : (session_id, session) Hashtbl.t;
  server_ref : t;
}

and t = {
  services : (ServiceId.t, service) Hashtbl.t;
  service_path : (path, service_id) Hashtbl.t;
}

val create_server : unit -> t
val register_service : t -> spec:service_spec -> unit
val handle_entry : t -> path:string -> payload -> payload io
val kill_session : session -> Monad.error -> unit
val receive_at_paths : session -> path_spec list -> request io
val accept_at_paths : service -> path_spec list -> (session * request) io

module Util : sig
  val get_session : t -> service_id -> session_id -> session io
end
