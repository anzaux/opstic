open! Types

type path_spec = { path : path; path_kind : path_kind; path_role : Role.t }

type service_spec = {
  service_id : ServiceId.t;
  path_specs : (path, path_spec) Hashtbl.t;
  my_role : Role.t;
  other_roles : Role.t list;
  parse_session_id : payload -> SessionId.t io;
}

type server
type session
type service
type response = payload

type request = {
  request_pathspec : path_spec;
  request_body : payload;
  request_resolv : response waiting;
}

type request_queue = request ConcurrentQueue.t
type response_queue = response ConcurrentQueue.t
type queues = { request_queue : request_queue; response_queue : response_queue }
type greeting_queue = request ConcurrentQueue.t
type invitation_queue = (session * request) ConcurrentQueue.t

module Server : sig
  type t = server

  val create : unit -> t
  val service : t -> service_id -> service
  val register_service : t -> spec:service_spec -> unit
  val get_service : t -> path:path -> service io
  val handle_request : t -> path:string -> payload -> payload io
end

module Service : sig
  type t = service

  val server : t -> Server.t
  val id : t -> service_id
  val spec : t -> service_spec
  val greeting_queue : t -> path:path -> greeting_queue
  val invitation_queue : t -> path:path -> invitation_queue
  val get_session : t -> session_id -> session io
end

module Session : sig
  type t = session

  val service : t -> Service.t
  val id : t -> session_id
  val queues : t -> role -> queues
  val kill : t -> Monad.error -> unit
end

module Comm : sig
  val accept_at_paths : Service.t -> path_spec list -> (Session.t * request) io
  val receive_at_paths : Session.t -> path_spec list -> request io
end
