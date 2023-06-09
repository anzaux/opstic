open! Types

type t

val create_server : unit -> t

module Handler : sig
  type entrypoint_kind =
    | StartLeader of role
    | StartFollower of role * conversation_id
    | Join of role
    | JoinCorrelation of role * conversation_id
    | InSession of http_session_id
  [@@deriving show]

  val handle_entry :
    t ->
    entrypoint_id:entrypoint_id ->
    entrypoint_kind:entrypoint_kind ->
    request:http_request ->
    http_response io
end

module Mpst : sig
  val accept_initial :
    t ->
    entrypoint_id:entrypoint_id ->
    kind:[ `AsFollower | `AsLeader ] ->
    roles:Role.t list ->
    (conversation_id * role * http_session_id) io

  val accept_join :
    t ->
    entrypoint_id:entrypoint_id ->
    role:role ->
    conversation_id:conversation_id ->
    kind:[ `Correlation | `Fresh ] ->
    http_request io

  val receive_from_client :
    t -> http_session_id:http_session_id -> http_request io

  val send_to_client :
    t -> http_session_id:http_session_id -> http_response -> unit io
end

type protocol_spec = {
  entrypoint_id : entrypoint_id;
  kind : [ `Leader | `Follower ];
  my_role : role;
  other_roles : role list;
  initial_roles : role list;
  joining_roles : role list;
  joining_correlation_roles : role list;
}
[@@deriving show]

val make_spec :
  entrypoint_id:string ->
  kind:[ `Leader | `Follower ] ->
  my_role:string ->
  other_roles:string list ->
  initial_roles:string list ->
  ?joining_roles:string list ->
  ?joining_correlation_roles:string list ->
  unit ->
  protocol_spec

val register_entrypoint : t -> spec:protocol_spec -> unit
