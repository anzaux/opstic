open Types

type nonrec ep = {
  server_ref : Server.t;
  protocol : Server.entrypoint_spec;
  conversation_id : conversation_id;
  self_role : role;
  sessions : (role, http_session_id option) Hashtbl.t;
}

type 't protocol = { entrypoint_spec : Server.entrypoint_spec; witness : 't }

val make_protocol :
  witness:'a ->
  entrypoint_id:string ->
  kind:[ `Follower | `Leader ] ->
  my_role:string ->
  other_roles:string list ->
  initial_roles:string list ->
  joining_roles:string list ->
  joining_correlation_roles:string list ->
  'a protocol

val register_protocol : Server.t -> protocol:'a protocol -> unit

include
  Opstic.Endpoint
    with type t = ep
     and type 'x io = 'x ServerIo.t
     and type payload = payload

val create :
  server:Server.t ->
  protocol:'a protocol ->
  conversation_id:conversation_id ->
  ep
