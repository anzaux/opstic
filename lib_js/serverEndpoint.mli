open Types

type nonrec ep = {
  server_ref : Server.t;
  protocol : Server.protocol_spec;
  conversation_id : conversation_id;
  self_role : role;
  role_http_session_id : (role, http_session_id option) Hashtbl.t;
}

include
  Opstic.Endpoint
    with type t = ep
     and type 'x io = 'x ServerIo.t
     and type payload = payload
