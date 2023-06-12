open! Kxclib
include Types
module ServerIo = ServerIo
module ConcurrentQueue = ConcurrentQueue
module Server = Server

type nonrec ep = {
  server_ref : Server.t;
  protocol : Server.protocol_spec;
  conversation_id : conversation_id;
  self_role : role;
  role_http_session_id : (role, http_session_id option) Hashtbl.t;
}
[@@warning "-69"]

module ServerEndpoint :
  Opstic.Endpoint
    with type t = ep
     and type 'x io = 'x ServerIo.t
     and type payload = payload = struct
  type 'x io = 'x ServerIo.t

  type nonrec t = ep = {
    server_ref : Server.t;
    protocol : Server.protocol_spec;
    conversation_id : conversation_id;
    self_role : role;
    role_http_session_id : (role, http_session_id option) Hashtbl.t;
  }
  [@@warning "-69"]

  open ServerIo

  let ( let* ) = ServerIo.bind

  type nonrec payload = payload

  let get_session_id ~ctx t role =
    match Hashtbl.find t.role_http_session_id role with
    | Some s -> return s
    | None ->
        error_with
          (Format.asprintf "%s: Session is not established for role: %a" ctx
             Role.pp role)
    | exception Not_found ->
        error_with
          (Format.asprintf "%s: impossible: No such role: %a" ctx Role.pp role)

  let send t ~connection ~role ~label ~payload : unit io =
    assert (connection = Opstic.Connected);
    let role = Role.create role in
    let* http_session_id = get_session_id t role ~ctx:"send" in
    let msg : Json.jv = `obj [ ("label", `str label); ("payload", payload) ] in
    Server.Mpst.send_to_client t.server_ref ~http_session_id msg

  let receive t ~(connection : Opstic.connection) ~roles =
    match roles with
    | [ role ] -> (
        let role = Role.create role in
        let* payload =
          match connection with
          | Connected ->
              let* http_session_id = get_session_id t role ~ctx:"receive" in
              Server.Mpst.receive_from_client t.server_ref ~http_session_id
          | Join | JoinCorrelation ->
              let kind = if connection = Join then `Fresh else `Correlation in
              Server.Mpst.accept_join t.server_ref
                ~entrypoint_id:t.protocol.entrypoint_id ~role
                ~conversation_id:t.conversation_id ~kind
        in
        match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
        | `obj [ ("label", `str label); ("payload", payload) ] ->
            return (Role.to_string role, label, payload)
        | _ -> error_with "bad payload")
    | _ ->
        error_with
          "TODO: Opstic_monadic_js: can't wait for multiple roles for now"

  let close _ =
    (* TODO purge all sessions from server *)
    ()
end
