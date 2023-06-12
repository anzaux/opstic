open Types
open Kxclib

type 'x io = 'x ServerIo.t

type nonrec ep = {
  server_ref : Server.t;
  protocol : Server.entrypoint_spec;
  conversation_id : conversation_id;
  self_role : role;
  sessions : (role, http_session_id option) Hashtbl.t;
}
[@@warning "-69"]

type t = ep

open ServerIo
open Types

let ( let* ) = ServerIo.bind

type nonrec payload = payload

let get_session_id ~ctx t role =
  match Hashtbl.find t.sessions role with
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

type 't protocol = { entrypoint_spec : Server.entrypoint_spec; witness : 't }

let make_protocol ~witness ~entrypoint_id ~kind ~my_role ~other_roles
    ~initial_roles ~joining_roles ~joining_correlation_roles =
  let spec =
    Server.make_entrypoint_spec ~entrypoint_id ~kind ~my_role ~other_roles
      ~initial_roles ~joining_roles ~joining_correlation_roles ()
  in
  { entrypoint_spec = spec; witness }

let register_protocol server ~protocol =
  Server.register_entrypoint server ~spec:protocol.entrypoint_spec

let create ~server ~protocol ~conversation_id =
  let open Server in
  let sessions = Hashtbl.create 42 in
  protocol.entrypoint_spec.other_roles
  |> List.iter (fun role -> Hashtbl.replace sessions role None);
  {
    server_ref = server;
    protocol = protocol.entrypoint_spec;
    conversation_id;
    self_role = protocol.entrypoint_spec.my_role;
    sessions;
  }
