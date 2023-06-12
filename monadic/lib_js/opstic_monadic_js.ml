open! Kxclib
open! Opstic_js
open Opstic_js.ServerEndpoint

module Mpst_js = struct
  open ServerIo

  let ( let* ) = ServerIo.bind

  module Mpst_js = Opstic_monadic.Make (ServerIo) (Opstic_js.ServerEndpoint)

  let accept_start_session :
      (* FIXME bad API style *)
      Server.t ->
      spec:Server.protocol_spec ->
      witness:'b Mpst_js.Comm.inp ->
      'b ServerIo.t =
   fun server ~spec ~witness ->
    let* conversation_id, role, http_session_id =
      Server.Mpst.accept_initial server ~entrypoint_id:spec.entrypoint_id
        ~kind:`AsLeader ~roles:spec.other_roles
    in
    let role_http_session_id = Hashtbl.create 42 in
    spec.other_roles
    |> List.iter (fun role -> Hashtbl.replace role_http_session_id role None);
    Hashtbl.replace role_http_session_id role (Some http_session_id);
    let t =
      {
        server_ref = server;
        protocol = spec;
        conversation_id;
        self_role = spec.my_role;
        role_http_session_id;
      }
    in
    let inp = witness in
    (* FIXME use Comm.receive instead *)
    let* payload = Server.Mpst.receive_from_client server ~http_session_id in
    let* payload =
      match Jv.access [ `f "message" ] payload with
      | Some x -> return x
      | _ ->
          error_with
            (Format.asprintf "no message body in request: %a" Json.pp_lit
               payload)
    in
    match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
    | `obj [ ("label", `str label); ("payload", v) ] ->
        let role_str = Role.to_string role in
        let (InpChoice c) = Hashtbl.find inp.inp_choices (role_str, label) in
        let v = c.inp_choice_marshal v in
        return
          (c.inp_choice_role.make_var
          @@ c.inp_choice_label.make_var
               ( v,
                 {
                   ep_raw = t;
                   ep_witness = Opstic.Lin.create c.inp_choice_next_wit;
                 } ))
    | _ -> error_with (Format.asprintf "bad payload: %a" Json.pp_lit payload)

  include Mpst_js
end
