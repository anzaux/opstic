open! Kxclib
open! Opstic_js
open Opstic_js.ServerEndpoint

module Mpst_js = struct
  open ServerIo

  let ( let* ) = ServerIo.bind

  module Mpst_js = Opstic_monadic.Make (ServerIo) (Opstic_js.ServerEndpoint)

  let accept_start_session :
      Server.t -> protocol:'b Mpst_js.Comm.inp protocol -> 'b ServerIo.t =
   fun server ~protocol ->
    let* conversation_id, role, http_session_id =
      Server.Mpst.accept_initial server
        ~entrypoint_id:protocol.entrypoint_spec.entrypoint_id ~kind:`AsLeader
        ~roles:protocol.entrypoint_spec.other_roles
    in
    let t = ServerEndpoint.create ~server ~protocol ~conversation_id in
    Hashtbl.replace t.sessions role (Some http_session_id);
    let inp = protocol.witness in
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
