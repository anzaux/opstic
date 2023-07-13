open Types

type 'a ep = { ep_raw : ServerImpl.session; ep_witness : 'a Lin.t }
type 'a service_spec = { sv_spec : ServerImpl.service_spec; sv_witness : 'a }

type _ inp_label =
  | InpLabel : {
      label_constr : ('m, 'v * 'b ep) Rows.constr;
      parse_payload : Types.payload -> 'v io;
      cont : 'b witness lazy_t;
    }
      -> 'm inp_label

and _ inp_role =
  | InpRole : {
      role_constr : ('a, 'l) Rows.constr;
      path_spec : ServerImpl.path_spec;
      parse_label : Types.payload -> string io;
      labels : (string * 'l inp_label) list;
    }
      -> 'a inp_role

and 'a inp = (Role.t * 'a inp_role) list

and ('v, 'a) out = {
  out_role : Role.t;
  out_label : string;
  out_unparse : session_id -> string -> 'v -> Types.payload io;
  out_cont : 'a witness lazy_t;
}

and 'obj out_role_method =
  | Method : {
      role : 'obj -> 'm;
      label : 'm -> ('v, 'a) out;
    }
      -> 'obj out_role_method

and 'a witness

val create_service_spec :
  ?parse_session_id:(payload -> string io) ->
  id:string ->
  my_role:string ->
  other_roles:string list ->
  'a witness ->
  'a service_spec

val make_inp_label :
  constr:('a, 'v * 'c ep) Rows.constr ->
  parse:(Types.payload -> 'v io) ->
  'c witness lazy_t ->
  'a inp_label

val make_inp_role :
  ?path_kind:path_kind ->
  ?parse_label:(payload -> string io) ->
  path:path ->
  constr:('a, 'b) Rows.constr ->
  (string * 'b inp_label) list ->
  'a inp_role

val make_inp : 'a inp_role list -> 'a inp witness

val make_outcore :
  role:Role.t ->
  label:string ->
  unparse:(session_id -> string -> 'a -> Types.payload io) ->
  'b witness lazy_t ->
  ('a, 'b) out

val make_out : labels:'obj out_role_method list -> 'obj -> 'obj witness
val close : unit witness
val witness : 'a witness -> 'a
