open Types

type 'a ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }

type _ inp_label =
  | InpLabel : {
      label_constr : ('m, 'v * 'b ep) Rows.constr;
      parse_payload : Types.payload -> 'v;
      cont : 'b witness lazy_t;
    }
      -> 'm inp_label

and _ inp_role =
  | InpRole : {
      role_constr : ('a, 'l) Rows.constr;
      path_spec : Server.path_spec;
      parse_label : Types.payload -> string;
      labels : (string * 'l inp_label) list;
    }
      -> 'a inp_role

and 'a inp = (Role.t * 'a inp_role) list

and ('v, 'a) out = {
  out_role : Role.t;
  out_label : string;
  out_marshal : 'v -> Types.payload;
  out_cont : 'a witness lazy_t;
}

and 'obj out_labels =
  | Method : {
      role : 'obj -> 'm;
      label : 'm -> ('v, 'a) out;
    }
      -> 'obj out_labels

and 'a witness =
  | Out : { obj : 'obj; labels : 'obj out_labels list } -> 'obj witness
  | Inp : 'a inp -> 'a inp witness
  | Close : unit witness

type 'a service_spec = {
  sv_spec : Server.service_spec;
  sv_witness : 'a witness;
}

val to_pathspec : 'a witness -> Server.path_spec list

val create_service :
  ?parse_session_id:(payload -> string) ->
  id:string ->
  my_role:string ->
  other_roles:string list ->
  'a witness ->
  'a service_spec

val make_inp_label :
  constr:('a, 'b * 'c ep) Rows.constr ->
  label_constr:(Types.payload -> 'b) ->
  'c witness lazy_t ->
  'a inp_label

val make_inp_role :
  ?path_kind:path_kind ->
  ?parse_label:(payload -> string) ->
  path:path ->
  constr:('a, 'b) Rows.constr ->
  (string * 'b inp_label) list ->
  'a inp_role

val make_inp : 'a inp_role list -> 'a inp witness

val make_out :
  role:Role.t ->
  label:string ->
  marshal:('a -> Types.payload) ->
  'b witness lazy_t ->
  ('a, 'b) out

val witness : 'a witness -> 'a
