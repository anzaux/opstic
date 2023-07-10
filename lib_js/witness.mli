type 'a ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }

type _ inp_label =
  | InpLabel : {
      inp_label_constr : ('m, 'v * 'b ep) Rows.constr;
      inp_label_parse_payload : Types.payload -> 'v;
      inp_label_cont : 'b witness lazy_t;
    }
      -> 'm inp_label

and _ inp_role =
  | InpRole : {
      inp_role_constr : ('a, 'l) Rows.constr;
      inp_role_path : string;
      inp_role_path_kind : Types.path_kind;
      inp_role_parse_label : Types.payload -> string;
      inp_role_labels : (string * 'l inp_label) list;
    }
      -> 'a inp_role

and 'a inp = (string * 'a inp_role) list

and ('v, 'a) out = {
  out_role : string;
  out_label : string;
  out_marshal : 'v -> Types.payload;
  out_cont : 'a witness lazy_t;
}

and 'obj method0 =
  | Method : { role : 'obj -> 'm; label : 'm -> ('v, 'a) out } -> 'obj method0

and 'a witness =
  | Out : { obj : 'obj; methods : 'obj method0 list } -> 'obj witness
  | Inp : 'a inp -> 'a inp witness
  | Close : unit witness

val to_subspec : 'a witness lazy_t -> Server.path_spec list

val make_inp_label :
  constr:('a, 'b * 'c ep) Rows.constr ->
  parse_payload:(Types.payload -> 'b) ->
  'c witness lazy_t ->
  'a inp_label

val make_inp_role :
  ?path_kind:Types.path_kind ->
  path:string ->
  constr:('b, 'c) Rows.constr ->
  parse_label:(Types.payload -> string) ->
  (string * 'c inp_label) list ->
  'b inp_role

val make_out :
  role:string ->
  label:string ->
  marshal:('a -> Types.payload) ->
  'b witness lazy_t ->
  ('a, 'b) out

val witness : 'a witness -> 'a
