open Types

type nonrec payload = payload
type 'a ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }

type 'm inp_choice =
  | InpChoice : {
      inp_choice_role : ('m, 'l) Rows.constr;
      inp_choice_label : ('l, 'v * 'b ep) Rows.constr;
      inp_choice_marshal : payload -> 'v;
      inp_choice_next_wit : 'b witness;
    }
      -> 'm inp_choice

and 'a inp = {
  inp_roles : string list;
  inp_subpath : string;
  inp_choices : (string * string, 'a inp_choice) Hashtbl.t;
  inp_kind : kind;
}

and ('v, 'a) out = {
  out_role : string;
  out_label : string;
  out_marshal : 'v -> payload;
  out_next_wit : 'a witness;
  out_kind : kind;
}

and 'obj method0 =
  | Method : { role : 'obj -> 'm; label : 'm -> ('v, 'a) out } -> 'obj method0

and 'a witness =
  | Out : { obj : 'obj; methods : 'obj method0 list } -> 'obj witness
  | Inp : 'a inp -> 'a inp witness
  | Close : unit witness

val make_inp :
  ?kind:kind -> subpath:string -> ([> ] as 'a) inp_choice list -> 'a inp

val make_out :
  role:string ->
  label:string ->
  marshal:('a -> payload) ->
  ?kind:kind ->
  'b witness ->
  ('a, 'b) out

val witness : 'a witness -> 'a
