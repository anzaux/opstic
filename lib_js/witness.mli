open Types

type raw_endpoint = ServerEndpoint.t
type payload = ServerEndpoint.payload
type 'a ep = { ep_raw : raw_endpoint; ep_witness : 'a Lin.t }

type 'm inp_choice =
  | InpChoice : {
      inp_choice_role : ('m, 'l) Rows.constr;
      inp_choice_label : ('l, 'v * 'b ep) Rows.constr;
      inp_choice_marshal : payload -> 'v;
      inp_choice_next_wit : 'b;
    }
      -> 'm inp_choice

type 'a inp = {
  inp_roles : string list;
  inp_subpath : string;
  inp_choices : (string * string, 'a inp_choice) Hashtbl.t;
  inp_kind : kind;
}
  constraint 'a = [> ]

val make_inp :
  ?kind:kind -> subpath:string -> ([> ] as 'a) inp_choice list -> 'a inp

type ('v, 'a) out = {
  out_role : string;
  out_label : string;
  out_marshal : 'v -> payload;
  out_next_wit : 'a;
  out_kind : kind;
}

val make_out :
  role:string ->
  label:string ->
  marshal:('a -> payload) ->
  ?kind:kind ->
  'b ->
  ('a, 'b) out
