type state_id = string list

type 't inp_label0 = {
  inp_label : string;
  inp_parse_payload : Parsetree.expression;
  inp_cont : 't;
}

and 't inp_role0 = {
  inp_role : string;
  inp_endpoint : Gtype.endpoint;
  inp_parse_label : Parsetree.expression;
  inp_labels : (string * 't inp_label0) list;
}

and 't out_label0 = {
  out_label : string;
  out_unparse : Parsetree.expression;
  out_cont : 't;
}

and 't out_role0 = {
  out_role : string;
  out_labels : (string * 't out_label0) list;
}

type t_ =
  | Inp of (string * t inp_role0) list
  | Out of (string * t out_role0) list
  | Close
  | Goto of state_id

and t = state_id * t_

type inp_role = t inp_role0
type inp_label = t inp_label0
type out_role = t out_role0
type out_label = t out_label0

val project : onto:Gtype.role -> Gtype.t -> t
