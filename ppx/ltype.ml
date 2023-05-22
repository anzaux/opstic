open Gtype

type t =
  | RecvL of label * role * t
  | SendL of label * role * t
  | ChoiceL of role * t list
  | TVarL of pvar * Expr.t list
  | MuL of pvar * (bool * Gtype.rec_var) list * t
  | EndL
