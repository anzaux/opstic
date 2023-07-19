type pvar = string Location.loc
type var = string Location.loc
type role = string Location.loc
type label = string
type expr = Parsetree.expression
type endpoint = expr

type t_ =
  | MessageG : role * label * expr option * role * expr * t -> t_
  | Routed : role * label * expr * role * expr * role * expr * t -> t_
  | ChoiceG : role * t list -> t_
  | EndG : t_
  | ErrG : label -> t_
  | LetRecG : pvar * var list * t * t -> t_
  | CallG : pvar * expr list -> t_

and t_id = { body : t_; id : label }
and t = t_id Location.loc

val parse : Parsetree.expression -> t
val show : t -> string
