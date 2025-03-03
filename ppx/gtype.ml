open Ppxlib

let pp_loc (inner : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (x : 'a loc) : unit =
  inner fmt x.txt

let pp_expr = Pprintast.expression

type pvar = string loc [@@deriving show]
type var = string loc [@@deriving show]
type role = string loc [@@deriving show]
type label = string [@@deriving show]
type expr = Ast.expression
type endpoint = expr [@@deriving show]

type t_ =
  | MessageG : role * label * endpoint option * role * expr * t -> t_
  (* a#lab ==> b :: expr >> gtyp *)
  | Routed : role * label * endpoint * role * endpoint * role * expr * t -> t_
  (* a#lab = c ==> b :: expr >> gtyp *)
  | ChoiceG : role * t list -> t_ (* a *>> (gtyp1, gtyp2, ..) *)
  | EndG : t_ (* end *)
  | ErrG : string -> t_
  | LetRecG : pvar * var list * t * t -> t_ (* let rec f x y = e1 in e2 *)
  | CallG : pvar * expr list -> t_ (* f e1 e2 .. *)
(* | PureLet : var * expr * t -> t_ *)
[@@deriving show]

and t_id = { body : t_; id : string }
and t = t_id loc

let fresh =
  let cnt = ref 0 in
  let gen () =
    let n = !cnt in
    cnt := n + 1;
    "g" ^ string_of_int n
  in
  fun t_ -> { id = gen (); body = t_ }

let named ~name t_ = { id = name; body = t_ }

let err_ ?(debug = "") exp : t_id =
  fresh
  @@ ErrG
       ("unsupported payload " ^ debug ^ ": "
       ^ Format.asprintf "%a" Pprintast.expression exp)

let err ?debug exp = { txt = err_ ?debug exp; loc = exp.pexp_loc }
let end_ ~loc : t = { txt = fresh EndG; loc }
let show x = show_t_ x.txt.body

let role_of_exp exp =
  Ast_pattern.(parse (pexp_ident (lident __'))) exp.pexp_loc exp (fun x -> x)

let role_label_of_exp exp =
  (* split `r#label` into (r,label) *)
  Ast_pattern.(parse (pexp_send (pexp_ident (lident __')) __)) exp.pexp_loc exp
    (fun r lbl -> (r, lbl))

let rec parse (exp : expression) : t =
  match exp with
  | [%expr ()] -> { txt = fresh EndG; loc = exp.pexp_loc }
  | [%expr 0] -> { txt = fresh EndG; loc = exp.pexp_loc }
  | [%expr
      [%e? e];
      [%e? g]] ->
      (* sequencing *)
      parse_gtype_prefixed e (Some g)
  | [%expr
      let rec [%p? pat] = [%e? e1] in
      [%e? e2]] ->
      (* recursive definition:
         let rec pat = fun x1 ... xn -> e1 in e2 *)
      let id =
        Ast_pattern.(parse (ppat_var __') pat.ppat_loc pat (fun x -> x))
      in
      let vars, g1 = parse_gtype_defbody e1 in
      let g2 = parse e2 in
      {
        txt = named ~name:id.txt @@ LetRecG (id, vars, g1, g2);
        loc = exp.pexp_loc;
      }
  | { pexp_desc = Pexp_ident _; _ } ->
      (* recursion (variable) *)
      { txt = parse_gtype_call exp []; loc = exp.pexp_loc }
  | { pexp_desc = Pexp_apply (_, _); _ } -> (
      match exp with
      | [%expr [%e? r] *>> [%e? alts]] ->
          (* choice *)
          let r = role_of_exp r in
          let alts =
            Ast_pattern.(parse (pexp_tuple __) exp.pexp_loc alts (fun x -> x))
          in
          let alts = List.map parse alts in
          { txt = fresh @@ ChoiceG (r, alts); loc = exp.pexp_loc }
      | e ->
          (* prefix without sequencing, or recursive call.
             e.g.,
             (r1#lbl) ==> (r2::payload)
             or
             (r1#lbl) = (s) ==> (r2::payload)
             or
             f e1 e2 ... en
          *)
          parse_gtype_prefixed e None)
  (* | [%expr
      let [%p? pat] = [%e? e1] in
      [%e? e2]] ->
      let id =
        Ast_pattern.(parse (ppat_var __') pat.ppat_loc pat (fun x -> x))
      in
      { txt = fresh @@ PureLet (id, e1, parse e2); loc = exp.pexp_loc } *)
  | _ -> err ~debug:"(gtype)" exp

and parse_gtype_prefixed (exp : expression) (cont : expression option) : t =
  let parse_cont () =
    match cont with None -> end_ ~loc:Location.none | Some cont -> parse cont
  in
  let g_ =
    match exp with
    | [%expr
        [%e? r1_lbl]
        = ([%e? e1], [%e? s], [%e? e2])
        => [%e? r2] :: [%e? payload]] ->
        (* r1 # lbl = s => r2 :: payload *)
        let r1, lbl = role_label_of_exp r1_lbl in
        let s = role_of_exp s in
        let r2 = role_of_exp r2 in
        fresh @@ Routed (r1, lbl, e1, s, e2, r2, payload, parse_cont ())
    | [%expr [%e? r1_lbl] = [%e? ep] => [%e? r2] :: [%e? payload]] ->
        (* r1 # lbl ==> r2 :: payload *)
        let r1, lbl = role_label_of_exp r1_lbl in
        let r2 = role_of_exp r2 in
        fresh @@ MessageG (r1, lbl, Some ep, r2, payload, parse_cont ())
    | [%expr [%e? r1_lbl] ==> [%e? r2] :: [%e? payload]] ->
        (* r1 # lbl ==> r2 :: payload *)
        let r1, lbl = role_label_of_exp r1_lbl in
        let r2 = role_of_exp r2 in
        fresh @@ MessageG (r1, lbl, None, r2, payload, parse_cont ())
    | { pexp_desc = Pexp_apply (body, args); _ } ->
        let args = List.rev_map (fun (_, arg) -> arg) args in
        parse_gtype_call body args
    | _ -> err_ ~debug:"(prefix)" exp
  in
  { txt = g_; loc = exp.pexp_loc }

and parse_gtype_defbody (exp : expression) : var list * t =
  match exp with
  | [%expr fun [%p? pat] -> [%e? body]] ->
      let pat =
        Ast_pattern.(parse (ppat_var __') pat.ppat_loc pat (fun x -> x))
      in
      let pats, body = parse_gtype_defbody body in
      (pat :: pats, body)
  | _ -> ([], parse exp)

and parse_gtype_call (funbody : expression) (funarg_rev : expression list) :
    t_id =
  match funbody with
  | { pexp_desc = Pexp_ident lid; _ } ->
      let var = Ast_pattern.(parse (lident __) lid.loc lid.txt (fun x -> x)) in
      named ~name:var
      @@ CallG ({ txt = var; loc = lid.loc }, List.rev funarg_rev)
  | [%expr [%e? e1] [%e? e2]] -> parse_gtype_call e1 (e2 :: funarg_rev)
  | _ -> err_ ~debug:"(call)" funbody
