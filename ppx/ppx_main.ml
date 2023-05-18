open Ppxlib

let pp_loc (inner : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (x : 'a loc) : unit =
  inner fmt x.txt

type longident = [%import: Longident.t] [@@deriving show]

let pp_expr = Pprintast.expression

type pvar = string loc [@@deriving show]
type var = string loc [@@deriving show]
type role = string loc [@@deriving show]
type lbl = string [@@deriving show]
type expr = Ast.expression

type gtype_ =
  (* (µF(vars).gtyp)<exprs>*)
  (* | Fix : pvar * var list * gtype * expr list -> gtype_ *)
  | LetRec : pvar * var list * gtype * gtype -> gtype_
  | Direct :
      role * lbl * role * expr * gtype
      -> gtype_ (* a#lab ==> b :: expr >> gtyp *)
  | Routed :
      role * lbl * role * role * expr * gtype
      -> gtype_ (* a#lab = c ==> b :: expr >> gtyp *)
  | Choice : role * gtype list -> gtype_ (* a >>? [gtyp1; gtyp2; ..]*)
  | Call : pvar * expr list -> gtype_ (* F<exprs> *)
  | End : gtype_ (* end *)
  | Err : string -> gtype_
[@@deriving show]

and gtype = gtype_ loc

let gtype_err_ ?(debug = "") exp : gtype_ =
  Err
    ("unsupported payload " ^ debug ^ ": "
    ^ Format.asprintf "%a" Pprintast.expression exp)

let gtype_err ?debug exp = { txt = gtype_err_ ?debug exp; loc = exp.pexp_loc }
let gtype_end ~loc : gtype = { txt = End; loc }

let role_of_exp exp =
  Ast_pattern.(parse (pexp_ident (lident __'))) exp.pexp_loc exp (fun x -> x)

let role_label_of_exp exp =
  (* split `r#lbl` into (r,lbl) *)
  Ast_pattern.(parse (pexp_send (pexp_ident (lident __')) __)) exp.pexp_loc exp
    (fun r lbl -> (r, lbl))

let rec parse_gtype (exp : expression) : gtype =
  match exp with
  | [%expr ()] -> { txt = End; loc = exp.pexp_loc }
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
      let vars, g1 = parse_gtype_defbody e1 in
      let g2 = parse_gtype e2 in
      let id =
        Ast_pattern.(parse (ppat_var __') pat.ppat_loc pat (fun x -> x))
      in
      { txt = LetRec (id, vars, g1, g2); loc = exp.pexp_loc }
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
          let alts = List.map parse_gtype alts in
          { txt = Choice (r, alts); loc = exp.pexp_loc }
      | e ->
          (* prefix without continuation, or recursive call.
             e.g.,
             (r1#lbl) ==> (r2::payload)
             or
             (r1#lbl) = s) ==> (r2::payload)
             or
             f e1 e2 ... en
          *)
          parse_gtype_prefixed e None)
  | _ -> gtype_err ~debug:"(gtype)" exp

and parse_gtype_prefixed (exp : expression) (cont : expression option) : gtype =
  let parse_cont () =
    match cont with
    | None -> gtype_end ~loc:Location.none
    | Some cont -> parse_gtype cont
  in
  let g_ =
    match exp with
    | [%expr [%e? r1_lbl] ==> [%e? r2] :: [%e? payload]] ->
        (* r1 # lbl ==> r2 :: payload *)
        let r1, lbl = role_label_of_exp r1_lbl in
        let r2 = role_of_exp r2 in
        Direct (r1, lbl, r2, payload, parse_cont ())
    | [%expr [%e? r1_lbl] = [%e? s] => [%e? r2] :: [%e? payload]] ->
        (* r1 # lbl = s => r2 :: payload *)
        let r1, lbl = role_label_of_exp r1_lbl in
        let s = role_of_exp s in
        let r2 = role_of_exp r2 in
        Routed (r1, lbl, s, r2, payload, parse_cont ())
    | { pexp_desc = Pexp_apply (body, args); _ } ->
        let args = List.rev_map (fun (_, arg) -> arg) args in
        parse_gtype_call body args
    | _ -> gtype_err_ ~debug:"(prefix)" exp
  in
  { txt = g_; loc = exp.pexp_loc }

and parse_gtype_defbody (exp : expression) : var list * gtype =
  match exp with
  | [%expr fun [%p? pat] -> [%e? body]] ->
      let pat =
        Ast_pattern.(parse (ppat_var __') pat.ppat_loc pat (fun x -> x))
      in
      let pats, body = parse_gtype_defbody body in
      (pat :: pats, body)
  | _ -> ([], parse_gtype exp)

and parse_gtype_call (funbody : expression) (funarg_rev : expression list) :
    gtype_ =
  match funbody with
  | { pexp_desc = Pexp_ident lid; _ } ->
      let var = Ast_pattern.(parse (lident __) lid.loc lid.txt (fun x -> x)) in
      Call ({ txt = var; loc = lid.loc }, List.rev funarg_rev)
  | [%expr [%e? e1] [%e? e2]] -> parse_gtype_call e1 (e2 :: funarg_rev)
  | _ -> gtype_err_ ~debug:"(call)" funbody

let generate (g : gtype) : expression =
  let open Ast_builder.Default in
  let loc = g.loc in
  let err = pexp_extension ~loc @@ Location.error_extensionf ~loc "todo" in
  match g.txt with
  | LetRec (_, _, _, _) -> estring ~loc (show_gtype g)
  | Direct (_, _, _, _, _) -> estring ~loc (show_gtype g)
  | Routed (_, _, _, _, _, _) -> estring ~loc (show_gtype g)
  | Call (_, _) -> estring ~loc (show_gtype g)
  | End -> estring ~loc (show_gtype g)
  | Choice (_, _) -> err
  | Err msg -> pexp_extension ~loc @@ Location.error_extensionf ~loc "%s" msg

let rule_gtype_expr =
  Context_free.Rule.extension
  @@ Extension.declare "global" Extension.Context.Expression
       Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
  @@ fun ~loc:_ ~path:_ expr -> generate @@ parse_gtype expr

let rule_gtype_def =
  Context_free.Rule.extension
  @@ Extension.declare "global" Extension.Context.Structure_item
       Ast_pattern.(
         pstr
           (pstr_value nonrecursive
              (value_binding ~pat:(as__ (ppat_var drop)) ~expr:__ ^:: nil)
           ^:: nil))
  @@ fun ~loc ~path:_ pat expr ->
  [%stri let [%p pat] = [%e generate @@ parse_gtype expr]]

let () =
  Driver.register_transformation
    ~rules:[ rule_gtype_def; rule_gtype_expr ]
    "ojmpst.ppx"
