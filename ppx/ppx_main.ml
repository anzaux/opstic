open Ppxlib

let gtypes = ref []

let generate (var : string) (g : Gtype.t) : expression =
  gtypes := (var, g) :: !gtypes;
  let open Ast_builder.Default in
  let loc = g.loc in
  let err = pexp_extension ~loc @@ Location.error_extensionf ~loc "todo" in
  match g.txt.body with
  | LetRecG (_, _, _, _) -> estring ~loc (Gtype.show g)
  | MessageG (_, _, _, _, _, _) -> estring ~loc (Gtype.show g)
  | Routed (_, _, _, _, _, _, _, _) -> estring ~loc (Gtype.show g)
  | CallG (_, _) -> estring ~loc (Gtype.show g)
  | EndG -> estring ~loc (Gtype.show g)
  | ChoiceG (_, _) -> err
  | ErrG msg -> pexp_extension ~loc @@ Location.error_extensionf ~loc "%s" msg
(* | PureLet (_, _, _) -> estring ~loc (Gtype.show g) *)

let project gvar role =
  let g = List.assoc gvar !gtypes in
  let t = Ltype.project ~onto:role g in
  (* prerr_endline @@ "ltype: " ^ Ltype.show t; *)
  Codegen.make_witness gvar role t

let rule_gtype_def =
  Context_free.Rule.extension
  @@ Extension.declare "global" Extension.Context.Structure_item
       Ast_pattern.(
         pstr
           (pstr_value nonrecursive
              (value_binding ~pat:(as__ (ppat_var __)) ~expr:__ ^:: nil)
           ^:: nil))
  @@ fun ~loc ~path:_ pat var expr ->
  [%stri let [%p pat] = [%e generate var @@ Gtype.parse expr]]

let rule_project_global =
  Context_free.Rule.extension
  @@ Extension.declare "project_global" Extension.Context.Expression
       Ast_pattern.(
         pstr
           (pstr_eval
              (pexp_apply
                 (pexp_ident (lident __))
                 (pair nolabel (pexp_ident (lident __')) ^:: nil))
              nil
           ^:: nil))
       (fun ~loc:_ ~path:_ gvar role -> project gvar role)

let () =
  Driver.register_transformation
    ~rules:[ rule_gtype_def; rule_project_global ]
    "opstic.ppx"
