open Ppxlib

let generate (g : Gtype.t) : expression =
  let open Ast_builder.Default in
  let loc = g.loc in
  let err = pexp_extension ~loc @@ Location.error_extensionf ~loc "todo" in
  match g.txt with
  | LetRecG (_, _, _, _) -> estring ~loc (Gtype.show g)
  | MessageG (_, _, _, _, _) -> estring ~loc (Gtype.show g)
  (* | Routed (_, _, _, _, _, _) -> estring ~loc (Gtype.show g) *)
  | CallG (_, _) -> estring ~loc (Gtype.show g)
  | EndG -> estring ~loc (Gtype.show g)
  | ChoiceG (_, _) -> err
  | ErrG msg -> pexp_extension ~loc @@ Location.error_extensionf ~loc "%s" msg

let rule_gtype_expr =
  Context_free.Rule.extension
  @@ Extension.declare "global" Extension.Context.Expression
       Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
  @@ fun ~loc:_ ~path:_ expr -> generate @@ Gtype.parse expr

let rule_gtype_def =
  Context_free.Rule.extension
  @@ Extension.declare "global" Extension.Context.Structure_item
       Ast_pattern.(
         pstr
           (pstr_value nonrecursive
              (value_binding ~pat:(as__ (ppat_var drop)) ~expr:__ ^:: nil)
           ^:: nil))
  @@ fun ~loc ~path:_ pat expr ->
  [%stri let [%p pat] = [%e generate @@ Gtype.parse expr]]

let () =
  Driver.register_transformation
    ~rules:[ rule_gtype_def; rule_gtype_expr ]
    "opstic.ppx"
