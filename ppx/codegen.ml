open Ppxlib
open Ast_helper

let gen_stateid_var state_id = "st_" ^ String.concat "_" state_id
let loc = Location.none

let gen_ident state_id =
  Exp.ident @@ { txt = Longident.parse (gen_stateid_var @@ state_id); loc }

let genvar =
  let cnt = ref 0 in
  fun () ->
    let n = !cnt in
    cnt := n + 1;
    "v" ^ string_of_int n

let rec make_list : expression list -> expression = function
  | [] -> [%expr []]
  | e :: es -> [%expr [%e e] :: [%e make_list es]]

let rec make_parser_aux (exp : expression) :
    string list * (pattern * expression) =
  let open Ast_helper in
  match exp with
  | { pexp_desc = Pexp_constant c; _ } -> ([], (Pat.constant c, exp))
  | { pexp_desc = Pexp_variant (c, Some arg); _ } ->
      let vars, (pat, arg) = make_parser_aux arg in
      ( vars,
        ( Pat.variant c (Some pat),
          { exp with pexp_desc = Pexp_variant (c, Some arg) } ) )
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident "()"; _ }, _); _ } ->
      ([], (Pat.any (), [%expr `obj []]))
  | { pexp_desc = Pexp_construct (c, arg); _ } ->
      let vars, (pat, arg) =
        match arg with
        | None -> ([], (Pat.construct c None, None))
        | Some arg ->
            let vars, (pat, arg) = make_parser_aux arg in
            (vars, (Pat.construct c (Some pat), Some arg))
      in
      (vars, (pat, { exp with pexp_desc = Pexp_construct (c, arg) }))
  | { pexp_desc = Pexp_tuple es; _ } ->
      let vars, patargs = List.split @@ List.map make_parser_aux es in
      let pats, args = List.split patargs in
      ( List.concat vars,
        (Pat.tuple pats, { exp with pexp_desc = Pexp_tuple args }) )
  | [%expr session_id] ->
      ( [ ],
        ( [%pat? session_id],
          exp ) )
  | [%expr __] ->
      let var = genvar () in
      ( [ var ],
        ( Pat.var { txt = var; loc },
          Exp.ident { txt = Longident.parse var; loc } ) )
  | _ ->
      failwith
        (Format.asprintf "cannot handle message: %a" Pprintast.expression exp)

let make_unparser exp =
  let open Ast_helper in
  let vars, (_, exp) = make_parser_aux exp in
  let pat =
    if List.length vars = 0 then [%pat? ()]
    else if List.length vars = 1 then Pat.var { txt = List.hd vars; loc }
    else Pat.tuple (List.map (fun var -> Pat.var { txt = var; loc }) vars)
  in
  [%expr
    fun session_id _label ->
      [%e Exp.fun_ Nolabel None pat [%expr Monad.return [%e exp]]]]

let make_parser exp =
  let open Ast_helper in
  let vars, (pat, _) = make_parser_aux exp in
  let tup =
    if List.length vars = 0 then [%expr ()]
    else if List.length vars = 1 then
      Exp.ident { txt = Longident.parse (List.hd vars); loc }
    else
      Exp.tuple
        (List.map
           (fun var -> Exp.ident { txt = Longident.parse var; loc })
           vars)
  in
  let dflt =
    match pat.ppat_desc with
    | Ppat_any -> []
    | _ -> [ Exp.case (Pat.any ()) [%expr Monad.error_with "can't parse"] ]
  in
  Exp.function_ (Exp.case pat [%expr Monad.return [%e tup]] :: dflt)

let make_meta_method role (outlabel : Ltype.out_label) =
  [%expr
    Method
      {
        role = (fun x -> [%e Exp.send [%expr x] { txt = role; loc }]);
        label =
          (fun x -> [%e Exp.send [%expr x] { txt = outlabel.out_label; loc }]);
      }]

let make_meta_methods0 ((r, outrole) : string * Ltype.out_role) =
  List.map (fun (_, outlabel) -> make_meta_method r outlabel) outrole.out_labels

let make_meta_methods out = List.concat_map make_meta_methods0 out |> make_list

let make_label_method role (outlabel : Ltype.out_label) =
  let exp =
    [%expr
      Witness.make_outcore
        ~role:(Role.create [%e Const.string role |> Exp.constant])
        ~label:[%e Const.string outlabel.out_label |> Exp.constant]
        ~unparse:[%e make_unparser outlabel.out_unparser]
        [%e gen_ident (fst outlabel.out_cont)]]
  in
  Cf.method_
    { txt = outlabel.out_label; loc }
    Public
    (Cfk_concrete (Fresh, exp))

let make_role_method (outrole : Ltype.out_role) =
  let label_methods =
    List.map
      (fun (_, outlabel) -> make_label_method outrole.out_role outlabel)
      outrole.out_labels
  in
  let labobj = Exp.object_ (Cstr.mk (Pat.any ()) label_methods) in
  Cf.method_
    { txt = outrole.out_role; loc }
    Public
    (Cfk_concrete (Fresh, labobj))

let make_out_obj out =
  let role_methods =
    List.map (fun (_, outrole) -> make_role_method outrole) out
  in
  Exp.object_ (Cstr.mk (Pat.any ()) role_methods)

let make_out (out : (string * Ltype.out_role) list) =
  [%expr
    lazy
      (Witness.make_out ~labels:[%e make_meta_methods out] [%e make_out_obj out])]

let make_inp_label ((l, inplabel) : string * Ltype.inp_label) =
  [%expr
    Witness.make_inp_label
      ~constr:
        [%rows_make_constr [%e Exp.ident { txt = Longident.parse l; loc }]]
      ~parse:[%e make_parser inplabel.inp_parser]
      [%e gen_ident (fst inplabel.inp_cont)]]

let make_inp_role ((r, inprole) : string * Ltype.inp_role) =
  let labels =
    List.map fst inprole.inp_labels
    |> List.map (fun x -> x |> Const.string |> Exp.constant)
  in
  [%expr
    Witness.make_inp_role ~path_kind:`Greeting
      ~parse_label:(Witness.parse_label_default [%e make_list labels])
      ~path:(Path.create [%e inprole.inp_endpoint])
      ~constr:
        [%rows_make_constr [%e Exp.ident { txt = Longident.parse r; loc }]]
      [%e make_list (List.map make_inp_label inprole.inp_labels)]]

let make_inp (inp : (string * Ltype.inp_role) list) =
  [%expr lazy (Witness.make_inp [%e make_list (List.map make_inp_role inp)])]

let make_close = [%expr lazy Witness.close]

let rec gen_binding : seen:Ltype.state_id list -> Ltype.t -> value_binding list
    =
 fun ~seen (state_id, t) ->
  if List.mem state_id seen then []
  else
    let seen = state_id :: seen in
    let mkbind exp =
      Vb.mk (Pat.var { txt = gen_stateid_var state_id; loc }) exp
    in
    match t with
    | Out out ->
        let proc_label (_, lab) = gen_binding ~seen lab.Ltype.out_cont in
        let proc_role (_, role) =
          List.concat_map proc_label role.Ltype.out_labels
        in
        mkbind (make_out out) :: List.concat_map proc_role out
    | Inp inp ->
        let proc_label (_, lab) = gen_binding ~seen lab.Ltype.inp_cont in
        let proc_role (_, role) =
          List.concat_map proc_label role.Ltype.inp_labels
        in
        mkbind (make_inp inp) :: List.concat_map proc_role inp
    | Close -> [ mkbind make_close ]
    | Goto goto_id ->
        [ mkbind [%expr lazy (Lazy.force [%e gen_ident goto_id])] ]

let make_witness : string -> Gtype.role -> Ltype.t -> expression =
 fun service_id role ((state_id, _) as t) ->
  let service_id = Exp.constant @@ Const.string service_id in
  let my_role = Exp.constant @@ Const.string role.txt in
  let other_roles = Ltype.roles t |> List.filter (fun r -> r <> role.txt) in
  let other_roles =
    List.map (fun r -> Exp.constant @@ Const.string r) other_roles |> make_list
  in
  let wit = Exp.let_ Recursive (gen_binding ~seen:[] t) (gen_ident state_id) in
  [%expr
    let wit = [%e wit] in
    Witness.create_service_spec ~id:[%e service_id] ~my_role:[%e my_role]
      ~other_roles:[%e other_roles] (Lazy.force wit)]
