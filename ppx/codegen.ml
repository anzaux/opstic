open Ppxlib
open Ast_helper

let gen_var state_id = "st_" ^ String.concat "_" state_id
let loc = Location.none

let gen_ident state_id =
  Exp.ident @@ { txt = Longident.parse (gen_var @@ state_id); loc }

let rec make_list : expression list -> expression = function
  | [] -> [%expr []]
  | e :: es -> [%expr [%e e] :: [%e make_list es]]

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
      {
        out_role = Role.create [%e Const.string role |> Exp.constant];
        out_label = [%e Const.string outlabel.out_label |> Exp.constant];
        out_unparse = [%e outlabel.out_unparse];
        out_cont = [%e gen_ident (fst outlabel.out_cont)];
      }]
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
      ~parse:[%e inplabel.inp_parse_payload]
      [%e gen_ident (fst inplabel.inp_cont)]]

let make_inp_role ((r, inprole) : string * Ltype.inp_role) =
  [%expr
    Witness.make_inp_role ~path_kind:`Greeting
      ~parse_label:(Witness.parse_label_default [ "args" ])
      ~path:(Path.create "/adder")
      ~constr:
        [%rows_make_constr [%e Exp.ident { txt = Longident.parse r; loc }]]
      [%e make_list (List.map make_inp_label inprole.inp_labels)]]

let make_inp (inp : (string * Ltype.inp_role) list) =
  [%expr lazy (Witness.make_inp [%e make_list (List.map make_inp_role inp)])]

let make_close = [%expr Lazy.from_val Witness.Close]

let rec gen_binding : seen:Ltype.state_id list -> Ltype.t -> value_binding list
    =
 fun ~seen (state_id, t) ->
  if List.mem state_id seen then []
  else
    let seen = state_id :: seen in
    let mkbind exp = Vb.mk (Pat.var { txt = gen_var state_id; loc }) exp in
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
    | Goto goto_id -> [ mkbind (gen_ident goto_id) ]

let make_witness : Ltype.t -> expression =
 fun ((state_id, _) as t) ->
  Exp.let_ Recursive (gen_binding ~seen:[] t) (gen_ident state_id)
