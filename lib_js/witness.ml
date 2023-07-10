open Types

type nonrec payload = payload
type 'a ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }

type _ inp_label =
  | InpLabel : {
      inp_label_constr : ('m, 'v * 'b ep) Rows.constr;
      inp_label_parse_payload : payload -> 'v;
      inp_label_cont : 'b witness lazy_t;
    }
      -> 'm inp_label

and _ inp_role =
  | InpRole : {
      inp_role_constr : ('a, 'l) Rows.constr;
      inp_role_path : string;
      inp_role_path_kind : path_kind;
      inp_role_parse_label : payload -> string;
      inp_role_labels : (string * 'l inp_label) list;
    }
      -> 'a inp_role

and 'a inp = (string * 'a inp_role) list

and ('v, 'a) out = {
  out_role : string;
  out_label : string;
  out_marshal : 'v -> payload;
  out_cont : 'a witness lazy_t;
}

and 'obj method0 =
  | Method : { role : 'obj -> 'm; label : 'm -> ('v, 'a) out } -> 'obj method0

and 'a witness =
  | Out : { obj : 'obj; methods : 'obj method0 list } -> 'obj witness
  | Inp : 'a inp -> 'a inp witness
  | Close : unit witness

let make_inp_label ~constr ~parse_payload cont =
  InpLabel
    {
      inp_label_constr = constr;
      inp_label_parse_payload = parse_payload;
      inp_label_cont = cont;
    }

let make_inp_role ?(path_kind = `Established) ~path ~constr ~parse_label labels
    =
  InpRole
    {
      inp_role_path_kind = path_kind;
      inp_role_path = path;
      inp_role_constr = constr;
      inp_role_parse_label = parse_label;
      inp_role_labels = labels;
    }

type visited = string list (* paths *)

let rec subspec_out :
    type obj. visited -> obj -> obj method0 -> Server.path_spec list =
 fun visited obj (Method meth) ->
  let out = meth.label (meth.role obj) in
  to_subspec_aux visited out.out_cont

and subspec_inp_label : type l. visited -> l inp_label -> Server.path_spec list
    =
 fun visited (InpLabel inplab) -> to_subspec_aux visited inplab.inp_label_cont

and subspec_inp_role :
    type var. visited -> var inp_role -> Server.path_spec list =
 fun visited (InpRole inp : var inp_role) ->
  if List.mem inp.inp_role_path visited then []
  else
    let subspec =
      Server.
        {
          path_role = Role.create inp.inp_role_constr.constr_name;
          path = inp.inp_role_path;
          path_kind = inp.inp_role_path_kind;
        }
    in
    let subspecs =
      List.map snd inp.inp_role_labels
      |> List.concat_map (fun inplabel -> subspec_inp_label visited inplabel)
    in
    subspec :: subspecs

and to_subspec_aux :
    type a. visited -> a witness lazy_t -> Server.path_spec list =
 fun visited wit ->
  match Lazy.force wit with
  | Inp inp -> List.map snd inp |> List.concat_map (subspec_inp_role visited)
  | Out out ->
      out.methods |> List.map (subspec_out visited out.obj) |> List.concat
  | Close -> []

let to_subspec x = to_subspec_aux [] x

(* let get_label_default payload =
   match Kxclib.Jv.pump_field "label" payload with
   | `obj (("label", `str lab) :: _) -> lab
   | _ -> failwith "no label" *)

let make_out ~role ~label ~marshal next =
  { out_role = role; out_label = label; out_marshal = marshal; out_cont = next }

let witness : type a. a witness -> a = function
  | Out out -> out.obj
  | Inp inp -> inp
  | Close -> ()
