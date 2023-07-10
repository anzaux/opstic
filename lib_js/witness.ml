open Types

type nonrec payload = payload
type 'a ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }

type _ inp_label =
  | InpLabel : {
      label_constr : ('m, 'v * 'b ep) Rows.constr;
      parse_payload : payload -> 'v;
      cont : 'b witness lazy_t;
    }
      -> 'm inp_label

and _ inp_role =
  | InpRole : {
      role_constr : ('a, 'l) Rows.constr;
      path : string;
      path_kind : path_kind;
      parse_label : payload -> string;
      labels : (string * 'l inp_label) list;
    }
      -> 'a inp_role

and 'a inp = (Role.t * 'a inp_role) list

and ('v, 'a) out = {
  out_role : Role.t;
  out_label : string;
  out_marshal : 'v -> payload;
  out_cont : 'a witness lazy_t;
}

and 'obj out_labels =
  | Method : {
      role : 'obj -> 'm;
      label : 'm -> ('v, 'a) out;
    }
      -> 'obj out_labels

and 'a witness =
  | Out : { obj : 'obj; labels : 'obj out_labels list } -> 'obj witness
  | Inp : 'a inp -> 'a inp witness
  | Close : unit witness

let make_inp_label ~constr ~label_constr cont =
  InpLabel { label_constr = constr; parse_payload = label_constr; cont }

let make_inp_role ?(path_kind = `Established) ~path ~constr ~parse_label labels
    =
  InpRole { path_kind; path; role_constr = constr; parse_label; labels }

type visited = string list (* paths *)

let rec pathspec_out :
    type obj. visited -> obj -> obj out_labels -> Server.path_spec list =
 fun visited obj (Method meth) ->
  let out = meth.label (meth.role obj) in
  to_pathspec_aux visited out.out_cont

and pathspec_inp_label : type l. visited -> l inp_label -> Server.path_spec list
    =
 fun visited (InpLabel inplab) -> to_pathspec_aux visited inplab.cont

and pathspec_inp_role :
    type var. visited -> var inp_role -> Server.path_spec list =
 fun visited (InpRole inp : var inp_role) ->
  if List.mem inp.path visited then []
  else
    let pathspec =
      Server.
        {
          path_role = Role.create inp.role_constr.constr_name;
          path = inp.path;
          path_kind = inp.path_kind;
        }
    in
    let pathspecs =
      List.map snd inp.labels
      |> List.concat_map (fun inplabel -> pathspec_inp_label visited inplabel)
    in
    pathspec :: pathspecs

and to_pathspec_aux :
    type a. visited -> a witness lazy_t -> Server.path_spec list =
 fun visited wit ->
  match Lazy.force wit with
  | Inp inp -> List.map snd inp |> List.concat_map (pathspec_inp_role visited)
  | Out out ->
      out.labels |> List.map (pathspec_out visited out.obj) |> List.concat
  | Close -> []

let to_pathspec x = to_pathspec_aux [] (Lazy.from_val x)

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
