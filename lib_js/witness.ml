open Types
open Server

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
      path_spec : Server.path_spec;
      parse_label : payload -> string;
      labels : (string * 'l inp_label) list;
    }
      -> 'a inp_role

and 'a inp = (Role.t * 'a inp_role) list

and ('v, 'a) out = {
  out_role : Role.t;
  out_label : string;
  out_unparse : session_id -> string -> 'v -> payload;
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

type 'a service_spec = { sv_spec : Server.service_spec; sv_witness : 'a }

let parse_label_default payload =
  match Kxclib.Jv.pump_field "label" payload with
  | `obj (("label", `str lab) :: _) -> lab
  | _ -> failwith "no label"

let parse_sessionid_default payload =
  match Kxclib.Jv.pump_field "session_id" payload with
  | `obj (("session_id", `str lab) :: _) -> lab
  | _ -> failwith "no label"

let make_inp_label ~constr ~parse cont =
  InpLabel { label_constr = constr; parse_payload = parse; cont }

let make_inp_role ?(path_kind = `Established)
    ?(parse_label = parse_label_default) ~path ~constr labels =
  InpRole
    {
      path_spec =
        { path_kind; path; path_role = Role.create constr.constr_name };
      role_constr = constr;
      parse_label;
      labels;
    }

let make_inp inproles : 'a inp witness =
  Inp
    (List.map
       (fun (InpRole inprole as i) ->
         (Role.create inprole.role_constr.constr_name, i))
       inproles)

type visited = path list (* paths *)

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
  if List.mem inp.path_spec.path visited then []
  else
    let pathspecs =
      List.map snd inp.labels
      |> List.concat_map (fun inplabel -> pathspec_inp_label visited inplabel)
    in
    inp.path_spec :: pathspecs

and to_pathspec_aux :
    type a. visited -> a witness lazy_t -> Server.path_spec list =
 fun visited wit ->
  match Lazy.force wit with
  | Inp inp -> List.map snd inp |> List.concat_map (pathspec_inp_role visited)
  | Out out ->
      out.labels |> List.map (pathspec_out visited out.obj) |> List.concat
  | Close -> []

let get_witness : type a. a witness -> a = function
  | Inp inp -> inp
  | Out out -> out.obj
  | Close -> ()

let to_pathspec x = to_pathspec_aux [] (Lazy.from_val x)

let create_service_spec :
    ?parse_session_id:(payload -> string) ->
    id:string ->
    my_role:string ->
    other_roles:string list ->
    'a witness ->
    'a service_spec =
 fun ?(parse_session_id = parse_sessionid_default) ~id ~my_role ~other_roles
     witness ->
  let my_role = Role.create my_role in
  let other_roles = List.map Role.create other_roles in
  let path_specs0 = to_pathspec witness in
  let path_specs = Hashtbl.create 42 in
  path_specs0
  |> List.iter (fun path_spec ->
         Hashtbl.replace path_specs path_spec.path path_spec);
  let spec =
    {
      service_id = ServiceId.create id;
      path_specs;
      my_role;
      other_roles;
      parse_session_id =
        (fun payload -> SessionId.create (parse_session_id payload));
    }
  in
  { sv_spec = spec; sv_witness = get_witness witness }

let make_out ~role ~label ~unparse next =
  { out_role = role; out_label = label; out_unparse = unparse; out_cont = next }

let witness : type a. a witness -> a = function
  | Out out -> out.obj
  | Inp inp -> inp
  | Close -> ()
