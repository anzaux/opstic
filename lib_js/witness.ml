open Types

type nonrec payload = payload
type 'a ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }

type 'm inp_choice =
  | InpChoice : {
      inp_choice_role : ('m, 'l) Rows.constr;
      inp_choice_label : ('l, 'v * 'b ep) Rows.constr;
      inp_choice_marshal : payload -> 'v;
      inp_choice_next_wit : 'b witness;
    }
      -> 'm inp_choice

and 'a inp = {
  inp_roles : string list;
  inp_subpath : string;
  inp_choices : (string * string, 'a inp_choice) Hashtbl.t;
  inp_kind : kind;
}

and ('v, 'a) out = {
  out_role : string;
  out_label : string;
  out_marshal : 'v -> payload;
  out_next_wit : 'a witness;
  out_kind : kind;
}

and 'obj method0 =
  | Method : { role : 'obj -> 'm; label : 'm -> ('v, 'a) out } -> 'obj method0

and 'a witness =
  | Out : { obj : 'obj; methods : 'obj method0 list } -> 'obj witness
  | Inp : 'a inp -> 'a inp witness
  | Close : unit witness

let make_inp ?(kind = `Established) ~subpath (xs : 'm inp_choice list) : 'm inp
    =
  let tbl = Hashtbl.create (List.length xs) in
  let rec put_all = function
    | (InpChoice c0 as c) :: xs ->
        Hashtbl.add tbl
          (c0.inp_choice_role.constr_name, c0.inp_choice_label.constr_name)
          c;
        put_all xs
    | [] -> ()
  in
  put_all xs;
  let uniq xs =
    let hash = Hashtbl.create (List.length xs) in
    xs |> List.iter (fun x -> Hashtbl.replace hash x ());
    Hashtbl.to_seq_keys hash |> List.of_seq
  in
  let roles =
    xs
    |> List.map (fun (InpChoice c0) -> c0.inp_choice_role.constr_name)
    |> uniq
  in
  {
    inp_roles = roles;
    inp_subpath = subpath;
    inp_kind = kind;
    inp_choices = tbl;
  }

let make_out ~role ~label ~marshal ?(kind = `Established) next =
  {
    out_role = role;
    out_label = label;
    out_marshal = marshal;
    out_next_wit = next;
    out_kind = kind;
  }

let witness : type a. a witness -> a = function
  | Out out -> out.obj
  | Inp inp -> inp
  | Close -> ()
