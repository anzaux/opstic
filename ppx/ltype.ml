open Ppxlib

type state_id = string list

type 't inp_label0 = {
  inp_label : string;
  inp_parser : expression;
  inp_cont : 't;
}

and 't inp_role0 = {
  inp_role : string;
  inp_endpoint : Gtype.endpoint;
  inp_labels : (string * 't inp_label0) list;
}

and 't out_label0 = {
  out_label : string;
  out_unparser : expression;
  out_cont : 't;
}

and 't out_role0 = {
  out_role : string;
  out_labels : (string * 't out_label0) list;
}

type inp_role1 = nondet inp_role0
and out_role1 = nondet out_role0

(* deterministic transition *)
and det =
  | DetInp of (string * inp_role1) list
  | DetOut of (string * out_role1) list
  | DetClose

and nondet =
  | Det of state_id * det lazy_t
  | Merge of nondet list
  | Concat of nondet list
  | Lazy of nondet lazy_t

exception UnguardedLoop

type context = (state_id * det lazy_t) list ref

let union_ids xs ys =
  let module M = Map.Make (struct
    type t = string

    let compare = String.compare
  end) in
  let add_all m xs = List.fold_left (fun m x -> M.add x () m) m xs in
  let m = add_all M.empty xs in
  let m = add_all m ys in
  m |> M.to_seq |> Seq.map fst |> List.of_seq

let make_out_one (r : Gtype.role) (lab : Gtype.label) (msg : expression)
    (cont : nondet) : out_role1 =
  {
    out_role = r.txt;
    out_labels =
      [
        ( lab,
          {
            out_label = lab;
            out_unparser = msg;
            out_cont = cont;
          } );
      ];
  }

let make_out (state_id : state_id) (r : Gtype.role) (lab : Gtype.label)
    (msg : expression) (cont : nondet) =
  Det
    (state_id, Lazy.from_val @@ DetOut [ (r.txt, make_out_one r lab msg cont) ])

let make_inp_one (r : Gtype.role) (lab : Gtype.label)
    (ep : Gtype.endpoint option) (msg : expression) (cont : nondet) : inp_role1
    =
  let ep = match ep with None -> failwith "no endpoint" | Some ep -> ep in
  {
    inp_role = r.txt;
    inp_endpoint = ep;
    inp_labels =
      [
        ( lab,
          {
            inp_label = lab;
            inp_parser = msg;
            inp_cont = cont;
          } );
      ];
  }

let make_inp (state_id : state_id) (r : Gtype.role) (lab : Gtype.label)
    (ep : Gtype.endpoint option) (msg : expression) (cont : nondet) : nondet =
  Det
    ( state_id,
      Lazy.from_val @@ DetInp [ (r.txt, make_inp_one r lab ep msg cont) ] )

let rec mem_phys k = function x :: xs -> k == x || mem_phys k xs | [] -> false

let rec merge_one (mergefun : 'a -> 'a -> 'a) acc (l0, out0) = function
  | (l, out) :: outs ->
      if String.equal l0 l then
        let merged = List.rev acc @ ((l, mergefun out0 out) :: outs) in
        Some merged
      else merge_one mergefun ((l, out) :: acc) (l0, out0) outs
  | [] -> None

let merge_exactly_same mergefun x ys =
  match merge_one mergefun [] x ys with
  | Some ys -> ys
  | None -> failwith "can't merge"

let same_indices xs ys =
  let xs = List.map fst xs and ys = List.map fst ys in
  List.sort String.compare xs = List.sort String.compare ys

let full_merge mergefun x ys =
  match merge_one mergefun [] x ys with Some ys -> ys | None -> x :: ys

let merge_out_labels outs1 outs2 =
  let merge_out_label o1 o2 =
    assert (o1.out_label = o2.out_label);
    if o1.out_unparser = o2.out_unparser then
      { o1 with out_cont = Merge [ o1.out_cont; o2.out_cont ] }
    else failwith "unparse functions differ"
  in
  if same_indices outs1 outs2 then
    List.fold_right (merge_exactly_same merge_out_label) outs1 outs2
  else failwith "label sets differ"

let merge_out out1 out2 =
  let merge_out_role o1 o2 =
    assert (o1.out_role = o2.out_role);
    { o1 with out_labels = merge_out_labels o1.out_labels o2.out_labels }
  in
  if same_indices out1 out2 then
    List.fold_right (merge_exactly_same merge_out_role) out1 out2
  else failwith "role sets differ"

let merge_inp_labels inps1 inps2 =
  let merge_inp_label i1 i2 =
    assert (i1.inp_label = i2.inp_label);
    if i1.inp_parser = i2.inp_parser then
      { i1 with inp_cont = Merge [ i1.inp_cont; i2.inp_cont ] }
    else failwith "payload parse functions differ"
  in
  List.fold_right (full_merge merge_inp_label) inps1 inps2

let merge_inp inp1 inp2 =
  let merge_inp_role i1 i2 =
    assert (i1.inp_role = i2.inp_role);
    if i1.inp_endpoint <> i2.inp_endpoint then failwith "endpoints differs"
    else { i1 with inp_labels = merge_inp_labels i1.inp_labels i2.inp_labels }
  in
  if same_indices inp1 inp2 then
    let merge_one = merge_exactly_same merge_inp_role in
    List.fold_right merge_one inp1 inp2
  else failwith "role sets differ"

let merge_det_head : det -> det -> det =
 fun d1 d2 ->
  match (d1, d2) with
  | DetInp inp1, DetInp inp2 -> DetInp (merge_inp inp1 inp2)
  | DetOut out1, DetOut out2 -> DetOut (merge_out out1 out2)
  | DetClose, DetClose -> DetClose
  | _ -> failwith "can't merge"

let concat_out_labels out1 out2 =
  let concat_out_label (l0, out0) outs =
    match List.assoc_opt l0 outs with
    | Some _ -> failwith "label sets are not disjoint"
    | None -> (l0, out0) :: outs
  in
  List.fold_right concat_out_label out1 out2

let concat_out out1 out2 =
  let concat_out_role out1 out2 =
    assert (out1.out_role = out2.out_role);
    { out1 with out_labels = concat_out_labels out1.out_labels out2.out_labels }
  in
  List.fold_right (full_merge concat_out_role) out1 out2

let concat_det : det -> det -> det =
 fun d1 d2 ->
  match (d1, d2) with
  | DetOut out1, DetOut out2 -> DetOut (concat_out out1 out2)
  | _ -> failwith "can't concat"

let filter_self_cycles self backward =
  let backward =
    (* filter out backward epsilon transitions pointing to known states *)
    List.filter (fun id -> id != self) backward
  in
  if List.length backward > 0 then
    (* there're some backward links yet -- return them *)
    Either.Right backward
  else
    (* no backward epsilons anymore: unguarded recursion! *)
    raise UnguardedLoop

let merge_det_full :
    context:context -> state_id -> det lazy_t list -> det lazy_t =
 fun ~context state_id dets ->
  match List.assoc_opt state_id !context with
  | Some det -> det
  | None ->
      let det =
        lazy
          (let dets = List.map Lazy.force dets in
           match dets with
           | det :: dets -> List.fold_left merge_det_head det dets
           | [] -> failwith "impossible: empty merge")
      in
      context := (state_id, det) :: !context;
      det

let rec determinise : context:context -> nondet -> state_id * det lazy_t =
 fun ~context t ->
  let determinise_cont det =
    let determinise_ t =
      let id, det = determinise ~context t in
      Det (id, det)
    in
    match det with
    | DetInp roles ->
        let det_label (l, ({ inp_cont; _ } as inp)) =
          (l, { inp with inp_cont = determinise_ inp_cont })
        in
        let det_role (r, ({ inp_labels; _ } as inp)) =
          (r, { inp with inp_labels = List.map det_label inp_labels })
        in
        DetInp (List.map det_role roles)
    | DetOut roles ->
        let det_label (l, ({ out_cont; _ } as out)) =
          (l, { out with out_cont = determinise_ out_cont })
        in
        let det_role (r, ({ out_labels; _ } as out)) =
          (r, { out with out_labels = List.map det_label out_labels })
        in
        DetOut (List.map det_role roles)
    | DetClose -> DetClose
  in
  let rec flatten_epsilon_cycles ~(visited : nondet list) (t : nondet) :
      (state_id * det lazy_t list, nondet list) Either.t =
    if mem_phys t visited then Right [ t ]
    else
      let visited = t :: visited in
      match t with
      | Det (id, det) -> Either.Left (id, [ det ])
      | Concat ts ->
          let ids, ts = List.split @@ List.map (determinise ~context) ts in
          let det =
            lazy
              (let ts = List.map Lazy.force ts in
               List.fold_left concat_det (List.hd ts) (List.tl ts))
          and state_id = List.fold_left union_ids (List.hd ids) (List.tl ids) in
          Left (state_id, [ det ])
      | Merge ts ->
          let dets, backward =
            List.partition_map (flatten_epsilon_cycles ~visited) ts
          in
          if List.length dets > 0 then
            (* concrete transitons found - return the merged state, discarding the backward links ==== *)
            let ids, dets = List.split dets in
            let id = List.fold_left union_ids (List.hd ids) (List.tl ids) in
            Left (id, List.concat dets)
          else
            (* all transitions are epsilon - verify guardedness ==== *)
            let backward = List.concat backward in
            filter_self_cycles t backward
      | Lazy self -> (
          match flatten_epsilon_cycles ~visited (Lazy.force self) with
          | Left _ as t -> t
          | Right backward -> filter_self_cycles t backward)
  in
  match flatten_epsilon_cycles ~visited:[] t with
  | Left (state_id, dets) ->
      let det = merge_det_full ~context state_id dets in
      let det = lazy (determinise_cont (Lazy.force det)) in
      (state_id, det)
  | Right _ -> raise UnguardedLoop

let rec project0 ~context0 ~(onto : Gtype.role) (gtype : Gtype.t) : nondet =
  let state_id = [ gtype.txt.id ] in
  match gtype.txt.body with
  | EndG -> Det (state_id, Lazy.from_val DetClose)
  | MessageG (r1, lab, ep, r2, msg, cont) -> (
      match () with
      | _ when r1.txt = onto.txt ->
          let cont = project0 ~context0 ~onto cont in
          make_out state_id r2 lab msg cont
      | _ when r2.txt = onto.txt ->
          let cont = project0 ~context0 ~onto cont in
          make_inp state_id r1 lab ep msg cont
      | _ -> project0 ~context0 ~onto cont)
  | ChoiceG (r, conts) ->
      if r.txt = onto.txt then
        let conts = List.map (project0 ~context0 ~onto) conts in
        Concat conts
      else
        let conts = List.map (project0 ~context0 ~onto) conts in
        Merge conts
  | LetRecG (var, _, body0, cont) ->
      let body_state_id = [ var.txt ] in
      let rec context0 = (body_state_id, body) :: context0
      and body = lazy (project0 ~context0 ~onto body0) in
      project0 ~context0 ~onto cont
  | CallG (var, _) ->
      let state_id = [ var.txt ] in
      let body = List.assoc state_id context0 in
      Lazy body
  | ErrG _ -> failwith "todo"
  | Routed (_, _, _, _, _, _, _, _) -> failwith "todo"

type t_ =
  | Inp of (string * t inp_role0) list
  | Out of (string * t out_role0) list
  | Close
  | Goto of state_id

and t = state_id * t_

type inp_role = t inp_role0
type inp_label = t inp_label0
type out_role = t out_role0
type out_label = t out_label0

let rec occurs state_id (cur, t) =
  if state_id = cur then true
  else
    match t with
    | Inp inp ->
        let inplab (_, i) = occurs state_id i.inp_cont in
        let inprole (_, i) = List.exists inplab i.inp_labels in
        List.exists inprole inp
    | Out out ->
        let outlab (_, i) = occurs state_id i.out_cont in
        let outrole (_, i) = List.exists outlab i.out_labels in
        List.exists outrole out
    | Close -> false
    | Goto _ -> false

let rec roles (_, t) : string list =
  let add xs x = if List.mem x xs then xs else x :: xs in
  match t with
  | Inp inp ->
      let inplab (_, i) = roles i.inp_cont in
      let inprole (r, i) = r :: List.concat_map inplab i.inp_labels in
      let roles = List.concat_map inprole inp in
      List.fold_left add [] roles
  | Out out ->
      let outlab (_, i) = roles i.out_cont in
      let outrole (r, i) = r :: List.concat_map outlab i.out_labels in
      let roles = List.concat_map outrole out in
      List.fold_left add [] roles
  | Close -> []
  | Goto _ -> []

let show_state_id = String.concat "_"

let rec show : t -> string =
 fun (state_id, t) ->
  let prefix =
    if occurs state_id ([ "" ], t) then "mu " ^ show_state_id state_id ^ "."
    else ""
  in
  let body =
    match t with
    | Inp inp ->
        let inplab (l, i) = l ^ ":" ^ show i.inp_cont in
        let inprole (r, i) =
          let labels = List.map inplab i.inp_labels |> String.concat ", " in
          r ^ " ? {" ^ labels ^ "}"
        in
        inp |> List.map inprole |> String.concat " & "
    | Out out ->
        let outlab (l, o) = l ^ ":" ^ show o.out_cont in
        let outrole (r, o) =
          let labels = List.map outlab o.out_labels |> String.concat ", " in
          r ^ " ! {" ^ labels ^ "}"
        in
        out |> List.map outrole |> String.concat " + "
    | Close -> "0"
    | Goto goto_id -> show_state_id goto_id
  in
  prefix ^ body

let map_inp_labels f (l, ({ inp_cont; _ } as i)) =
  (l, { i with inp_cont = f inp_cont })

let map_inp_role f (r, ({ inp_labels; _ } as i)) =
  (r, { i with inp_labels = List.map (map_inp_labels f) inp_labels })

let map_out_labels f (l, ({ out_cont; _ } as o)) =
  (l, { o with out_cont = f out_cont })

let map_out_role f (r, ({ out_labels; _ } as o)) =
  (r, { o with out_labels = List.map (map_out_labels f) out_labels })

let rec conv ~seen = function
  | Det (state_id, det) -> (
      if List.mem state_id seen then (state_id, Goto state_id)
      else
        let seen = state_id :: seen in
        match Lazy.force det with
        | DetInp inp ->
            (state_id, Inp (List.map (map_inp_role (conv ~seen)) inp))
        | DetOut out ->
            (state_id, Out (List.map (map_out_role (conv ~seen)) out))
        | DetClose -> (state_id, Close))
  | _ -> failwith "impossible"

let project ~onto gtype =
  project0 ~context0:[] ~onto gtype |> determinise ~context:(ref [])
  |> fun (id, det) -> conv ~seen:[] (Det (id, det))
