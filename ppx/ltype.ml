open Ppxlib

type state_id = string list

type inp_label = {
  inp_label : string;
  inp_parse_payload : expression;
  inp_cont : t;
}

and inp_role = {
  inp_role : Gtype.role;
  inp_endpoint : Gtype.endpoint;
  inp_parse_label : expression;
  inp_labels : (string * inp_label) list;
}

and out_label = { out_label : string; out_unparse : expression; out_cont : t }
and out_role = { out_role : Gtype.role; out_labels : (string * out_label) list }

(* deterministic transition *)
and det =
  | Inp of (Gtype.role * inp_role) list
  | Out of (Gtype.role * out_role) list
  | Close

and t = Det of state_id * det lazy_t | Merge of t list | Call of state_id

exception UnguardedLoop

type visited = state_id list
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

let make_out_one (r : Gtype.role) (lab : Gtype.label) (_msg : expression)
    (cont : t) : out_role =
  {
    out_role = r;
    out_labels =
      [
        (lab, { out_label = lab; out_unparse = assert false; out_cont = cont });
      ];
  }

let make_out (id : string) (r : Gtype.role) (lab : Gtype.label)
    (msg : expression) (cont : t) =
  Det ([ id ], Lazy.from_val @@ Out [ (r, make_out_one r lab msg cont) ])

let make_inp_one (r : Gtype.role) (lab : Gtype.label) (ep : Gtype.endpoint)
    (_msg : expression) (cont : t) : inp_role =
  {
    inp_role = r;
    inp_endpoint = ep;
    inp_parse_label = assert false;
    inp_labels =
      [
        ( lab,
          { inp_label = lab; inp_parse_payload = assert false; inp_cont = cont }
        );
      ];
  }

let make_inp (id : string) (r : Gtype.role) (lab : Gtype.label)
    (ep : Gtype.endpoint) (msg : expression) (cont : t) : t =
  Det ([ id ], Lazy.from_val @@ Inp [ (r, make_inp_one r lab ep msg cont) ])

let rec mem_phys k = function x :: xs -> k == x || mem_phys k xs | [] -> false

let merge_det_head : det -> det -> det =
 fun d1 d2 ->
  match (d1, d2) with
  | Inp _, Inp _ | Out _, Out _ | Close, Close -> assert false
  | _ -> failwith "can't merge"

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

let rec merge_det_full :
    context:context -> state_id -> det lazy_t list -> det lazy_t =
 fun ~context state_id dets ->
  let determinise_cont det =
    let determinise_ : t -> t =
     fun t ->
      let id, det = determinise ~context t in
      Det (id, det)
    in
    match det with
    | Inp roles ->
        let det_label (l, ({ inp_cont; _ } as inp)) =
          (l, { inp with inp_cont = determinise_ inp_cont })
        in
        let det_role (r, ({ inp_labels; _ } as inp)) =
          (r, { inp with inp_labels = List.map det_label inp_labels })
        in
        Inp (List.map det_role roles)
    | Out roles ->
        let det_label (l, ({ out_cont; _ } as out)) =
          (l, { out with out_cont = determinise_ out_cont })
        in
        let det_role (r, ({ out_labels; _ } as out)) =
          (r, { out with out_labels = List.map det_label out_labels })
        in
        Out (List.map det_role roles)
    | Close -> Close
  in
  match List.assoc_opt state_id !context with
  | Some det -> det
  | None ->
      let det =
        lazy
          (let dets = List.map Lazy.force dets in
           match dets with
           | [ det ] -> determinise_cont det
           | det :: dets ->
               let det = List.fold_left merge_det_head det dets in
               determinise_cont det
           | [] -> failwith "impossible: empty merge")
      in
      context := (state_id, det) :: !context;
      det

and determinise : context:context -> t -> state_id * det lazy_t =
 fun ~context t ->
  let rec flatten_epsilon_cycles ~visited t :
      (state_id * det lazy_t list, t list) Either.t =
    if mem_phys t visited then Right [ t ]
    else
      match t with
      | Det (id, det) -> Either.Left (id, [ det ])
      | Merge ts ->
          let dets, backward =
            List.partition_map
              (flatten_epsilon_cycles ~visited:(t :: visited))
              ts
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
      | Call _ -> assert false
  in
  match flatten_epsilon_cycles ~visited:[] t with
  | Left (state_id, dets) -> (state_id, merge_det_full ~context state_id dets)
  | Right _ -> raise UnguardedLoop

let merge ~context t1 t2 : t =
  let id1, det1 = determinise ~context t1
  and id2, det2 = determinise ~context t2 in
  let id = union_ids id1 id2 in
  Det (id, merge_det_full ~context id [ det1; det2 ])

let rec project0 ~(onto : Gtype.role) (gtype : Gtype.t) : t =
  match gtype.txt.body with
  | EndG -> Det ([ gtype.txt.id ], Lazy.from_val Close)
  | MessageG (r1, lab, ep, r2, msg, cont) -> (
      match () with
      | _ when r1.txt = onto.txt ->
          let cont = project0 ~onto cont in
          make_out gtype.txt.id r2 lab msg cont
      | _ when r2.txt = onto.txt ->
          let cont = project0 ~onto cont in
          make_inp gtype.txt.id r1 lab ep msg cont
      | _ -> project0 ~onto cont)
  | ChoiceG (r, conts) ->
      if r.txt = onto.txt then assert false
      else
        let conts = List.map (project0 ~onto) conts in
        Merge conts
  | CallG (var, _) -> Call [ var.txt ]
  | ErrG _ | LetRecG (_, _, _, _) | PureLet (_, _, _) -> failwith "todo"
  | Routed (_, _, _, _, _, _, _, _) -> failwith "todo"

let project ~onto gtype = project0 ~onto gtype |> determinise ~context:(ref [])
