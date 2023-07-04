type 'a ep = 'a Witness.ep = {
  ep_raw : ServerEndpoint.t;
  ep_witness : 'a Lin.t;
}

type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out

let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep ServerIo.t =
 fun ep call (*fun x -> x#a#lab*) v ->
  let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
  ServerIo.bind
    (ServerEndpoint.send ep.ep_raw ~kind:out.out_kind ~role:out.out_role
       ~label:out.out_label ~payload:(out.out_marshal v))
    (fun () ->
      ServerIo.return { ep with ep_witness = Lin.create out.out_next_wit })

let receive : 'a inp ep -> 'a ServerIo.t =
 fun ep ->
  let inp = Lin.get ep.ep_witness in
  ServerIo.bind
    (ServerEndpoint.receive ep.ep_raw ~roles:inp.inp_roles ~kind:inp.inp_kind)
    (fun (role, label, v) ->
      let (InpChoice c) = Hashtbl.find inp.inp_choices (role, label) in
      let v = c.inp_choice_marshal v in
      ServerIo.return
        (c.inp_choice_role.make_var
           (c.inp_choice_label.make_var
              (v, { ep with ep_witness = Lin.create c.inp_choice_next_wit }))))

let close (ep : unit ep) =
  ignore @@ Lin.get ep.ep_witness;
  ServerEndpoint.close ep.ep_raw

let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit }
