type payload = (string * bytes) list

module type Monadic = sig
  type _ t

  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
end

module Lin : sig
  type 'a t

  exception LinearityViolation

  val create : 'a -> 'a t
  val get : 'a t -> 'a
end = struct
  type 'a t = { lin_val : 'a; mutable lin_flag : bool }

  exception LinearityViolation

  let create x = { lin_val = x; lin_flag = true }

  let get x =
    (* XXX race *)
    if x.lin_flag then (
      x.lin_flag <- false;
      x.lin_val)
    else raise LinearityViolation
end

module Make (Io : Monadic) = struct
  module Endpoint = struct
    type raw_channel = {
      send : string * payload -> unit;
      receive : unit -> string * payload;
      close : unit -> unit;
    }

    type role = string
    type raw_endpoint = (role, raw_channel) Hashtbl.t
    type 'a t = { ep_raw : raw_endpoint; ep_witness : 'a Lin.t }

    let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit }
  end

  module Witness = struct
    type ('m, 'x) variant = { make_var : 'x -> 'm (* 'x -> [> `lab of 'x] *) }

    type 'm choice =
      | Choice : {
          choice_label : string;
          choice_marshal : payload -> 'v;
          choice_variant : ('m, 'v * 'b Endpoint.t) variant;
          choice_next_wit : 'b;
        }
          -> 'm choice

    type 'm inp = {
      inp_role : string;
      inp_choices : (string, 'm choice) Hashtbl.t;
    }
      constraint 'm = [> ]

    let make_inp ~role (xs : (string * 'm choice) list) : 'm inp =
      let tbl = Hashtbl.create (List.length xs) in
      let rec put_all = function
        | (k, v) :: xs ->
            Hashtbl.add tbl k v;
            put_all xs
        | [] -> ()
      in
      put_all xs;
      { inp_role = role; inp_choices = tbl }

    type ('v, 'a) out = {
      out_role : string;
      out_label : string;
      out_marshal : 'v -> payload;
      out_next_wit : 'a;
    }
  end

  module Comm = struct
    type 'm inp = 'm Witness.inp
    type ('v, 'a) out = ('v, 'a) Witness.out
    type 'a ep = 'a Endpoint.t

    let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep =
     fun ep call (*fun x -> x#a#lab*) v ->
      let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
      let raw_ch = Hashtbl.find ep.ep_raw out.out_role in
      raw_ch.send (out.out_label, out.out_marshal v);
      { ep with ep_witness = Lin.create out.out_next_wit }

    let receive : type a. a ep -> (a -> ([> ] as 'b) inp) -> 'b =
     fun ep call (*fun x -> x#a*) ->
      let inp = call @@ Lin.get ep.ep_witness in
      let raw_ch = Hashtbl.find ep.ep_raw inp.inp_role in
      let label, v = raw_ch.receive () in
      let (Choice c) = Hashtbl.find inp.inp_choices label in
      let v = c.choice_marshal v in
      c.choice_variant.make_var (*fun x -> `lab x*)
        (v, { ep with ep_witness = Lin.create c.choice_next_wit })

    let close (ep : unit ep) =
      ignore @@ Lin.get ep.ep_witness;
      Hashtbl.iter (fun _ ch -> ch.Endpoint.close ()) ep.ep_raw
  end

  module type CHAN = sig
    type t

    val send : t -> string * payload -> unit
    val receive : t -> string * payload
    val create : unit -> t
  end

  module SimpleMpstChannel_Make (C : CHAN) = struct
    let make (roles : string list) : (string * Endpoint.raw_endpoint) list =
      let all_tables =
        let t = Hashtbl.create (List.length roles - 1) in
        roles
        |> List.fold_left
             (fun t r ->
               Hashtbl.add t r (Hashtbl.create (List.length roles - 1));
               t)
             t
      in
      let rec create_bidirectional_channels (myname : string)
          (theirnames : string list) =
        match theirnames with
        | [] -> ()
        | othrname :: rest ->
            let me2othr = C.create () in
            let othr2me = C.create () in
            let mychan : Endpoint.raw_channel =
              {
                send = C.send me2othr;
                receive = (fun () -> C.receive othr2me);
                close = ignore;
              }
            and othrchan : Endpoint.raw_channel =
              {
                send = C.send othr2me;
                receive = (fun () -> C.receive me2othr);
                close = ignore;
              }
            in
            let mytbl = Hashtbl.find all_tables myname in
            let othrtbl = Hashtbl.find all_tables othrname in
            Hashtbl.add mytbl othrname mychan;
            Hashtbl.add othrtbl myname othrchan;
            create_bidirectional_channels myname rest
      in
      let rec create_all = function
        | r :: rs ->
            create_bidirectional_channels r rs;
            create_all rs
        | [] -> ()
      in
      create_all roles;
      roles |> List.map (fun r -> (r, Hashtbl.find all_tables r))
  end
end
