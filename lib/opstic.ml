module type Payload = sig
  type payload
end

module type Monadic = sig
  type _ t

  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
end

type role = string

module type Endpoint = sig
  type t
  type _ io
  type payload

  val send : t -> string -> string * payload -> unit
  val receive : t -> string -> (string * payload) io
  val close : t -> unit
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

module Make (Io : Monadic) (Endpoint : Endpoint with type 'x io = 'x Io.t) =
struct
  type 'a t = { ep_raw : Endpoint.t; ep_witness : 'a Lin.t }
  type payload = Endpoint.payload

  let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit }

  module Lin = Lin

  module Witness = struct
    type ('m, 'x) variant = { make_var : 'x -> 'm (* 'x -> [> `lab of 'x] *) }

    type 'm choice =
      | Choice : {
          choice_label : string;
          choice_marshal : Endpoint.payload -> 'v;
          choice_variant : ('m, 'v * 'b t) variant;
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
      out_marshal : 'v -> Endpoint.payload;
      out_next_wit : 'a;
    }
  end

  module Comm = struct
    type 'm inp = 'm Witness.inp
    type ('v, 'a) out = ('v, 'a) Witness.out
    type 'a ep = 'a t

    let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep Io.t =
     fun ep call (*fun x -> x#a#lab*) v ->
      let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
      Endpoint.send ep.ep_raw out.out_role (out.out_label, out.out_marshal v);
      Io.return { ep with ep_witness = Lin.create out.out_next_wit }

    let receive : type a. a ep -> (a -> ([> ] as 'b) inp) -> 'b Io.t =
     fun ep call (*fun x -> x#a*) ->
      let inp = call @@ Lin.get ep.ep_witness in
      Io.bind (Endpoint.receive ep.ep_raw inp.inp_role) (fun (label, v) ->
          let (Choice c) = Hashtbl.find inp.inp_choices label in
          let v = c.choice_marshal v in
          Io.return
            (c.choice_variant.make_var (*fun x -> `lab x*)
               (v, { ep with ep_witness = Lin.create c.choice_next_wit })))

    let close (ep : unit ep) =
      ignore @@ Lin.get ep.ep_witness;
      Endpoint.close ep.ep_raw
  end
end

module type Channel = sig
  type t
  type _ io
  type payload

  val create : unit -> t
  val send : t -> string * payload -> unit
  val receive : t -> (string * payload) io
  val close : t -> unit
end

module LocalEndpoint (C : Channel) : sig
  include Endpoint

  val make : string list -> (string * t) list
end
with type 'x io = 'x C.io
 and type payload = C.payload = struct
  type payload = C.payload
  type 'x io = 'x C.io

  type raw_channel = {
    raw_send : string * payload -> unit;
    raw_receive : unit -> (string * payload) io;
    raw_close : unit -> unit;
  }

  type t = { raw_channels : (string, raw_channel) Hashtbl.t }

  let send t role msg = (Hashtbl.find t.raw_channels role).raw_send msg
  let receive t role = (Hashtbl.find t.raw_channels role).raw_receive ()
  let close t = Hashtbl.iter (fun _ ch -> ch.raw_close ()) t.raw_channels

  let make (roles : string list) : (string * t) list =
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
          let mychan : raw_channel =
            {
              raw_send = C.send me2othr;
              raw_receive = (fun () -> C.receive othr2me);
              raw_close = ignore;
            }
          and othrchan : raw_channel =
            {
              raw_send = C.send othr2me;
              raw_receive = (fun () -> C.receive me2othr);
              raw_close = ignore;
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
    roles
    |> List.map (fun r -> (r, { raw_channels = Hashtbl.find all_tables r }))
end

module type Marshal = sig
  type payload

  val to_dyn : 'a -> payload
  val from_dyn : payload -> 'a
end

module Sample0
    (Io : Monadic)
    (Endpoint : Endpoint with type 'x io = 'x Io.t)
    (Marshal : Marshal with type payload = Endpoint.payload) =
struct
  module Mpst = Make (Io) (Endpoint)
  open Mpst

  let sample1 () =
    let wit_a =
      let open Witness in
      object
        method b =
          Witness.make_inp ~role:"b"
            [
              ( "lab",
                Choice
                  {
                    choice_label = "lab";
                    choice_variant = { make_var = (fun x -> `lab x) };
                    choice_next_wit =
                      object
                        method b =
                          object
                            method lab2 =
                              {
                                out_role = "b";
                                out_label = "lab2";
                                out_marshal = Marshal.to_dyn;
                                out_next_wit = ();
                              }

                            method lab3 =
                              {
                                out_role = "b";
                                out_label = "lab3";
                                out_marshal = Marshal.to_dyn;
                                out_next_wit = ();
                              }
                          end
                      end;
                    choice_marshal = Marshal.from_dyn;
                  } );
            ]
      end
    and wit_b =
      let open Witness in
      object
        method a =
          object
            method lab =
              {
                out_role = "a";
                out_label = "lab";
                out_marshal = (Marshal.to_dyn : int -> payload);
                out_next_wit =
                  object
                    method a =
                      Witness.make_inp ~role:"a"
                        [
                          ( "lab2",
                            Choice
                              {
                                choice_label = "lab2";
                                choice_variant =
                                  { make_var = (fun x -> `lab2 x) };
                                choice_next_wit = ();
                                choice_marshal =
                                  (Marshal.from_dyn : payload -> unit);
                              } );
                          ( "lab3",
                            Choice
                              {
                                choice_label = "lab3";
                                choice_variant =
                                  { make_var = (fun x -> `lab3 x) };
                                choice_next_wit = ();
                                choice_marshal =
                                  (Marshal.from_dyn : payload -> unit);
                              } );
                        ]
                  end;
              }
          end
      end
    in
    (wit_a, wit_b)
end
