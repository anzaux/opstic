type data = (string * bytes) list

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

module Endpoint = struct
  type raw_channel = {
    send : string * data -> unit;
    receive : unit -> string * data;
  }

  type role = string
  type raw_endpoint = (role, raw_channel) Hashtbl.t

  type 'a t = {
    ep_raw : raw_endpoint;
    ep_witness : 'a Lin.t;
    ep_close : unit -> unit;
  }
end

module Witness = struct
  type ('m, 'v) variant = {
    make_var : 'v -> 'm; (* match_var : 'm -> 'v option  *)
  }

  type 'm choice =
    | Choice : {
        choice_label : string;
        choice_marshal : data -> 'v;
        choice_variant : ('m, 'v * 'b Endpoint.t) variant;
        choice_next_wit : 'b;
      }
        -> 'm choice

  type 'm inp = {
    inp_role : string;
    inp_choices : (string, 'm choice) Hashtbl.t;
  }

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
    out_marshal : 'v -> data;
    out_next_wit : 'a;
  }
end

module Comm = struct
  type 'm inp = 'm Witness.inp
  type ('v, 'a) out = ('v, 'a) Witness.out
  type 'a ep = 'a Endpoint.t

  let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep =
   fun ep call v ->
    let out : ('v, 'b) out = call @@ Lin.get ep.ep_witness in
    let raw_ch = Hashtbl.find ep.ep_raw out.out_role in
    raw_ch.send (out.out_label, out.out_marshal v);
    { ep with ep_witness = Lin.create out.out_next_wit }

  let receive : type a. a ep -> (a -> ([> ] as 'b) inp) -> 'b =
   fun ep call ->
    let inp = call @@ Lin.get ep.ep_witness in
    let raw_ch = Hashtbl.find ep.ep_raw inp.inp_role in
    let label, v = raw_ch.receive () in
    let (Choice c) = Hashtbl.find inp.inp_choices label in
    let v = c.choice_marshal v in
    c.choice_variant.make_var
      (v, { ep with ep_witness = Lin.create c.choice_next_wit })

  let close (ep : unit ep) =
    ignore @@ Lin.get ep.ep_witness;
    ep.ep_close ()
end

module SimpleMarshal = struct
  type t = data

  let to_dyn v : t = [ ("data", Marshal.to_bytes v []) ]

  let from_dyn = function
    | [ ("data", bytes) ] -> Marshal.from_bytes bytes 0
    | _ -> failwith "bad protocol"
end

module type CHAN = sig
  type t

  val send : t -> string * data -> unit
  val receive : t -> string * data
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
            { send = C.send me2othr; receive = (fun () -> C.receive othr2me) }
          and othrchan : Endpoint.raw_channel =
            { send = C.send othr2me; receive = (fun () -> C.receive me2othr) }
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

module SynchronousChannel = struct
  type t = (string * data) Event.channel

  let send ch v = Event.sync (Event.send ch v)
  let receive ch = Event.sync (Event.receive ch)
  let create = Event.new_channel
end

module SimpleMpstChannel = SimpleMpstChannel_Make (SynchronousChannel)

let make_a_b () =
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
                              out_marshal = SimpleMarshal.to_dyn;
                              out_next_wit = ();
                            }

                          method lab3 =
                            {
                              out_role = "b";
                              out_label = "lab3";
                              out_marshal = SimpleMarshal.to_dyn;
                              out_next_wit = ();
                            }
                        end
                    end;
                  choice_marshal = SimpleMarshal.from_dyn;
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
              out_marshal = SimpleMarshal.to_dyn;
              out_next_wit =
                object
                  method a =
                    Witness.make_inp ~role:"a"
                      [
                        ( "lab2",
                          Choice
                            {
                              choice_label = "lab2";
                              choice_variant = { make_var = (fun x -> `lab2 x) };
                              choice_next_wit = ();
                              choice_marshal = SimpleMarshal.from_dyn;
                            } );
                        ( "lab3",
                          Choice
                            {
                              choice_label = "lab3";
                              choice_variant = { make_var = (fun x -> `lab3 x) };
                              choice_next_wit = ();
                              choice_marshal = SimpleMarshal.from_dyn;
                            } );
                      ]
                end;
            }
        end
    end
  in
  let raw_ep_a, raw_ep_b =
    let raw_endpoints = SimpleMpstChannel.make [ "a"; "b" ] in
    (List.assoc "a" raw_endpoints, List.assoc "b" raw_endpoints)
  in
  ( Endpoint.
      {
        ep_raw = raw_ep_a;
        ep_witness = Lin.create wit_a;
        ep_close = (fun () -> ());
      },
    Endpoint.
      {
        ep_raw = raw_ep_b;
        ep_witness = Lin.create wit_b;
        ep_close = (fun () -> ());
      } )

open Comm

let debug str = ignore str
(* print_endline str *)

let%test "test_comm" =
  let ep_a, ep_b = make_a_b () in
  let thread_a () =
    let ep = ep_a in
    debug "thread A: receiving";
    let (`lab (_, ep)) = receive ep (fun x -> x#b) in
    debug "thread A: sending";
    if Random.bool () then close @@ send ep (fun x -> x#b#lab2) ()
    else close @@ send ep (fun x -> x#b#lab3) ()
  in
  let thread_b () =
    let ep = ep_b in
    debug "thread B: sending";
    let ep = send ep (fun x -> x#a#lab) 10 in
    debug "thread B: receiving";
    match receive ep (fun x -> x#a) with
    | `lab2 (_, (ep : unit ep)) ->
        debug "thread B: received lab2";
        close ep
    | `lab3 (_, (ep : unit ep)) ->
        debug "thread B: received lab3";
        close ep
  in
  debug "running test";
  Random.init (int_of_float @@ Unix.time ());
  flush stdout;
  let ts = [ thread_a; thread_b ] |> List.map (fun f -> Thread.create f ()) in
  let success = ref false in
  ignore
  @@ Thread.create
       (fun () ->
         Unix.sleepf 2.0;
         if !success then () else ignore @@ Unix._exit 1)
       ();
  List.iter Thread.join ts;
  debug "done.";
  success := true;
  true

let%test "test_lin" =
  debug "checking that linearity violation check is working fine";
  let ep_a, ep_b = make_a_b () in
  try
    let f ep = send ep (fun x -> x#a#lab) () in
    ignore @@ Thread.create (fun () -> ignore @@ receive ep_a (fun x -> x#b)) ();
    ignore
    @@ Thread.create
         (fun () -> try ignore @@ receive ep_a (fun x -> x#b) with _ -> ())
         ();
    ignore @@ f ep_b;
    ignore @@ f ep_b;
    debug "linearity violation detection failure";
    false
  with Lin.LinearityViolation -> true
