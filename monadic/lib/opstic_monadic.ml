open Opstic

module type Monadic = sig
  type _ t

  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
end

module type Endpoint = sig
  type t
  type _ io
  type payload

  val send :
    t ->
    connection:connection ->
    role:string ->
    label:string ->
    payload:payload ->
    unit io

  val receive :
    t ->
    connection:connection ->
    roles:string list ->
    (string * string * payload) io

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

module type S = sig
  type endpoint
  type payload
  type 'x io
  type 'a t = { ep_raw : endpoint; ep_witness : 'a Lin.t }

  val create : endpoint -> 'a -> 'a t

  module Lin = Lin

  module Witness : sig
    type 'm inp_choice =
      | InpChoice : {
          inp_choice_role : ('m, 'l) Rows.constr;
          inp_choice_label : ('l, 'v * 'b t) Rows.constr;
          inp_choice_marshal : payload -> 'v;
          inp_choice_next_wit : 'b;
        }
          -> 'm inp_choice

    type 'a inp = {
      inp_roles : string list;
      inp_choices : (string * string, 'a inp_choice) Hashtbl.t;
      inp_connection : connection;
    }
      constraint 'a = [> ]

    val make_inp : ?conn:connection -> ([> ] as 'a) inp_choice list -> 'a inp

    type ('v, 'a) out = {
      out_role : string;
      out_label : string;
      out_marshal : 'v -> payload;
      out_next_wit : 'a;
      out_connection : connection;
    }

    val make_out :
      role:string ->
      label:string ->
      marshal:('a -> payload) ->
      ?conn:connection ->
      'b ->
      ('a, 'b) out
  end

  module Comm : sig
    type 'a inp = 'a Witness.inp constraint 'a = [> ]
    type ('v, 'a) out = ('v, 'a) Witness.out
    type 'a ep = 'a t

    val send : 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep io
    val receive : ([> ] as 'b) inp ep -> 'b io
    val close : unit ep -> unit
  end
end

module Make (Io : Monadic) (Endpoint : Endpoint with type 'x io = 'x Io.t) :
  S
    with type 'x io = 'x Io.t
     and type payload = Endpoint.payload
     and type endpoint = Endpoint.t = struct
  type 'a t = { ep_raw : Endpoint.t; ep_witness : 'a Lin.t }
  type 'x io = 'x Io.t
  type endpoint = Endpoint.t
  type payload = Endpoint.payload

  let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit }

  module Lin = Lin

  module Witness = struct
    type 'm inp_choice =
      | InpChoice : {
          inp_choice_role : ('m, 'l) Rows.constr;
          inp_choice_label : ('l, 'v * 'b t) Rows.constr;
          inp_choice_marshal : payload -> 'v;
          inp_choice_next_wit : 'b;
        }
          -> 'm inp_choice

    type 'a inp = {
      inp_roles : string list;
      inp_choices : (string * string, 'a inp_choice) Hashtbl.t;
      inp_connection : connection;
    }
      constraint 'a = [> ]

    let make_inp ?(conn = Connected) (xs : 'm inp_choice list) : 'm inp =
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
      let roles =
        xs |> List.map (fun (InpChoice c0) -> c0.inp_choice_role.constr_name)
      in
      { inp_roles = roles; inp_connection = conn; inp_choices = tbl }

    type ('v, 'a) out = {
      out_role : string;
      out_label : string;
      out_marshal : 'v -> payload;
      out_next_wit : 'a;
      out_connection : connection;
    }

    let make_out ~role ~label ~marshal ?(conn = Connected) next =
      {
        out_role = role;
        out_label = label;
        out_marshal = marshal;
        out_next_wit = next;
        out_connection = conn;
      }
  end

  module Comm = struct
    type 'm inp = 'm Witness.inp
    type ('v, 'a) out = ('v, 'a) Witness.out
    type 'a ep = 'a t

    let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep io =
     fun ep call (*fun x -> x#a#lab*) v ->
      let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
      Io.bind
        (Endpoint.send ep.ep_raw ~connection:out.out_connection
           ~role:out.out_role ~label:out.out_label ~payload:(out.out_marshal v))
        (fun () ->
          Io.return { ep with ep_witness = Lin.create out.out_next_wit })

    let receive : 'a inp ep -> 'a Io.t =
     fun ep ->
      let inp = Lin.get ep.ep_witness in
      Io.bind
        (Endpoint.receive ep.ep_raw ~roles:inp.inp_roles
           ~connection:inp.inp_connection) (fun (role, label, v) ->
          let (InpChoice c) = Hashtbl.find inp.inp_choices (role, label) in
          let v = c.inp_choice_marshal v in
          Io.return
            (c.inp_choice_role.make_var
               (c.inp_choice_label.make_var
                  (v, { ep with ep_witness = Lin.create c.inp_choice_next_wit }))))

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

module LocalEndpoint (Io : Monadic) (C : Channel with type 'x io = 'x Io.t) : sig
  include Endpoint with type 'x io = 'x Io.t and type payload = C.payload

  val make : string list -> (string * t) list
end
with type 'x io = 'x C.io
 and type payload = C.payload = struct
  type payload = C.payload
  type 'x io = 'x C.io

  let ( let* ) = Io.bind

  type raw_channel = {
    raw_send : string * payload -> unit;
    raw_receive : unit -> (string * payload) io;
    raw_close : unit -> unit;
  }

  type t = { raw_channels : (string, raw_channel) Hashtbl.t }

  let send t ~connection:_ ~role ~label ~payload =
    (Hashtbl.find t.raw_channels role).raw_send (label, payload);
    Io.return ()

  let receive t ~connection:_ ~roles =
    match roles with
    | [ role ] ->
        let* label, value = (Hashtbl.find t.raw_channels role).raw_receive () in
        Io.return (role, label, value)
    | _ ->
        failwith
          "TODO: Opstic_monadic (native): Cannot wait for multirple roles"

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
  open Rows

  [%%declare_constr a]
  [%%declare_constr b]
  [%%declare_constr lab]
  [%%declare_constr lab2]
  [%%declare_constr lab3]

  let sample1 () =
    let wit_a =
      let open Witness in
      Witness.make_inp ~conn:Join
        [
          InpChoice
            {
              inp_choice_role = b;
              inp_choice_label = lab;
              inp_choice_next_wit =
                object
                  method b =
                    object
                      method lab2 =
                        Witness.make_out ~role:"b" ~label:"lab2"
                          ~marshal:Marshal.to_dyn ()

                      method lab3 =
                        Witness.make_out ~role:"b" ~label:"lab3"
                          ~marshal:Marshal.to_dyn ()
                    end
                end;
              inp_choice_marshal = Marshal.from_dyn;
            };
        ]
    and wit_b =
      let open Witness in
      object
        method a =
          object
            method lab =
              Witness.make_out ~conn:Join ~role:"a" ~label:"lab"
                ~marshal:(Marshal.to_dyn : int -> payload)
                (Witness.make_inp
                   [
                     InpChoice
                       {
                         inp_choice_role = a;
                         inp_choice_label = lab2;
                         inp_choice_next_wit = ();
                         inp_choice_marshal =
                           (Marshal.from_dyn : payload -> unit);
                       };
                     InpChoice
                       {
                         inp_choice_role = a;
                         inp_choice_label = lab3;
                         inp_choice_next_wit = ();
                         inp_choice_marshal =
                           (Marshal.from_dyn : payload -> unit);
                       };
                   ])
          end
      end
    in
    (wit_a, wit_b)
end
