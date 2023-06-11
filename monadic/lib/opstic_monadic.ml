open Opstic

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

module Sample0
    (Io : Opstic.Monadic)
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
      (Witness.make_inp ~conn:Join
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
        : [< `b of _ ] inp)
      (* NB this type annotation is mandatory for session-type safety *)
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
                         inp_choice_role = (a : ([< `a of _ ], _) Rows.constr);
                         inp_choice_label = lab2;
                         inp_choice_next_wit = ();
                         inp_choice_marshal =
                           (Marshal.from_dyn : payload -> unit);
                       };
                     InpChoice
                       {
                         inp_choice_role = (a : ([< `a of _ ], _) Rows.constr);
                         inp_choice_label = lab3;
                         inp_choice_next_wit = ();
                         inp_choice_marshal =
                           (Marshal.from_dyn : payload -> unit);
                       };
                   ]
                  : [< `a of _ ] inp)
          end
      end
    in
    (wit_a, wit_b)
end
