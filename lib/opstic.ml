type connection = Connected | Join | JoinCorrelation

module Id = Id

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

module Make (Io : Monadic) (Endpoint : Endpoint with type 'x io = 'x Io.t) =
struct
  type 'a t = { ep_raw : Endpoint.t; ep_witness : 'a Lin.t }
  type 'x io = 'x Io.t
  type endpoint = Endpoint.t
  type payload = Endpoint.payload

  let return = Io.return
  let ( let* ) = Io.bind
  let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit }

  module Lin = Lin

  module Witness = struct
    type _ inp_label_choice =
      | InpLabelChoice : {
          inp_label_choice_label : ('l, 'v * 'b t) Rows.constr;
          inp_label_choice_marshal : payload -> 'v;
          inp_label_choice_next_wit : 'b;
        }
          -> 'l inp_label_choice

    type _ inp_role_choice =
      | InpRoleChoice : {
          inp_role_choice_role : ('m, 'l -> unit io) Rows.constr;
          inp_role_choice_labels : (string, 'l inp_label_choice) Hashtbl.t;
        }
          -> 'm inp_role_choice

    type 'm inp = {
      inp_roles : string list;
      inp_choices : 'm inp_role_choice list;
      inp_connection : connection;
    }
      constraint 'm = [> ]

    type _ inp_role_spec =
      | InpRoleSpec : {
          inp_role_spec_role : ('m, 'l -> unit io) Rows.constr;
          inp_role_spec_labels : 'l inp_label_choice list;
        }
          -> 'm inp_role_spec

    let make_inp ?(conn = Connected) (xs : 'm inp_role_spec list) : 'm inp =
      let rec put_all tbl = function
        | (InpLabelChoice c0 as c) :: xs ->
            Hashtbl.add tbl c0.inp_label_choice_label.constr_name c;
            put_all tbl xs
        | [] -> ()
      in
      let make_role_choice (InpRoleSpec c) =
        let hash = Hashtbl.create (List.length c.inp_role_spec_labels) in
        put_all hash c.inp_role_spec_labels;
        InpRoleChoice
          {
            inp_role_choice_role = c.inp_role_spec_role;
            inp_role_choice_labels = hash;
          }
      in
      let roles =
        xs
        |> List.map (fun (InpRoleSpec c0) -> c0.inp_role_spec_role.constr_name)
      in
      {
        inp_roles = roles;
        inp_connection = conn;
        inp_choices = List.map make_role_choice xs;
      }

    type _ out_choice =
      | OutChoice : {
          out_choice_role : ('m, 'l) Rows.constr;
          out_choice_label : ('l, 'v * ('b t -> unit io)) Rows.constr;
          out_choice_marshal : 'v -> payload;
          out_choice_next_wit : 'b;
        }
          -> 'm out_choice

    type 'm out = {
      out_choices : 'm out_choice list;
      out_connection : connection;
    }
      constraint 'm = [> ]

    let make_out ?(conn = Connected) (xs : 'm out_choice list) : 'm out =
      { out_connection = conn; out_choices = xs }
  end

  module Comm = struct
    type 'm inp = 'm Witness.inp
    type 'm out = 'm Witness.out
    type 'a ep = 'a t

    let send : 'm. 'm out ep -> 'm -> unit io =
     fun ep msg ->
      let out = Lin.get ep.ep_witness in
      let rec loop = function
        | Witness.OutChoice c :: cs -> (
            match
              Option.bind
                (c.out_choice_role.match_var msg)
                c.out_choice_label.match_var
            with
            | None -> loop cs
            | Some (v, callback) ->
                let* () =
                  Endpoint.send ep.ep_raw ~connection:out.out_connection
                    ~role:c.out_choice_role.constr_name
                    ~label:c.out_choice_label.constr_name
                    ~payload:(c.out_choice_marshal v)
                in
                callback
                  { ep with ep_witness = Lin.create c.out_choice_next_wit })
        | [] -> failwith ""
      in
      loop out.out_choices

    let do_receive (type l) ep ~role ~connection
        ~(labels : (string, l Witness.inp_label_choice) Hashtbl.t)
        ~(callback : l -> unit io) : unit io =
      let* role', label, payload =
        Endpoint.receive ep.ep_raw ~roles:[ role ] ~connection
      in
      assert (role = role');
      let (InpLabelChoice cl) = Hashtbl.find labels label in
      let value = cl.inp_label_choice_marshal payload
      and ep =
        { ep with ep_witness = Lin.create cl.inp_label_choice_next_wit }
      in
      callback (cl.inp_label_choice_label.make_var (value, ep))

    let receive : ([> ] as 'a) inp ep -> 'a -> unit io =
     fun ep role_choice ->
      let inp = Lin.get ep.ep_witness in
      let rec loop = function
        | [] -> failwith "protocol error"
        | Witness.InpRoleChoice cr :: cs -> (
            match cr.inp_role_choice_role.match_var role_choice with
            | None -> loop cs
            | Some callback ->
                let role = cr.inp_role_choice_role.constr_name
                and labels = cr.inp_role_choice_labels
                and connection = inp.inp_connection in
                do_receive ep ~role ~connection ~labels ~callback)
      in
      loop inp.inp_choices

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
    | [] -> failwith "No role is given"
    | _ ->
        failwith
          ("TODO: Opstic: Cannot wait for multirple roles: "
         ^ String.concat "," roles)

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
  open! Mpst
  open! Rows

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
           InpRoleSpec
             {
               inp_role_spec_role = b;
               inp_role_spec_labels =
                 [
                   InpLabelChoice
                     {
                       inp_label_choice_label = lab;
                       inp_label_choice_marshal =
                         (Marshal.from_dyn : payload -> int);
                       inp_label_choice_next_wit =
                         (Witness.make_out ~conn:Connected
                            [
                              OutChoice
                                {
                                  out_choice_role = b;
                                  out_choice_label = lab2;
                                  out_choice_marshal =
                                    (Marshal.to_dyn : unit -> payload);
                                  out_choice_next_wit = ();
                                };
                              OutChoice
                                {
                                  out_choice_role = b;
                                  out_choice_label = lab3;
                                  out_choice_marshal =
                                    (Marshal.to_dyn : unit -> payload);
                                  out_choice_next_wit = ();
                                };
                            ]
                           : [< `b of [ `lab2 of _ | `lab3 of _ ] ] out);
                     };
                 ];
             };
         ]
        : [< `b of _ ] inp)
      (* NB this type annotation is mandatory for session-type safety *)
    and wit_b =
      let open Witness in
      (Witness.make_out ~conn:Connected
         [
           OutChoice
             {
               out_choice_role = a;
               out_choice_label = lab;
               out_choice_marshal = (Marshal.to_dyn : int -> payload);
               out_choice_next_wit =
                 (make_inp
                    [
                      InpRoleSpec
                        {
                          inp_role_spec_role = a;
                          inp_role_spec_labels =
                            [
                              InpLabelChoice
                                {
                                  inp_label_choice_label = lab2;
                                  inp_label_choice_marshal =
                                    (Marshal.from_dyn : payload -> unit);
                                  inp_label_choice_next_wit = ();
                                };
                              InpLabelChoice
                                {
                                  inp_label_choice_label = lab3;
                                  inp_label_choice_marshal =
                                    (Marshal.from_dyn : payload -> unit);
                                  inp_label_choice_next_wit = ();
                                };
                            ];
                        };
                    ]
                   : [< `a of _ ] inp);
             };
         ]
        : [< `a of [ `lab of _ ] ] out)
    in
    (wit_a, wit_b)
end
