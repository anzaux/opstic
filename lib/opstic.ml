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
    type 'm inp_choice =
      | InpChoice : {
          inp_choice_role : ('m, 'l) Rows.constr;
          inp_choice_label : ('l, 'v * 'b t) Rows.constr;
          inp_choice_marshal : payload -> 'v;
          inp_choice_next_wit : 'b;
        }
          -> 'm inp_choice

    type 'm inp = {
      inp_roles : string list;
      inp_choices : (string * string, 'm inp_choice) Hashtbl.t;
      inp_connection : connection;
    }
      constraint 'm = [> ]

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

    type 'm out_choice =
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

    let receive : ([> ] as 'a) inp ep -> ('a -> unit io) -> unit io =
     fun ep callback ->
      let inp = Lin.get ep.ep_witness in
      let* role, label, v =
        Endpoint.receive ep.ep_raw ~roles:inp.inp_roles
          ~connection:inp.inp_connection
      in
      let (InpChoice c) = Hashtbl.find inp.inp_choices (role, label) in
      let v = c.inp_choice_marshal v in
      callback
        (c.inp_choice_role.make_var
           (c.inp_choice_label.make_var
              (v, { ep with ep_witness = Lin.create c.inp_choice_next_wit })))

    let close (ep : unit ep) =
      ignore @@ Lin.get ep.ep_witness;
      Endpoint.close ep.ep_raw
  end
end
