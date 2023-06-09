type ('m, 'x) variant = { make_var : 'x -> 'm; call_var : 'm -> 'x option }
type connection = Connected | Join | JoinCorrelation

let () =
  let spin = ref (assert false) in
  !spin (`A (100, fun ep -> ()))

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
    t -> connection:connection -> role:string -> (string * payload) io

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

  let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit }

  module Lin = Lin

  module Witness = struct
    type 'm inp_choice =
      | InpChoice : {
          inp_choice_role : string;
          inp_choice_label : string;
          inp_choice_marshal : payload -> 'v;
          inp_choice_variant : ('m, 'v * 'b t) variant;
          inp_choice_next_wit : 'b;
        }
          -> 'm inp_choice

    type 'm inp = {
      inp_role : string;
      inp_choices : (string, 'm inp_choice) Hashtbl.t;
      inp_connection : connection;
    }
      constraint 'm = [> ]

    let make_inp ~role ?(conn = Connected) (xs : (string * 'm inp_choice) list)
        : 'm inp =
      let tbl = Hashtbl.create (List.length xs) in
      let rec put_all = function
        | (k, v) :: xs ->
            Hashtbl.add tbl k v;
            put_all xs
        | [] -> ()
      in
      put_all xs;
      { inp_role = role; inp_connection = conn; inp_choices = tbl }

    type 'm out_choice =
      | OutChoice : {
          out_choice_role : string;
          out_choice_label : string;
          out_choice_marshal : 'v -> payload;
          out_choice_variant : ('m, 'v * ('b t -> unit)) variant;
          out_choice_next_wit : 'b;
        }
          -> 'm out_choice

    type 'm out = {
      out_choices : 'm out_choice list;
      out_connection : connection;
    }

    (* let make_out ?(con = Connected)
        (xs : 'm choice list) : 'm out =
        {
          out_choices = List.map (fun (role,label,choice))
          out_connection=Connected
        } *)
  end

  module Comm = struct
    type 'm inp = 'm Witness.inp
    type 'm out = 'm Witness.out
    type 'a ep = 'a t

    let send : 'm out ep -> 'm -> unit io =
     fun ep msg ->
      let out : 'm out = Lin.get ep.ep_witness in
      let rec loop = function
        | Witness.OutChoice c :: cs -> (
            match c.out_choice_variant.call_var msg with
            | Some (v, f) ->
                Io.bind
                  (Endpoint.send ep.ep_raw ~connection:out.out_connection
                     ~role:c.out_choice_role ~label:c.out_choice_label
                     ~payload:(c.out_choice_marshal v))
                  (fun () ->
                    Io.return
                    @@ f
                         {
                           ep with
                           ep_witness = Lin.create c.out_choice_next_wit;
                         })
            | None -> loop cs)
        | [] -> failwith ""
      in
      loop out.out_choices

    let receive : ([> ] as 'a) inp ep -> ('a -> unit) -> unit io =
     fun ep f (*fun x -> x#a*) ->
      let inp = Lin.get ep.ep_witness in
      Io.bind
        (Endpoint.receive ep.ep_raw ~role:inp.inp_role
           ~connection:inp.inp_connection) (fun (label, v) ->
          let (InpChoice c) = Hashtbl.find inp.inp_choices label in
          let v = c.inp_choice_marshal v in
          Io.return
          @@ f
               (c.inp_choice_variant.make_var (*fun x -> `lab x*)
                  (v, { ep with ep_witness = Lin.create c.inp_choice_next_wit })))

    let close (ep : unit ep) =
      ignore @@ Lin.get ep.ep_witness;
      Endpoint.close ep.ep_raw
  end
end
