type payload = (string * bytes) list

module SynchronousChannel = struct
  type t = (string * payload) Event.channel
  type 'x io = 'x
  type nonrec payload = payload

  let create = Event.new_channel
  let send ch v = Event.sync (Event.send ch v)
  let receive ch = Event.sync (Event.receive ch)
  let close _ = ()
end

module Io = struct
  type 'x t = 'x

  let return x = x
  let bind x f = f x
end

module Endpoint = Opstic.LocalEndpoint (Io) (SynchronousChannel)
module Direct = Opstic.Make (Io) (Endpoint)
open Direct

module MarshalPayload = struct
  type nonrec payload = payload

  let to_dyn v : payload = [ ("payload", Marshal.to_bytes v []) ]

  let from_dyn : payload -> 'a = function
    | [ ("payload", bytes) ] -> Marshal.from_bytes bytes 0
    | _ -> failwith "bad protocol"
end

module Sample = Opstic.Sample0 (Io) (Endpoint) (MarshalPayload)

let test_make_witness_ab () =
  let wit_a, wit_b = Sample.sample1 () in

  let raw_ep_a, raw_ep_b =
    let raw_endpoints = Endpoint.make [ "a"; "b" ] in
    (List.assoc "a" raw_endpoints, List.assoc "b" raw_endpoints)
  in
  ( { ep_raw = raw_ep_a; ep_witness = Lin.create wit_a },
    { ep_raw = raw_ep_b; ep_witness = Lin.create wit_b } )

open Comm

let debug str =
  (* ignore str *)
  print_endline str

let%test "test_comm" =
  let thread_a ep_a () =
    let ep = ep_a in
    debug "opstic_native: thread A: receiving";
    receive ep
    @@ `b
         (fun (`lab (_, ep)) ->
           debug "thread A: sending";
           if Random.bool () then send ep (`b (`lab2 ((), close)))
           else send ep (`b (`lab3 ((), close))))
  in
  let thread_b ep_b () =
    let ep = ep_b in
    debug "opstic_native: thread B: sending";
    send ep
      (`a
        (`lab
          ( 123,
            fun ep ->
              debug "thread B: receiving";
              receive ep
              @@ `a
                   (function
                   | `lab2 (_, ep) ->
                       debug "thread B: received lab2";
                       close ep
                   | `lab3 (_, ep) ->
                       debug "thread B: received lab3";
                       close ep) )))
  in
  debug "running test";
  Random.init (int_of_float @@ Unix.time ());
  flush stdout;
  let ep_a, ep_b = test_make_witness_ab () in
  let ts =
    [ thread_a ep_a; thread_b ep_b ] |> List.map (fun f -> Thread.create f ())
  in
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
  let ep_a, ep_b = test_make_witness_ab () in
  try
    let f ep_b = send ep_b (`a (`lab (123, fun _ -> return ()))) in
    ignore
    @@ Thread.create
         (fun () ->
           ignore @@ receive ep_a @@ `b (fun (`lab (_, _)) -> return ()))
         ();
    ignore @@ f ep_b;
    ignore @@ f ep_b;
    debug "linearity violation detection failure";
    false
  with Lin.LinearityViolation -> true
