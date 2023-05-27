open Direct

module MarshalPayload = struct
  type t = payload

  let to_dyn v : t = [ ("payload", Marshal.to_bytes v []) ]

  let from_dyn = function
    | [ ("payload", bytes) ] -> Marshal.from_bytes bytes 0
    | _ -> failwith "bad protocol"
end

module SynchronousChannel = struct
  type t = (string * payload) Event.channel

  let send ch v = Event.sync (Event.send ch v)
  let receive ch = Event.sync (Event.receive ch)
  let create = Event.new_channel
end

module SimpleMpstChannel = SimpleMpstChannel_Make (SynchronousChannel)

let test_make_witness_ab () =
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
                              out_marshal = MarshalPayload.to_dyn;
                              out_next_wit = ();
                            }

                          method lab3 =
                            {
                              out_role = "b";
                              out_label = "lab3";
                              out_marshal = MarshalPayload.to_dyn;
                              out_next_wit = ();
                            }
                        end
                    end;
                  choice_marshal = MarshalPayload.from_dyn;
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
              out_marshal = (MarshalPayload.to_dyn : int -> payload);
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
                              choice_marshal =
                                (MarshalPayload.from_dyn : payload -> unit);
                            } );
                        ( "lab3",
                          Choice
                            {
                              choice_label = "lab3";
                              choice_variant = { make_var = (fun x -> `lab3 x) };
                              choice_next_wit = ();
                              choice_marshal =
                                (MarshalPayload.from_dyn : payload -> unit);
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
  ( Endpoint.{ ep_raw = raw_ep_a; ep_witness = Lin.create wit_a },
    Endpoint.{ ep_raw = raw_ep_b; ep_witness = Lin.create wit_b } )

open Comm

let debug str =
  (* ignore str *)
  print_endline str

let%test "test_comm" =
  let thread_a ep_a () =
    let ep = ep_a in
    debug "thread A: receiving";
    let (`lab (_, ep)) = receive ep (fun x -> x#b) in
    debug "thread A: sending";
    if Random.bool () then close @@ send ep (fun x -> x#b#lab2) ()
    else close @@ send ep (fun x -> x#b#lab3) ()
  in
  let thread_b ep_b () =
    let ep = ep_b in
    debug "thread B: sending";
    let ep = send ep (fun x -> x#a#lab) 10 in
    debug "thread B: receiving";
    match receive ep (fun x -> x#a) with
    | `lab2 (_, ep) ->
        debug "thread B: received lab2";
        close ep
    | `lab3 (_, ep) ->
        debug "thread B: received lab3";
        close ep
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
    let f ep = send ep (fun x -> x#a#lab) () in
    ignore @@ Thread.create (fun () -> ignore @@ receive ep_a (fun x -> x#b)) ();
    ignore @@ f ep_b;
    ignore @@ f ep_b;
    debug "linearity violation detection failure";
    false
  with Lin.LinearityViolation -> true
