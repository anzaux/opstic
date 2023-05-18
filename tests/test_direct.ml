(* open OUnit
open Ojmpst.Direct
open Comm
module EP = Endpoint

let test_comm () =
  let wit_a =
    let open Witness in
    (* EXPERIMENTAL: this is INNEFFICIENT as each method call every time allocates the witness  *)
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
  let thread_a () =
    let ep =
      Endpoint.
        { ep_raw = raw_ep_a; ep_witness = wit_a; ep_close = (fun () -> ()) }
    in
    print_endline "thread A: receiving";
    let (`lab (_, ep)) = receive ep (fun x -> x#b) in
    print_endline "thread A: sending";
    if Random.bool () then close @@ send ep (fun x -> x#b#lab2) ()
    else close @@ send ep (fun x -> x#b#lab2) ()
  in
  let thread_b () =
    let ep =
      Endpoint.
        { ep_raw = raw_ep_b; ep_witness = wit_b; ep_close = (fun () -> ()) }
    in
    print_endline "thread B: sending";
    let ep = send ep (fun x -> x#a#lab) 10 in
    print_endline "thread B: receiving";
    match receive ep (fun x -> x#a) with
    | `lab2 (_, (ep : unit ep)) -> close ep
    | `lab3 (_, (ep : unit ep)) -> close ep
  in
  print_endline "running test";
  flush stdout;
  let ts = [ thread_a; thread_b ] |> List.map (fun f -> Thread.create f ()) in
  ignore
  @@ Thread.create
       (fun () ->
         Unix.sleepf 1.0;
         ignore @@ Unix._exit 1)
       ();
  List.iter Thread.join ts;
  print_endline "done."

let suite = "Running mpst communication" >::: [ "test_comm" >:: test_comm ]

let () =
  let _results = run_test_tt_main suite in
  () *)
