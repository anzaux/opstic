open Types
open Rows

module Marshal = struct
  let from_dyn : payload -> 'a = Obj.magic
  let to_dyn : 'a -> payload = Obj.magic
end

[%%declare_constr a]
[%%declare_constr b]
[%%declare_constr lab]
[%%declare_constr lab2]
[%%declare_constr lab3]

let sample1 () =
  let wit_a =
    let open Witness in
    (Lazy.from_val
     @@ make_inp
          [
            InpRole
              {
                role_constr = b;
                path_spec =
                  {
                    path = Path.create "";
                    path_kind = `Greeting;
                    path_role = Role.create "b";
                  };
                parse_label = assert false;
                labels =
                  [
                    ( "lab",
                      InpLabel
                        {
                          label_constr = lab;
                          parse_payload = assert false;
                          cont =
                            Lazy.from_val
                              (Witness.make_out
                                 ~labels:
                                   [
                                     Method
                                       {
                                         role = (fun x -> x#b);
                                         label = (fun x -> x#lab2);
                                       };
                                     Method
                                       {
                                         role = (fun x -> x#b);
                                         label = (fun x -> x#lab3);
                                       };
                                   ]
                                 (object
                                    method b =
                                      object
                                        method lab2 =
                                          {
                                            out_role = Role.create "b";
                                            out_label = "lab2";
                                            out_unparse = assert false;
                                            out_cont =
                                              Lazy.from_val Witness.close;
                                          }

                                        method lab3 =
                                          {
                                            out_role = Role.create "b";
                                            out_label = "lab3";
                                            out_unparse = assert false;
                                            out_cont =
                                              Lazy.from_val Witness.close;
                                          }
                                      end
                                 end));
                        } );
                  ];
              };
          ]
      : [< `b of [< `lab of _ ] ] inp witness lazy_t)
    (* NB this type annotation is mandatory for session-type safety *)
  in
  Witness.create_service_spec ~id:"sample0" ~my_role:"a" ~other_roles:[ "b" ]
    (Lazy.force wit_a)
