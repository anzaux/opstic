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
     @@ Inp
          [
            ( Role.create "b",
              InpRole
                {
                  role_constr = b;
                  path = "";
                  path_kind = `Greeting;
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
                                (Out
                                   {
                                     labels =
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
                                       ];
                                     obj =
                                       object
                                         method b =
                                           object
                                             method lab2 =
                                               {
                                                 out_role = Role.create "b";
                                                 out_label = "lab2";
                                                 out_marshal = assert false;
                                                 out_cont = Lazy.from_val Close;
                                               }

                                             method lab3 =
                                               {
                                                 out_role = Role.create "b";
                                                 out_label = "lab3";
                                                 out_marshal = assert false;
                                                 out_cont = Lazy.from_val Close;
                                               }
                                           end
                                       end;
                                   });
                          } );
                    ];
                } );
          ]
      : [< `b of [< `lab of _ ] ] inp witness lazy_t)
    (* NB this type annotation is mandatory for session-type safety *)
  in
  wit_a
