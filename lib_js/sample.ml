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
            ( "b",
              InpRole
                {
                  inp_role_constr = b;
                  inp_role_path = "";
                  inp_role_path_kind = `Greeting;
                  inp_role_parse_label = assert false;
                  inp_role_labels =
                    [
                      ( "lab",
                        InpLabel
                          {
                            inp_label_constr = lab;
                            inp_label_parse_payload = assert false;
                            inp_label_cont =
                              Lazy.from_val
                                (Out
                                   {
                                     methods =
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
                                                 out_role = "b";
                                                 out_label = "lab2";
                                                 out_marshal = assert false;
                                                 out_cont = Lazy.from_val Close;
                                               }

                                             method lab3 =
                                               {
                                                 out_role = "b";
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
