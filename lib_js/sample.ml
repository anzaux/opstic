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
    Inp
      (Witness.make_inp ~kind:`Greeting ~subpath:""
         [
           InpChoice
             {
               inp_choice_role = b;
               inp_choice_label = lab;
               inp_choice_next_wit =
                 Out
                   {
                     methods =
                       [
                         Method
                           {
                             role = (fun obj -> obj#b);
                             label = (fun obj -> obj#lab2);
                           };
                         Method
                           {
                             role = (fun obj -> obj#b);
                             label = (fun obj -> obj#lab3);
                           };
                       ];
                     obj =
                       object
                         method b =
                           object
                             method lab2 =
                               Witness.make_out ~role:"b" ~label:"lab2"
                                 ~marshal:Marshal.to_dyn Close

                             method lab3 =
                               Witness.make_out ~role:"b" ~label:"lab3"
                                 ~marshal:Marshal.to_dyn Close
                           end
                       end;
                   };
               inp_choice_marshal = Marshal.from_dyn;
             };
         ]
        : [< `b of _ ] inp)
    (* NB this type annotation is mandatory for session-type safety *)
  and wit_b =
    let open Witness in
    Out
      {
        methods =
          [ Method { role = (fun obj -> obj#a); label = (fun obj -> obj#lab) } ];
        obj =
          object
            method a =
              object
                method lab =
                  Witness.make_out ~kind:`Greeting ~role:"a" ~label:"lab"
                    ~marshal:(Marshal.to_dyn : int -> payload)
                    (Inp
                       (Witness.make_inp ~subpath:""
                          [
                            InpChoice
                              {
                                inp_choice_role =
                                  (a : ([< `a of _ ], _) Rows.constr);
                                inp_choice_label = lab2;
                                inp_choice_next_wit = Close;
                                inp_choice_marshal =
                                  (Marshal.from_dyn : payload -> unit);
                              };
                            InpChoice
                              {
                                inp_choice_role =
                                  (a : ([< `a of _ ], _) Rows.constr);
                                inp_choice_label = lab3;
                                inp_choice_next_wit = Close;
                                inp_choice_marshal =
                                  (Marshal.from_dyn : payload -> unit);
                              };
                          ]
                         : [< `a of _ ] inp))
              end
          end;
      }
  in
  (wit_a, wit_b)
