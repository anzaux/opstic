open Types
open Rows
open Monad
open Kxclib

[%%declare_constr a]
[%%declare_constr b]
[%%declare_constr args]
[%%declare_constr ans]
[%%declare_constr err]

let parse_x_y : payload -> (float * float) io =
 fun payload ->
  match payload |> Jv.(pump_field "y" &> pump_field "x") with
  | `obj [ ("args", `obj [ ("x", `num x); ("y", `num y) ]) ] ->
      Monad.return (x, y)
  | _ -> error_with ("parse error: " ^ Json.unparse payload)

let unparse_ans sessionid _label ans =
  Monad.return
  @@ `obj
       [
         ("sessionid", `str (SessionId.to_string sessionid)); ("ans", `num ans);
       ]

let unparse_msg sessionid _label msg =
  Monad.return
  @@ `obj
       [
         ("sessionid", `str (SessionId.to_string sessionid)); ("err", `str msg);
       ]

let sample1 () =
  let wit_a =
    let open Witness in
    (Lazy.from_val
     @@ Witness.make_inp
          [
            InpRole
              {
                role_constr = b;
                path_spec =
                  {
                    path = Path.create "/adder";
                    path_kind = `Greeting;
                    path_role = Role.create "b";
                  };
                parse_label = Witness.parse_label_default [ "args" ];
                labels =
                  [
                    ( "args",
                      InpLabel
                        {
                          label_constr = args;
                          parse_payload = parse_x_y;
                          cont =
                            Lazy.from_val
                              (Witness.make_out
                                 ~labels:
                                   [
                                     Method
                                       {
                                         role = (fun x -> x#b);
                                         label = (fun x -> x#ans);
                                       };
                                     Method
                                       {
                                         role = (fun x -> x#b);
                                         label = (fun x -> x#err);
                                       };
                                   ]
                                 (object
                                    method b =
                                      object
                                        method ans =
                                          {
                                            out_role = Role.create "b";
                                            out_label = "ans";
                                            out_unparse = unparse_ans;
                                            out_cont =
                                              Lazy.from_val Witness.close;
                                          }

                                        method err =
                                          {
                                            out_role = Role.create "b";
                                            out_label = "err";
                                            out_unparse = unparse_msg;
                                            out_cont =
                                              Lazy.from_val Witness.close;
                                          }
                                      end
                                 end));
                        } );
                  ];
              };
          ]
      : [< `b of [< `args of _ ] ] inp witness lazy_t)
    (* NB this type annotation is mandatory for session-type safety *)
  in
  Witness.create_service_spec ~id:"sample0" ~my_role:"a" ~other_roles:[ "b" ]
    (Lazy.force wit_a)
