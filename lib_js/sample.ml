open Types
open Rows
open Monad.Common
open Kxclib

[%%declare_constr a]
[%%declare_constr b]
[%%declare_constr args]
[%%declare_constr ans]
[%%declare_constr err]

let parse_x_y : payload -> (float * float) io =
 fun payload ->
  match payload |> Jv.(pump_field "y" &> pump_field "x") with
  | `obj [ ("args", `obj [ ("x", `num x); ("y", `num y) ]) ] -> return (x, y)
  | _ -> Monad.error_with ("parse error: " ^ Json.unparse payload)

let unparse_ans sessionid _label ans =
  return
  @@ `obj
       [
         ("session_id", `str (SessionId.to_string sessionid)); ("ans", `num ans);
       ]

let unparse_msg sessionid _label msg =
  return
  @@ `obj
       [
         ("session_id", `str (SessionId.to_string sessionid)); ("err", `str msg);
       ]

let sample1 () =
  let rec wit_a =
    let open Witness in
    lazy
      (Witness.make_inp
         [
           Witness.make_inp_role ~path_kind:`Greeting
             ~parse_label:(Witness.parse_label_default [ "args" ])
             ~path:(Path.create "/adder") ~constr:b
             [ Witness.make_inp_label ~constr:args ~parse:parse_x_y out ];
         ]
        : [< `b of [< `args of _ ] ] inp witness)
  (* NB this type annotation is mandatory for session-type safety *)
  and out =
    let open Witness in
    lazy
      (Witness.make_out
         ~labels:
           [
             Method { role = (fun x -> x#b); label = (fun x -> x#ans) };
             Method { role = (fun x -> x#b); label = (fun x -> x#err) };
           ]
         (object
            method b =
              object
                method ans =
                  {
                    out_role = Role.create "b";
                    out_label = "ans";
                    out_unparse = unparse_ans;
                    out_cont = wit_a;
                  }

                method err =
                  {
                    out_role = Role.create "b";
                    out_label = "err";
                    out_unparse = unparse_msg;
                    out_cont = Lazy.from_val Witness.close;
                  }
              end
         end))
  in
  Witness.create_service_spec ~id:"sample0" ~my_role:"a" ~other_roles:[ "b" ]
    (Lazy.force wit_a)
