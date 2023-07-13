open Opstic

let ep = "/adder"

let%global g =
  let rec loop =
    a#lab = ep => b :: `obj [ ("x", `num __); ("y", `num __) ];
    b
    *>> ( (b#lab2 ==> a :: `obj [ ("ans", `num __) ];
           loop),
          b#lab3 ==> a :: `obj [ ("msg", `str __) ] )
  in
  loop

let _w = [%project_global g b]

let () =
  print_newline ();
  print_endline g
