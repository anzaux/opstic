(* let x = 1 - 2 :: [] *)

let%global x =
    let rec f x y =
      a#x ==> b :: (1 + 2) ==> c;
      c#a = s ==> d :: "abc";
      a *>> (f 1 2, ())
    in
    e#z ==> f :: 1.23

let () =
  print_newline ();
  print_endline x
