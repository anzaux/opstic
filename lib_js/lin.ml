type 'a t = { lin_val : 'a; mutable lin_flag : bool }

exception LinearityViolation

let create x = { lin_val = x; lin_flag = true }

let get x =
  (* no race since we are in the js_of_ocaml world *)
  if x.lin_flag then (
    x.lin_flag <- false;
    x.lin_val)
  else raise LinearityViolation
