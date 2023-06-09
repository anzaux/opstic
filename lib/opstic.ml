type ('m, 'x) variant = { make_var : 'x -> 'm }
type connection = Connected | Join | JoinCorrelation
type payload

module Witness = struct end
(* callbackスタイル *)

type finish = Finish

let (spin :
      ([> `A of
          [> `lab of
             int ->
             next:
               ([> `B of
                   [> `lab2 of string -> next:(finish -> finish) -> finish ] ] ->
               finish) ->
             finish ] ] ->
      finish) ->
      unit) =
 fun f ->
  ignore
    (f
       (`A
         (`lab
           (fun _x ~next -> next (`B (`lab2 (fun _str ~next -> next Finish)))))))

type 'm choice =
  | Choice : {
      choice_label : string;
      choice_marshal : payload -> 'v;
      choice_variant : ('m, 'v * ('cont -> unit)) variant;
      choice_next_wit : 'cont;
    }
      -> 'm choice

type 'a inp = { choices : 'a choice list } constraint 'a = [> ]

let () =
  spin @@ function
  | `A (`lab f) ->
      f 123 ~next:(function `B (`lab2 g) -> g "foobar" ~next:(fun x -> x))
