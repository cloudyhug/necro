open Arith

module Input = struct
  type literal = int
  type state = unit
  type value = Value of literal
  type (_, _) term = 
    | C : literal -> (state, value) term
    | P : expr * expr -> (state, value) term
    | M : expr * expr -> (state, value) term
    | T : expr * expr -> (state, value) term
    | D : expr * expr -> (state, value) term
  and expr = (state, value) term
  let print_value (Value l) = Printf.printf "value = %d\n" l
  let mkstate () = ()
  let litToVal l = Some (Value l)
  let valToLit (Value l) = Some l
  let plus l1 l2 = Some (Value (l1 + l2))
  let minus l1 l2 = Some (Value (l1 - l2))
  let times l1 l2 = Some (Value (l1 * l2))
  let div l1 l2 = if l2 = 0 then None else Some (Value (l1 / l2))
end

module InterpArith : (INTERPRETER with type literal = int) = MakeInterpreter(Input)

open InterpArith

let () =
  let three = C 3 in
  let twelve = C 12 in
  let t1 = M (P (three, D (T (twelve, twelve), three)), twelve) in (* 3 + 12 * 12 / 3 - 12 *)
  let s = mkstate() in
  let v = eval s t1 in
  print_value v