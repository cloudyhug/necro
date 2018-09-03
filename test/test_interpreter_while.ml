(* while language test : needs an interpreter to be generated in "while.ml" *)

open While

module Flow = struct
  type ident = string
  type lit = int
  type vint = int
  type vbool = bool
  type value = Int of vint | Bool of vbool
  type state = (ident, value) Hashtbl.t
  let initial_state () = Hashtbl.create 29
  let litToVal lit = Some (Int lit)
  let read ident state = Hashtbl.find_opt state ident
  let isInt = function
    | Int i -> Some i
    | _ -> None
  let add a b = Some (Int (a + b))
  let eq a b = Some (Bool (a = b))
  let isBool = function
    | Bool b -> Some b
    | _ -> None
  let neg b = Some (Bool (not b))
  let write ident state value =
    let () = Hashtbl.replace state ident value in
    Some state
  let id state = Some state
  let isTrue b = if b then Some () else None
  let isFalse b = if b then None else Some ()
  let print_value = function
    | Int i -> Printf.printf "%d" i
    | Bool b -> Printf.printf "%B" b
  let print_state state =
    let f id v =
      Printf.printf "%s : " id;
      print_value v;
      print_newline()
    in
    Hashtbl.iter f state
end

module InterpWhile : (INTERPRETER with type ident = string and type lit = int) = MakeInterpreter (Flow)

open InterpWhile

let () =
  let t =
    SemicolonSemicolon (
      ColonEqual ("a", Const 10),
      If (
        EqualEqual (Var "a", Const 9),
        ColonEqual ("x", Const 1),
        ColonEqual ("x", Const 0)
      )
    )
  in
  let s = eval (initial_state()) t in
  print_state s;

  let t =
    SemicolonSemicolon (
      ColonEqual ("n", Const 10),
    SemicolonSemicolon (
      ColonEqual ("a", Const 1),
    SemicolonSemicolon (
      ColonEqual ("b", Const 1),
    SemicolonSemicolon (
      ColonEqual ("c", Const 0),
    SemicolonSemicolon (
      ColonEqual ("i", Const 0),
      While (
        Bang (EqualEqual (Var "i", Var "n")),
        SemicolonSemicolon (
          ColonEqual ("d", Var "b"),
        SemicolonSemicolon (
          ColonEqual ("b", Plus (Var "a", Var "b")),
        SemicolonSemicolon (
          ColonEqual ("a", Var "d"),
        SemicolonSemicolon (
          ColonEqual ("c", Plus (Var "c", Var "b")),
          ColonEqual ("i", Plus (Var "i", Const 1))
        ))))
      )
    )))))
  in
  let s = eval (initial_state()) t in
  print_state s;

  let t =
    SemicolonSemicolon (
      ColonEqual ("over", Const 0),
    SemicolonSemicolon (
      ColonEqual ("fact", Const 1),
    SemicolonSemicolon (
      ColonEqual ("n", Const 10),
      While (
        EqualEqual (Var "over", Const 0),
        If (
          EqualEqual (Var "n", Const 0),
          ColonEqual ("over", Const 1),
          SemicolonSemicolon (
            ColonEqual ("a", Var "fact"),
          SemicolonSemicolon (
            ColonEqual ("i", Const 1),
          SemicolonSemicolon (
            While (
              Bang (EqualEqual (Var "i", Var "n")),
              SemicolonSemicolon (
                ColonEqual ("fact", Plus (Var "fact", Var "a")),
                ColonEqual ("i", Plus (Var "i", Const 1))
              )
            ),
            ColonEqual ("n", Plus (Var "n", Const (-1)))
          )))
        )
      )
    )))
  in
  let s = eval (initial_state()) t in
  print_state s