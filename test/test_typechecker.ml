open Stream
open Types
open Lexer
open Parser
open Typechecker

(* getlines ic takes all the lines available in the input channel and puts
   them in a string.
*)
let getlines ic =
  let rec f acc =
    try f ((input_line ic) :: acc) with End_of_file -> String.concat "\n" (List.rev acc)
  in f []

let t1 () =
  let gamma = [(FVar "x", Base "tx"); (TVar "y", Flow "ty")] in
  let g = make_gamma_f gamma in
  assert (g (FVar "x") = Base "tx");
  assert (g (TVar "y") = Flow "ty");
  let () =
    try
      ignore (g (TVar "x"));
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  try
    ignore (g (FVar "y"));
    failwith "X"
  with Failure s -> if s = "X" then assert false

let t2 () =
  let o1 =
    Program { ps_name = "o1";
              input_type = Flow "o1i";
              output_type = Flow "o1o" }
  in
  let o2 =
    Program { ps_name = "o2";
              input_type = Flow "o2i";
              output_type = Flow "o2o" }
  in
  let c1 =
    { c_name = "c1";
      c_input_sorts = [Base "i1"; Base "i2"];
      c_output_sort = o1 }
  in
  let c2 =
    { c_name = "c2";
      c_input_sorts = [o1];
      c_output_sort = o2 }
  in
  let gamma =
    [ (TVar "a", Base "i1");
      (TVar "b", Base "i2");
      (TVar "d", Base "i1") ]
  in
  let t1 = Cterm (c1, [Vterm "a"; Vterm "b"]) in
  let t2 = Cterm (c1, [Vterm "a"; Vterm "d"]) in
  let t3 = Cterm (c2, [Vterm "d"]) in
  let t4 = Cterm (c2, [t1]) in
  let t5 = Cterm (c2, [t2]) in
  assert (sort gamma t1 = o1);
  let () =
    try
      ignore (sort gamma t2);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      ignore (sort gamma t3);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  assert (sort gamma t4 = o2);
  try
    ignore (sort gamma t5);
    failwith "X"
  with Failure s -> if s = "X" then assert false
    
let t3 () =
  let b =
    Program { ps_name = "b";
              input_type = Flow "";
              output_type = Flow "" }
  in
  check_sort_type (TVar "x") (Base "a");
  check_sort_type (TVar "x") (b);
  check_sort_type (FVar "x") (Flow "c");
  let () =
    try
      check_sort_type (TVar "x") (Flow "d");
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      check_sort_type (FVar "x") (Base "e");
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  try
    check_well_formed_gamma [(TVar "x", Base "i"); (FVar "y", b)];
    failwith "X"
  with Failure s -> if s = "X" then assert false

let t4 () =
  let h1 = { state = FVar "x"; term = Vterm "u"; output = FVar "y" } in
  let h2 = { state = TVar "x"; term = Vterm "u"; output = FVar "y" } in
  let p =
    Program { ps_name = "p"; input_type = Flow "a"; output_type = Flow "b" }
  in
  let d = Domain.of_list [FVar "x"; TVar "u"] in
  let g1 = [(FVar "x", Flow "a"); (TVar "u", p); (TVar "other", Base "z")] in
  let g2 = [(FVar "x", Flow "a"); (TVar "u", p)] in
  let g3 = [(TVar "u", p)] in
  let () =
    try
      ignore (check_well_formed_hook h1 g1 d);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  ignore (check_well_formed_hook h1 g2 d);
  let () =
    try
      ignore (check_well_formed_hook h1 g3 d);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  try
    ignore (check_well_formed_hook h2 g2 d);
    failwith "X"
  with Failure s -> if s = "X" then assert false

let t5 () =
  let fsig1 =
    { fs_name = "f";
      fs_input_sorts = [Flow "a"; Flow "b"];
      fs_output_sorts = [Base "c"] }
  in
  let fsig2 =
    { fs_name = "f";
      fs_input_sorts = [Flow "m"; Flow "b"];
      fs_output_sorts = [Base "c"] }
  in
  let fsig3 =
    { fs_name = "f";
      fs_input_sorts = [Flow "a"; Flow "b"];
      fs_output_sorts = [Base "c"; Base "d"] }
  in
  let fsig4 =
    { fs_name = "fx";
      fs_input_sorts = [Flow "a"; Flow "b"];
      fs_output_sorts = [Base "c"] }
  in
  let f =
    { f_name = "f";
      f_inputs = [FVar "x"; FVar "y"];
      f_outputs = [TVar "z"] }
  in
  let d1 = Domain.of_list [FVar "x"; FVar "y"; TVar "z"] in
  let d2 = Domain.of_list [FVar "x"; FVar "y"] in
  let g = [(FVar "x", Flow "a"); (FVar "y", Flow "b")] in
  let () =
    try
      ignore (check_well_formed_filter f g d1 [fsig1]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      ignore (check_well_formed_filter f g d2 [fsig2]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      ignore (check_well_formed_filter f g d2 [fsig3]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      ignore (check_well_formed_filter f g d2 [fsig4]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  ignore (check_well_formed_filter f g d2 [fsig1])

let t6 () =
  let p1 =
    Program { ps_name = "p1"; input_type = Flow "a"; output_type = Flow "b" }
  in
  let p2 =
    Program { ps_name = "p2"; input_type = Flow "a"; output_type = Flow "c" }
  in
  let g1 = [(FVar "x_f1", Flow "a"); (TVar "x_t", p1)] in
  let g2 = [(FVar "x_f1", Flow "a"); (TVar "x_t", p2)] in
  let d = Domain.of_list [FVar "x_f1"; TVar "x_t"] in
  let f1sig =
    { fs_name = "f1";
      fs_input_sorts = [Flow "a"];
      fs_output_sorts = [] }
  in
  let f2sig =
    { fs_name = "f2";
      fs_input_sorts = [Flow "b"];
      fs_output_sorts = [Base "c"; Base "d"] }
  in
  let f1 =
    { f_name = "f1";
      f_inputs = [FVar "x_f1"];
      f_outputs = [] }
  in
  let f1' =
    { f_name = "f1";
      f_inputs = [FVar "x_f3"];
      f_outputs = [] }
  in
  let f2 =
    { f_name = "f2";
      f_inputs = [FVar "x_f2"];
      f_outputs = [TVar "x_t2"; TVar "x_t3"] }
  in
  let h = { state = FVar "x_f1"; term = Vterm "x_t"; output = FVar "x_f2" } in
  let sk1 = [Hook h; Filter f1; Filter f2] in
  let sk2 = [Hook h; Filter f1'; Filter f2] in
  let () =
    try
      ignore (check_well_formed_skeleton sk2 g1 d [f1sig; f2sig]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      ignore (check_well_formed_skeleton sk1 g2 d [f1sig; f2sig]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  ignore (check_well_formed_skeleton sk1 g1 d [f1sig; f2sig])

let t7 () =
  let p1 =
    Program { ps_name = "p1"; input_type = Flow "a"; output_type = Flow "c" }
  in
  let p2 =
    Program { ps_name = "p2"; input_type = Flow "s"; output_type = Flow "x" }
  in
  let fsig =
    { fs_name = "f"; fs_input_sorts = [Flow "c"]; fs_output_sorts = [p2] }
  in

  let h1 =
    { state = FVar "x_f1"; term = Vterm "x_t1"; output = FVar "x_f2" }
  in
  let f =
    { f_name = "f"; f_inputs = [FVar "x_f2"]; f_outputs = [TVar "x_t3"] }
  in
  let h2 =
    { state = FVar "x_s"; term = Vterm "x_t3"; output = FVar "x_o" }
  in

  let h1' =
    { state = FVar "x_f1"; term = Vterm "x_t1"; output = FVar "x_f3" }
  in
  let f' =
    { f_name = "f"; f_inputs = [FVar "x_f3"]; f_outputs = [TVar "x_t4"] }
  in
  let h2' =
    { state = FVar "x_s"; term = Vterm "x_t4"; output = FVar "x_o" }
  in

  let sk1 = [Hook h1; Filter f; Hook h2] in
  let sk2 = [Hook h1; Filter f] in
  let sk3 = [Hook h1'; Filter f'; Hook h2'] in

  let br1 = [sk1; sk1] in
  let br2 = [sk1; sk2] in
  let br3 = [sk1; sk3] in

  let o = [FVar "x_o"] in
  let g =
    [ (FVar "x_f1", Flow "a");
      (TVar "x_t1", p1);
      (FVar "x_s", Flow "s") ]
  in
  let d = Domain.of_list [FVar "x_f1"; TVar "x_t1"; FVar "x_s"] in

  let b1 = { branches = br1; outputs = o } in
  let b2 = { branches = br2; outputs = o } in
  let b3 = { branches = br3; outputs = o } in

  let () =
    try
      ignore (check_well_formed_branching b1 g d [fsig]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  let () =
    try
      ignore (check_well_formed_branching b2 g d [fsig]);
      failwith "X"
    with Failure s -> if s = "X" then assert false
  in
  ignore (check_well_formed_branching b3 g d [fsig])

let t8 () =
  let _, _, _, const, filtersig, _, rules =
    open_in "test/while_rules.txt"
    |> getlines
    |> stream_of_string
    |> analex
    |> parse
  in
  well_formedness_interpretation const filtersig rules

let () =
  let test_func i test =
    try
      test();
      Printf.printf "Success %d\n" (i+1)
    with _ -> Printf.printf "Failure %d\n" (i+1)
  in
  let tests = [t1; t2; t3; t4; t5; t6; t7; t8] in
  List.iteri test_func tests