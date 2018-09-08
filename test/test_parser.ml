open Stream
open Unitlex
open Types
open Lexer
open Parser

let t1 () =
  let i1 = "const : lit -> expr" in
  let i2 = "const : lit * lit2 -> expr" in
  let i3 = "const : : lit -> expr" in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in
  let s3 = analex (stream_of_string i3) in

  let ps_expr =
    Program { ps_name = "expr";
              input_type = Flow "a";
              output_type = Flow "b" }
  in

  let e1 =
    { c_name = "const";
      c_input_sorts = [Base "lit"];
      c_output_sort = ps_expr }
  in
  let e2 =
    { c_name = "const";
      c_input_sorts = [Base "lit"; Base "lit2"];
      c_output_sort = ps_expr }
  in

  let initial_state =
    let s = new_state() in
    { s with
      base_sorts = [Base "lit"; Base "lit2"];
      program_sorts = [ps_expr] }
  in

  let ok1 =
    match parse_constructor initial_state s1 with
    | None -> false
    | Some (state, _) -> state.constructors = [e1]
  in
  let ok2 =
    match parse_constructor initial_state s2 with
    | None -> false
    | Some (state, _) -> state.constructors = [e2]
  in
  let ok3 =
    parse_constructor initial_state s3 = None
  in
  assert (ok1 && ok2 && ok3)

let t2 () =
  let i1 = "a = in * out" in
  let i2 = "a = in * error * out" in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in

  let e1 =
    Program { ps_name = "a"; input_type = Flow "in"; output_type = Flow "out" }
  in

  let ok1 =
    match parse_program_sort (new_state()) s1 with
    | None -> false
    | Some (state, _) -> state.program_sorts = [e1]
  in
  let ok2 =
    parse_program_sort (new_state()) s2 = None
  in
  assert (ok1 && ok2)

let t3 () =
  let i1 = "myfilter : t1 * t2 -> t3" in
  let i2 = "myfilter : t1 -> t2 * t3" in
  let i3 = "myfilter : t1 -> t2 -> t3" in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in
  let s3 = analex (stream_of_string i3) in

  let t1 = Base "t1" in
  let t2 =
    Program { ps_name = "t2";
              input_type = Flow "i";
              output_type = Flow "o" }
  in
  let t3 = Flow "t3" in

  let initial_state =
    let s = new_state() in
    { s with
      base_sorts = [t1];
      flow_sorts = [t3];
      program_sorts = [t2] }
  in

  let e1 =
    { fs_name = "myfilter";
      fs_input_sorts = [t1; t2] ;
      fs_output_sorts = [t3] }
  in
  let e2 =
    { fs_name = "myfilter";
      fs_input_sorts = [t1] ;
      fs_output_sorts = [t2; t3] }
  in

  let ok1 =
    match parse_filter initial_state s1 with
    | None -> false
    | Some (state, _) -> state.filters = [e1]
  in
  let ok2 =
    match parse_filter initial_state s2 with
    | None -> false
    | Some (state, _) -> state.filters = [e2]
  in
  let ok3 =
    parse_filter initial_state s3 = None
  in
  assert (ok1 && ok2 && ok3)

let t4 () =
  let i1 = "myatom : t1 -> t2" in
  let i2 = "myatom : t1 -> t2 -> t3" in
  let i3 = "myatom : t1 -> t2 * t3" in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in
  let s3 = analex (stream_of_string i3) in

  let e1, e2 = i1, i2 in

  let ok1 =
    match parse_atom (new_state()) s1 with
    | None -> false
    | Some (state, _) -> state.atoms = [e1]
  in
  let ok2 =
    match parse_atom (new_state()) s2 with
    | None -> false
    | Some (state, _) -> state.atoms = [e2]
  in
  let ok3 =
    parse_atom (new_state()) s3 = None
  in
  assert (ok1 && ok2 && ok3)

let t5 () =
  let i1 = "H(s1, term, s2)" in
  let i2 = "H(s1, term, s2, s3)" in
  let i3 = "H(s1, term)" in
  let i4 = "H(s1, c x y, s2)" in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in
  let s3 = analex (stream_of_string i3) in
  let s4 = analex (stream_of_string i4) in

  let o =
    Program { ps_name = "out"; input_type = Flow "u"; output_type = Flow "v" }
  in

  let c =
    { c_name = "c";
      c_input_sorts = [Base "i1"; Base "i2"];
      c_output_sort = o }
  in

  let e1 = { state = FVar "s1"; term = Vterm "term"; output = FVar "s2" } in
  let e4 =
    { state = FVar "s1";
      term = Cterm (c, [Vterm "x"; Vterm "y"]);
      output = FVar "s2" }
  in

  let initial_state =
    let s = new_state() in { s with constructors = [c] }
  in

  let ok1 =
    match parse_hook initial_state s1 with
    | None -> false
    | Some (hook, _) -> hook = e1
  in
  let ok2 = parse_hook initial_state s2 = None in
  let ok3 = parse_hook initial_state s3 = None in
  let ok4 =
    match parse_hook initial_state s4 with
    | None -> false
    | Some (hook, _) -> hook = e4
  in
  assert (ok1 && ok2 && ok3 && ok4)

let t6 () =
  let i1 = "filter(x_f1, x_t2) ?> (x_f2, x_t2')" in
  let i2 = "filter(x_f1) ?> (x_t1)" in
  let i3 = "filter(x_t1) = (x_t2)" in
  let i4 = "filter(x_t1)" in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in
  let s3 = analex (stream_of_string i3) in
  let s4 = analex (stream_of_string i4) in

  let e1 =
    { f_name = "filter";
      f_inputs = [FVar "x_f1"; TVar "x_t2"] ;
      f_outputs = [FVar "x_f2"; TVar "x_t2'"] }
  in
  let e2 =
    { f_name = "filter";
      f_inputs = [FVar "x_f1"] ;
      f_outputs = [TVar "x_t1"] }
  in
  let e4 =
    { f_name = "filter"; f_inputs = [TVar "x_t1"]; f_outputs = [] }
  in

  let ok1 =
    match parse_filter2 s1 with
    | None -> false
    | Some (filter, _) -> filter = e1
  in
  let ok2 =
    match parse_filter2 s2 with
    | None -> false
    | Some (filter, _) -> filter = e2
  in
  let ok3 =
    match parse_filter2 s3 with
    | None -> false
    | Some (filter, _) -> filter = e4
    (* it stops reading before the end and assumes an empty filter *)
  in
  let ok4 =
    match parse_filter2 s4 with
    | None -> false
    | Some (filter, _) -> filter = e4
  in
  assert (ok1 && ok2 && ok3 && ok4)

let t7 () =
  let i1 = "[filter(x_f1) ?> (x_f2)]" in
  let i2 = "[H(x_s, x_t1, x_t2); filter(x_f1) ?> (x_f2)]" in
  let i3 =
    "[H(x_s, x_t1, x_t2); filter(x_f1) ?> (x_f2); [| [filter(x_f1) ?> (x_f2);"
    ^ " H(x_s, x_t1, x_t2)] || [filter(x_t1); H(x_s, x_t1, x_t2)] |]{x_o}]"
  in
  let s1 = analex (stream_of_string i1) in
  let s2 = analex (stream_of_string i2) in
  let s3 = analex (stream_of_string i3) in

  let f =
    { f_name = "filter"; f_inputs = [FVar "x_f1"]; f_outputs = [FVar "x_f2"] }
  in
  let f2 =
    { f with f_inputs = [TVar "x_t1"]; f_outputs = [] }
  in
  let h = { state = FVar "x_s"; term = Vterm "x_t1"; output = FVar "x_t2" } in
  let b =
    { branches = [[Filter f; Hook h]; [Filter f2; Hook h]];
      outputs = [FVar "x_o"] }
  in

  let e1 = [Filter f] in
  let e2 = [Hook h; Filter f] in
  let e3 = [Hook h; Filter f; Branching b] in

  let ok1 =
    match parse_skeleton parse_branching (new_state()) s1 with
    | None -> false
    | Some (st, _) -> st = e1
  in
  let ok2 =
    match parse_skeleton parse_branching (new_state()) s2 with
    | None -> false
    | Some (st, _) -> st = e2
  in
  let ok3 =
    match parse_skeleton parse_branching (new_state()) s3 with
    | None -> false
    | Some (st, _) -> st = e3
  in
  assert (ok1 && ok2 && ok3)

let t8 () =
  let i1 =
    "rule(const x_t1 x_t2) = [H(x_s, x_t1, x_t2); filter(x_f1) ?> (x_f2); "
    ^ "[| [filter(x_f1) ?> (x_f2); H(x_s, x_t1, x_t2)] || [filter(x_t1); "
    ^ "H(x_s, x_t1, x_t2)] |]{x_o}]"
  in
  let s1 = analex (stream_of_string i1) in

  let f =
    { f_name = "filter"; f_inputs = [FVar "x_f1"]; f_outputs = [FVar "x_f2"] }
  in
  let f2 =
    { f with f_inputs = [TVar "x_t1"]; f_outputs = [] }
  in
  let h = { state = FVar "x_s"; term = Vterm "x_t1"; output = FVar "x_t2" } in
  let b =
    { branches = [[Filter f; Hook h]; [Filter f2; Hook h]];
      outputs = [FVar "x_o"] }
  in

  let o =
    Program { ps_name = "out"; input_type = Flow "u"; output_type = Flow "v" }
  in
  let c =
    { c_name = "const";
      c_input_sorts = [Base "i1"; Base "i2"];
      c_output_sort = o }
  in

  let r =
    { r_name = "rule";
      constructor = c;
      constructor_arguments = [TVar "x_t1"; TVar "x_t2"];
      skeleton = [Hook h; Filter f; Branching b] }
  in

  let initial_state =
    let s = new_state() in
    { s with constructors = [c] }
  in

  let ok =
    match parse_rule initial_state s1 with
    | None -> false
    | Some (state, _) -> state.rules = [r]
  in
  assert ok

let t9 () =
  let bs = "lit\n" in
  let fs = "state\nvalue\nbool\n" in
  let ps = "expr = state * value\nstat = state * state\n" in
  let c_sig = "const : lit -> expr\n" in
  let if_sig = "if : expr * stat * stat -> stat\n" in
  let ltv_sig = "litToVal : lit -> value\n" in
  let isb_sig = "isBool : value -> bool\n" in
  let ist_sig = "isTrue : bool -> unit\n" in
  let isf_sig = "isFalse : bool -> unit\n" in
  let a = "initial_state : unit -> state" in

  let r1 = "LitInt(const x_t) = [litToVal(x_t) ?> (x_o)]\n" in
  let r2 =
    "If(if x_t1 x_t2 x_t3) = [H(x_s, x_t1, x_f1); isBool(x_f1) ?> (x_f1'); "
    ^ "[| [isTrue(x_f1'); H(x_s, x_t2, x_o)] || [isFalse(x_f1'); "
    ^ "H(x_s, x_t3, x_o)] |]{x_o}]"
  in

  let i1 =
    (Printf.sprintf "BASE\n%sFLOW\n%sPROGRAM\n%s- constructors\n%s"
      bs fs ps (c_sig ^ if_sig))
    ^
    (Printf.sprintf "- filters\n%s- atoms\n%s\n- rules\n%s"
      (ltv_sig ^ isb_sig ^ ist_sig ^ isf_sig) a (r1 ^ r2))
  in
  let s1 = analex (stream_of_string i1) in

  let bs = [Base "lit"] in
  let fs = [Flow "state"; Flow "value"; Flow "bool"] in
  let expr =
    Program { ps_name = "expr";
              input_type = Flow "state";
              output_type = Flow "value" }
  in
  let stat =
    Program { ps_name = "stat";
              input_type = Flow "state";
              output_type = Flow "state" }
  in
  let ps = [expr; stat] in

  let const_sig =
    { c_name = "const";
      c_input_sorts = [Base "lit"];
      c_output_sort = expr }
  in
  let if_sig =
    { c_name = "if";
      c_input_sorts = [expr; stat; stat];
      c_output_sort = stat }
  in
  let c = [const_sig; if_sig] in

  let ltv_sig =
    { fs_name = "litToVal";
      fs_input_sorts = [Base "lit"];
      fs_output_sorts = [Flow "value"] }
  in
  let isb_sig =
    { fs_name = "isBool";
      fs_input_sorts = [Flow "value"];
      fs_output_sorts = [Flow "bool"] }
  in
  let ist_sig =
    { fs_name = "isTrue";
      fs_input_sorts = [Flow "bool"];
      fs_output_sorts = [] }
  in
  let isf_sig =
    { fs_name = "isFalse";
      fs_input_sorts = [Flow "bool"];
      fs_output_sorts = [] }
  in
  let f = [ltv_sig; isb_sig; ist_sig; isf_sig] in

  let a = [a] in

  let r1 =
    { r_name = "LitInt";
      constructor = const_sig;
      constructor_arguments = [TVar "x_t"];
      skeleton =
        [ Filter
            { f_name = "litToVal";
              f_inputs = [TVar "x_t"];
              f_outputs = [FVar "x_o"] } ] }
  in
  let ist = { f_name = "isTrue"; f_inputs = [FVar "x_f1'"]; f_outputs = [] } in
  let isf =
    { f_name = "isFalse"; f_inputs = [FVar "x_f1'"]; f_outputs = [] }
  in
  let h1 = { state = FVar "x_s"; term = Vterm "x_t2"; output = FVar "x_o" } in
  let h2 = { state = FVar "x_s"; term = Vterm "x_t3"; output = FVar "x_o" } in
  let r2 =
    { r_name = "If";
      constructor = if_sig;
      constructor_arguments = [TVar "x_t1"; TVar "x_t2"; TVar "x_t3"];
      skeleton =
        [ Hook
            { state = FVar "x_s";
              term = Vterm "x_t1";
              output = FVar "x_f1" };
          Filter
            { f_name = "isBool";
              f_inputs = [FVar "x_f1"];
              f_outputs = [FVar "x_f1'"] };
          Branching
            { branches =
              [ [Filter ist; Hook h1];
                [Filter isf; Hook h2] ];
              outputs = [FVar "x_o"] } ] }
  in
  let r = [r1; r2] in

  let ok =
    let state = parse s1 in
    state = (bs, fs, ps, c, f, a, r)
  in
  assert ok

let () =
  let test_func i test =
    try
      test();
      Printf.printf "Success %d\n" (i+1)
    with _ -> Printf.printf "Failure %d\n" (i+1)
  in
  let tests = [t1; t2; t3; t4; t5; t6; t7; t8; t9] in
  List.iteri test_func tests