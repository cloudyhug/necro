open Stream
open Unitlex
open Types

(* Parser state type : the parser needs to remember the line number and the
   lists of everything it is building (constructors, filters, rules...).
   A 'phase' parameter is added, because the lines in the input file almost
   have the same form, and we do not want the parser to be confused. With this
   parameter, it knows that it must parse a specific thing, and if it does not,
   it raises an error.
*)

type parsing_phase =
  | Nothing
  | BaseSorts
  | FlowSorts
  | ProgramSorts
  | Constructors
  | Filters
  | Atoms
  | Rules

type parser_state =
  { base_sorts : sort list;
    flow_sorts : sort list;
    program_sorts : sort list;
    constructors : constructor_signature list;
    filters : filter_signature list;
    atoms : string list;
    rules : rule list;
    phase : parsing_phase;
    line_number : int;
  }

(* The parsing functions in this file use state machines to read the lexemes.
   Thanks to this, they can use pattern matching on the parse_function_state
   and the lexeme stream, so they know what they have to read next.
*)
type parse_function_state = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9

let sort_of_string_opt sort_name state =
  if base_contains_name sort_name state.base_sorts then
    Some (Base sort_name)
  else if flow_contains_name sort_name state.flow_sorts then
    Some (Flow sort_name)
  else
    get_program_sort_by_name sort_name state.program_sorts

let sort_of_string sort_name state =
  match sort_of_string_opt sort_name state with
  | None -> failwith (Printf.sprintf "Parsing error : undefined sort %s" sort_name)
  | Some s -> s

let var_of_string s =
  let fail () =
    failwith (Printf.sprintf
      "Parsing error : invalid variable name %s" s)
  in
  let beginning =
    try String.sub s 0 3
    with _ -> fail()
  in
  if beginning = "x_f" || s = "x_s" || s = "x_o" then FVar s
  else if beginning = "x_t" then TVar s
  else fail()

(* Base sorts and flow sorts are both just an identifier
   (no state machine needed)
*)
let parse_base_sort state s =
  match take_line_unitlex s with
  | ([Ident n], s') ->
    let state' = { state with base_sorts = (Base n) :: state.base_sorts } in
    Some (state', s')
  | _ -> None

let parse_flow_sort state s =
  match take_line_unitlex s with
  | ([Ident n], s') ->
    let state' = { state with flow_sorts = (Flow n) :: state.flow_sorts } in
    Some (state', s')
  | _ -> None

(* Program sorts have a constant form (no state machine needed) :
   name '=' input_type '*' output_type
*)
let parse_program_sort state s =
  match take_line_unitlex s with
  | ([Ident n; Equal; Ident i; Times; Ident o], s') ->
    let ps =
      Program { ps_name = n; input_type = Flow i; output_type = Flow o }
    in
    let state' = { state with program_sorts = ps :: state.program_sorts } in
    Some (state', s')
  | _ -> None

(* State machine for constructors :
   S0 -- name --> S1 -- ':' --> S2 -- type --> S3 -- '->' --> S4 -- type --> S5
                                ^              |
                                |              |
                                + --- '*' ---- +
   Entry point : S0
   Exit points : S5 (normal case), S3 (no inputs in the constructor)
*)
let parse_constructor state s =
  let rec f name inputs output pfs s =
    match pfs, s with
    | S0, More (ConstName c_name, g)
    | S0, More (Ident c_name, g) -> f (Some c_name) inputs output S1 (g())
    | S1, More (Colon, g) -> f name inputs output S2 (g())
    | S2, More (Ident t, g) -> f name (t :: inputs) output S3 (g())
    | S3, More (Times, g) -> f name inputs output S2 (g())
    | S3, More (Arrow, g) -> f name inputs output S4 (g())
    | S3, Empty -> begin
      match inputs with
      | [o] -> let output = Some o in Some (name, [], output)
      | _ -> None
    end
    | S4, More (Ident t, g) -> f name inputs (Some t) S5 (g())
    | S5, Empty -> Some (name, List.rev inputs, output)
    | _ -> None
  in
  let taken, s' = take_line_unitlex s in
  match f None [] None S0 (stream_of_list taken) with
  | Some (Some name, inputs, Some output) ->
    let g s = sort_of_string s state in
    let new_constructor =
      { c_name = name;
        c_input_sorts = List.map g inputs;
        c_output_sort = g output } in
    let state' = { state with constructors = new_constructor :: state.constructors } in
    Some (state', s')
  | _ -> None

(* State machine for filters :
  S0 -- name --> S1 -- ':' --> S2 -- type --> S3 -- '->' --> S4 -- type --> S5
                              ^              |              ^              |
                              |              |              |              |
                              + --- '*' ---- +              + --- '*' ---- +
   Entry point : S0
   Exit point : S5
*)
let parse_filter state s =
  let rec f name inputs outputs pfs s =
    match pfs, s with
    | S0, More (Ident name, g) -> f (Some name) inputs outputs S1 (g())
    | S1, More (Colon, g) -> f name inputs outputs S2 (g())
    | S2, More (Ident t, g) -> f name (t :: inputs) outputs S3 (g())
    | S3, More (Times, g) -> f name inputs outputs S2 (g())
    | S3, More (Arrow, g) -> f name inputs outputs S4 (g())
    | S4, More (Ident t, g) -> f name inputs (t :: outputs) S5 (g())
    | S5, More (Times, g) -> f name inputs outputs S4 (g())
    | S5, Empty -> Some (name, List.rev inputs, List.rev outputs)
    | _ -> None
  in
  let taken, s' = take_line_unitlex s in
  match f None [] [] S0 (stream_of_list taken) with
  | Some (Some name, input_sorts, output_sorts) ->
    let g s = sort_of_string s state in
    let filter =
      { fs_name = name;
        fs_input_sorts = List.map g input_sorts;
        fs_output_sorts =
          if output_sorts = ["unit"] then []
          else List.map g output_sorts }
    in
    let state' = { state with filters = filter :: state.filters } in
    Some (state', s')
  | _ -> None

(* NB : We do not want to get the separate information for the atoms,
   as they are not used in the interpreter generation. We just store them as
   raw strings.
*)
(* State machine for atoms :
   S0 -- name --> S1 -- ':' --> S2 -- type --> S3
                                ^              |
                                |              |
                                + --- '->' --- +
   Entry point : S0
   Exit point : S3
*)
let parse_atom state s =
  let rec f acc pfs s =
    match pfs, s with
    | S0, More (Ident name, g) -> f (name :: acc) S1 (g())
    | S1, More (Colon, g) -> f (":" :: acc) S2 (g())
    | S2, More (Ident t, g) -> f (t :: acc) S3 (g())
    | S3, More (Arrow, g) -> f ("->" :: acc) S2 (g())
    | S3, Empty -> Some (String.concat " " (List.rev acc))
    | _ -> None
  in
  let taken, s' = take_line_unitlex s in
  match f [] S0 (stream_of_list taken) with
  | None -> None
  | Some atom ->
    let state' = { state with atoms = atom :: state.atoms } in
    Some (state', s')

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* Some of the following functions do not have the 'state' parameter, because
   they do not modify the state as they can be called recursively when reading
   rules, thus they parse things that are not alone on a line in the input
   file. The returned object is the element built from parsing instead of a new
   state. This allows us to stack several of them before building the final
   object (which may be a step list, or a rule).
*)

(* State machine for hooks :
   S0 -- 'H' --> S1 -- '(' --> S2 -- state --> S3 -- ',' --> S4
                                                             |
                                                       const / vterm
                                                             |
                                                             v
                   S8 <-- ')' -- S7 <-- out -- S6 <-- ',' -- S5 <-- arg -- +
                                                             |             |
                                                             + ----------- +
   Entry point : S0
   Exit point : S8
*)
let parse_hook state s =
  let rec f hstate const term output pfs s =
    match pfs, s with
    | S0, More (HookLex, g) -> f hstate const term output S1 (g())
    | S1, More (LParen, g) -> f hstate const term output S2 (g())
    | S2, More (Ident st, g) -> f (Some st) const term output S3 (g())
    | S3, More (Comma, g) -> f hstate const term output S4 (g())
    | S4, More (ConstName t, g)
    | S4, More (Ident t, g) -> f hstate (Some t) term output S5 (g())
    | S5, More (Ident arg, g) -> f hstate const (arg :: term) output S5 (g())
    | S5, More (Comma, g) -> f hstate const term output S6 (g())
    | S6, More (Ident o, g) -> f hstate const term (Some o) S7 (g())
    | S7, More (RParen, g) -> f hstate const term output S8 (g())
    | S8, _ -> Some (hstate, const, List.rev term, output, s)
    | _ -> None
  in
  match f None None [] None S0 s with
  | Some (Some st, Some t, [], Some out, s') ->
    let hook =
      { state = FVar st;
        term = Vterm t;
        output = FVar out }
    in
    Some (hook, s')
  | Some (Some st, Some c, t, Some out, s') ->
    let is_const_called name const = const.c_name = name in
    let const =
      match List.find_opt (is_const_called c) state.constructors with
      | Some const -> const
      | None ->
        failwith (Printf.sprintf
          "Parsing error : undefined constructor %s" c)
    in
    let hook =
      { state = FVar st;
        term = Cterm (const, List.map (fun nt -> Vterm nt) t);
        output = FVar out }
    in
    Some (hook, s')
  | _ -> None

(* State machine for filters (this time the filter is parsed in a rule,
   so this is not its signature like in 'parse_filter', but its use) :
                                 + -- ',' --- +
                                 |            |
                                 v            |
   S0 -- name --> S1 -- '(' --> S2 -- id --> S3 -- ')' --> S4 --> S5
                                                           |
                                                         '?> ('
                                                           |
                                                           v
                              S8 <-- ')' -- S7 <-- out -- S6
                                            |             ^
                                            |             |
                                            + --- ',' --- +
   Entry point : S0
   Exit points : S5, S8
*)
let parse_filter2 s =
  let rec f name inputs outputs pfs s =
    match pfs, s with
    | S0, More (Ident f_name, g) -> f (Some f_name) inputs outputs S1 (g())
    | S1, More (LParen, g) -> f name inputs outputs S2 (g())
    | S2, More (Ident id, g) -> f name (id :: inputs) outputs S3 (g())
    | S3, More (Comma, g) -> f name inputs outputs S2 (g())
    | S3, More (RParen, g) -> f name inputs outputs S4 (g())
    | S4, More (EqualFilter, g) -> begin
      match g() with
      | More (LParen, h) -> f name inputs outputs S6 (h())
      | _ -> None
    end
    | S4, _ -> f name inputs outputs S5 s
    | S5, _ -> Some (name, List.rev inputs, [], s)
    | S6, More (Ident out, g) -> f name inputs (out :: outputs) S7 (g())
    | S7, More (Comma, g) -> f name inputs outputs S6 (g())
    | S7, More (RParen, g) -> f name inputs outputs S8 (g())
    | S8, _ -> Some (name, List.rev inputs, List.rev outputs, s)
    | _ -> None
  in
  match f None [] [] S0 s with
  | Some (Some name, input_vars, output_vars, s') ->
    let filter =
      { f_name = name;
        f_inputs = List.map var_of_string input_vars;
        f_outputs = List.map var_of_string output_vars }
    in
    Some (filter, s')
  | _ -> None

(* NB : parse_skeleton and parse_branching are mutually recursive, so we use an
   additional 'bf' parameter on parse_skeleton to give it the parse_branching
   function.
*)
(* State machine for skeleton
                 /-------- 'H' -----> [parse_hook] --------\
S0 -- '[' --> S1 -- filter name --> [parse_filter2] -------+--> S2 -- ']' --> S3
              ^  \------- '[|' -----> [parse_branching] --/     |
              |                                                 |
              + --------------------- ';' --------------------- +
   Entry point : S0
   Exit point : S3
*)
let parse_skeleton bf state s =
  let rec f acc pfs s =
    match pfs, s with
    | S0, More (LSquareBracket, g) -> f acc S1 (g())
    | S1, More (HookLex, g) -> begin
      match parse_hook state s with
      | None -> None
      | Some (hook, s') -> f ((Hook hook) :: acc) S2 s'
    end
    | S1, More (Ident _, _) -> begin
      match parse_filter2 s with
      | None -> None
      | Some (filter, s') -> f ((Filter filter) :: acc) S2 s'
    end
    | S1, More (BranchStart, _) -> begin
      match bf state s with (* 'bf' will be 'parse_branching', see above *)
      | None -> None
      | Some (branching, s') -> f ((Branching branching) :: acc) S2 s'
    end
    | S2, More (Semicolon, g) -> f acc S1 (g())
    | S2, More (RSquareBracket, g) -> f acc S3 (g())
    | S3, _ -> Some (List.rev acc, s)
    | _ -> None
  in
  f [] S0 s

(* State machine for branchings :
                                                              + -- ',' --- +
                                                              |            |
                                                              v            |
   S0 -- '[|' --> S1 -- [parse_skeleton] --> S2 -- '|]{' --> S3 -- out --> S4
                  ^                          |                             |
                  |                          |                            '}'
                  + --------- '||' --------- +                             |
                                                                           v
                                                                          S5
   Entry point : S0
   Exit point : S5
*)
let rec parse_branching state s =
  let rec f branches outputs pfs s =
    match pfs, s with
    | S0, More (BranchStart, g) -> f branches outputs S1 (g())
    | S1, More (LSquareBracket, _) -> begin
      match parse_skeleton parse_branching state s with
      | None -> None
      | Some (sk, s') -> f (sk :: branches) outputs S2 s'
    end
    | S2, More (BranchChange, g) -> f branches outputs S1 (g())
    | S2, More (BranchEnd, g) -> begin
      match g() with
      | More (LCurlyBracket, h) -> f branches outputs S3 (h())
      | _ -> None
    end
    | S3, More (Ident out, g) -> f branches (out :: outputs) S4 (g())
    | S4, More (Comma, g) -> f branches outputs S3 (g())
    | S4, More (RCurlyBracket, g) -> f branches outputs S5 (g())
    | S5, _ -> Some (List.rev branches, List.rev outputs, s)
    | _ -> None
  in
  match f [] [] S0 s with
  | Some (b, o, s') ->
    let branching =
      { branches = b;
        outputs = List.map var_of_string o }
    in
    Some (branching, s')
  | _ -> None

(* State machine for rules :
                                                 + ------------- +
                                                 |               |
                                                 v               |
   S0 -- name --> S1 -- '(' --> S2 -- const --> S3 ---- type --- +
                                                 |
                                                ')'
                                                 |
                                                 v
              S5 <-- [parse_skeleton] < '=[' -- S4

*)
let parse_rule state s =
  let rec f name const_name args skeleton pfs s =
    match pfs, s with
    | S0, More (ConstName name, g)
    | S0, More (Ident name, g) ->
      f (Some name) const_name args skeleton S1 (g())
    | S1, More (LParen, g) -> f name const_name args skeleton S2 (g())
    | S2, More (ConstName const, g)
    | S2, More (Ident const, g) -> f name (Some const) args skeleton S3 (g())
    | S3, More (Ident t, g) -> f name const_name (t :: args) skeleton S3 (g())
    | S3, More (RParen, g) -> f name const_name args skeleton S4 (g())
    | S4, More (Equal, g) -> begin
      match g() with
      | More (LSquareBracket, _) as h -> begin
        match parse_skeleton parse_branching state h with
        | None -> None
        | Some (sk, s') -> f name const_name args sk S5 s'
      end
      | _ -> None
    end
    | S5, Empty -> Some (name, const_name, List.rev args, skeleton)
    | _ -> None
  in
  let taken, s' = take_line_unitlex s in
  match f None None [] [] S0 (stream_of_list taken) with
  | Some (Some name, Some const_name, args, skeleton) ->
    let is_const_called name const = const.c_name = name in
    let constructor =
      match List.find_opt (is_const_called const_name) state.constructors with
      | Some c -> c
      | None ->
        failwith (Printf.sprintf
          "Parsing error : undefined constructor %s" const_name)
    in
    let rule =
      { r_name = name;
        constructor = constructor;
        constructor_arguments = List.map var_of_string args;
        skeleton = skeleton }
    in
    let state' = { state with rules = rule :: state.rules } in
    Some (state', s')
  | _ -> None

(* Gives a new empty state to use with 'parse' function. *)
let new_state () =
  { base_sorts = []; flow_sorts = []; program_sorts = []; constructors = [];
    filters = []; atoms = []; rules = []; phase = Nothing; line_number = 1 }

(* General parsing function. The parsing subfunction is chosen according to the
   section in the input file, determined by the [...]Section lexemes.
*)
let parse s =
  let rec parse_r state = function
    | Empty ->
      ( List.rev state.base_sorts, List.rev state.flow_sorts,
        List.rev state.program_sorts, List.rev state.constructors,
        List.rev state.filters, List.rev state.atoms, List.rev state.rules )
    | More (a, f) as s -> begin
      match a with
      | Newline -> (* updating the line number *)
        parse_r { state with line_number = state.line_number + 1 } (f())
      | ConstSection -> parse_r { state with phase = Constructors } (f())
      | BaseSortSection -> parse_r { state with phase = BaseSorts } (f())
      | FlowSortSection -> parse_r { state with phase = FlowSorts } (f())
      | ProgramSortSection -> parse_r { state with phase = ProgramSorts } (f())
      | FilterSection -> parse_r { state with phase = Filters } (f())
      | AtomSection -> parse_r { state with phase = Atoms } (f())
      | RuleSection -> parse_r { state with phase = Rules } (f())
      | ConstName _ -> begin (* constructor with special characters or rule *)
        let result =
          match state.phase with
          | Constructors -> parse_constructor state s
          | Rules -> parse_rule state s
          | _ -> None
        in
        match result with
        | Some (state', s') -> parse_r state' s'
        | None -> (* wrong lexemes in the middle of the line *)
          failwith
            (Printf.sprintf "Parsing error on line %d" state.line_number)
      end
      | Ident i -> begin (* not a section *)
        let result =
          match state.phase with
          | BaseSorts -> parse_base_sort state s
          | FlowSorts -> parse_flow_sort state s
          | ProgramSorts -> parse_program_sort state s
          | Constructors -> parse_constructor state (More (ConstName i, f))
          | Filters -> parse_filter state s
          | Atoms -> parse_atom state s
          | Rules -> parse_rule state s
          | _ -> None
        in
        match result with
        | Some (state', s') -> parse_r state' s'
        | None -> (* wrong lexemes in the middle of the line *)
          failwith
            (Printf.sprintf "Parsing error on line %d" state.line_number)
      end
      | u -> (* not a thing that should be at the beginning of a line *)
        failwith
          (Printf.sprintf "Unexpected '%s' on line %d"
          (string_of_unitlex u) state.line_number)
    end
  in
  parse_r (new_state()) s