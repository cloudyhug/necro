(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====               Unitlex type & functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

type unitlex =
  | Space
  | Newline
  | ConstSection
  | BaseSortSection
  | FlowSortSection
  | ProgramSortSection
  | FilterSection
  | AtomSection
  | RuleSection
  | Colon
  | Times
  | Arrow
  | LParen
  | RParen
  | LSquareBracket
  | RSquareBracket
  | HookLex
  | Comma
  | Semicolon
  | BranchStart
  | BranchChange
  | BranchEnd
  | LCurlyBracket
  | RCurlyBracket
  | EqualFilter
  | Equal
  | Ident of string
  | ConstName of string

let string_of_unitlex = function
  | Space -> " "
  | Newline -> "\\n"
  | ConstSection -> "- constructors"
  | BaseSortSection -> "BASE"
  | FlowSortSection -> "FLOW"
  | ProgramSortSection -> "PROGRAM"
  | FilterSection -> "- filters"
  | AtomSection -> "- atoms"
  | RuleSection -> "- rules"
  | Colon -> ":"
  | Times -> "*"
  | Arrow -> "->"
  | LParen -> "("
  | RParen -> ")"
  | LSquareBracket -> "["
  | RSquareBracket -> "]"
  | HookLex -> "H"
  | Comma -> ","
  | Semicolon -> ";"
  | BranchStart -> "[|"
  | BranchChange -> "||"
  | BranchEnd -> "|]"
  | LCurlyBracket -> "{"
  | RCurlyBracket -> "}"
  | EqualFilter -> "=?"
  | Equal -> "="
  | Ident s -> s
  | ConstName s -> s

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====             Stream type & functions             ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

type 'a stream =
  | Empty
  | More of 'a * (unit -> 'a stream)

(* take_n s n builds a list from the n next elements in the stream s, and
   returns this list with the remaining stream.
*)
let take_n s n =
  let rec f k acc = function
    | Empty -> (List.rev acc, Empty)
    | More (a, g) as s ->
      if k = n then (List.rev acc, s) else f (k+1) (a :: acc) (g())
  in
  f 0 [] s

(* take_line_char s takes all the characters of the char stream s until
   it finds a newline character ('\n'). It returns the list of these
   characters with the remaining stream.
*)
let take_line_char s =
  let rec f acc = function
    | Empty -> (List.rev acc, Empty)
    | More (a, g) as s ->
      if a = '\n' then (List.rev acc, s) else f (a :: acc) (g())
  in
  f [] s

(* Same as take_line_char with a unitlex stream (stops on 'Newline' lexeme). *)
let take_line_unitlex s =
  let rec f acc = function
    | Empty -> (List.rev acc, Empty)
    | More (a, g) as s ->
      if a = Newline then (List.rev acc, s) else f (a :: acc) (g())
  in
  f [] s

(* take_while_rgx r s takes all the characters in the char stream s while the
   whole taken string matches the regexp r. It returns the string and the
   remaining stream.
*)
let take_while_rgx r s =
  let rec f acc = function
    | Empty -> (acc, Empty)
    | More (a, g) as s ->
      try
        let nacc = acc ^ (String.make 1 a) in
        if Str.string_match r nacc 0 && Str.matched_string nacc = nacc then
          f nacc (g())
        else (acc, s)
      with Not_found -> (acc, s)
  in
  f "" s

let rec stream_of_string s =
  let f () = stream_of_string (String.sub s 1 (String.length s - 1)) in
  if s = "" then Empty else More (s.[0], f)

let rec stream_of_list = function
  | [] -> Empty
  | e :: r -> More (e, fun () -> stream_of_list r)

(* Builds a one-element stream. *)
let one e = More (e, fun () -> Empty)

(* make_stream e s makes a stream with e as the first element, followed by
   the stream s.
*)
let make_stream e s = More (e, fun () -> s)

(* dump s takes all the remaining elements in the stream s and returns a
   list of these elements.
*)
let dump s =
  let rec f acc = function
    | Empty -> List.rev acc
    | More (a, g) -> f (a :: acc) (g())
  in
  f [] s

(* append s1 s2 builds a stream by concatenation of streams s1 and s2. *)
let rec append s1 s2 =
  match s1 with
  | Empty -> s2
  | More (a, g) -> More (a, fun () -> append (g()) s2)

(* Works like List.filter for streams. *)
let rec filter_stream p = function
  | Empty -> Empty
  | More (a, g) ->
    if p a then
      More(a, fun () -> filter_stream p (g()))
    else filter_stream p (g()) 

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====             Other useful functions              ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

(* getlines ic takes all the lines available in the input channel and puts
   them in a string.
*)
let getlines ic =
  let rec f acc =
    try f ((input_line ic) :: acc) with End_of_file -> String.concat "\n" (List.rev acc)
  in f []

let string_of_char_list cl = String.concat "" (List.map (String.make 1) cl)

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====               Rule representation               ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

type sort =
  | Base of string (* terms *)
  | Program of
    { ps_name : string;
      input_type : sort; (* flow *)
      output_type : sort; (* flow *)
    } (* terms *)
  | Flow of string (* values *)

let rec base_contains_name n = function
  | Base name :: base_r ->
    if name = n then true else base_contains_name n base_r
  | _ -> false

let rec flow_contains_name n = function
  | Flow name :: base_r ->
    if name = n then true else flow_contains_name n base_r
  | _ -> false

let rec get_program_sort_by_name n = function
  | Program { ps_name; _ } as ps :: base_r ->
    if ps_name = n then Some ps else get_program_sort_by_name n base_r
  | _ -> None

let sort_in_opt = function
  | Program { ps_name ; input_type; _ } -> Some input_type
  | _ -> None

let sort_out_opt = function
  | Program { ps_name ; input_type; output_type } -> Some output_type
  | _ -> None

let string_of_sort = function
  | Base s -> s
  | Program { ps_name; _ } -> ps_name
  | Flow s -> s

type constructor_signature =
  { c_name : string;
    c_input_sorts : sort list; (* base or program *)
    c_output_sort : sort; (* program *)
  }

type filter_signature =
  { fs_name : string;
    fs_input_sorts : sort list;
    fs_output_sorts : sort list;
  }

type term =
  | Vterm of string
  | Cterm of constructor_signature * term list

type var =
  | TVar of string
  | FVar of string

let string_of_var = function
  | TVar s -> s
  | FVar s -> s

type hook =
  { state : var; (* fvar *)
    term : term;
    output : var; (* fvar *)
  }

type filter =
  { f_name : string;
    f_inputs : var list;
    f_outputs : var list;
  }

type bone =
  | Hook of hook
  | Filter of filter
  | Branching of branching
and branching =
  { branches : bone list list;
    outputs : var list;
  }

type rule =
  { r_name : string;
    constructor : constructor_signature;
    constructor_arguments : var list; (* tvar *)
    skeleton : bone list;
  }