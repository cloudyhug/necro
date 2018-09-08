open Stream

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

(* Same as take_line_char with a unitlex stream (stops on 'Newline' lexeme). *)
let take_line_unitlex s =
  let rec f acc = function
    | Empty -> (List.rev acc, Empty)
    | More (a, g) as s ->
      if a = Newline then (List.rev acc, s) else f (a :: acc) (g())
  in
  f [] s