open Stream
open Unitlex

(* Tokenisers are functions that recognise some specific characters and
   translate them into lexemes. A tokeniser is called on a stream and returns
   an option : if it does not recognise the character(s), it returns None.
*)

let string_of_char_list cl = String.concat "" (List.map (String.make 1) cl)

let token_space = function
  | More (' ', f) -> Some (Space, f())
  | _ -> None
  
let token_newline = function
  | More ('\n', f) -> Some (Newline, f())
  | _ -> None

let token_constsection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "- constructors" then
    Some (ConstSection, s')
  else None

let token_filtersection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "- filters" then
    Some (FilterSection, s')
  else None

let token_atomsection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "- atoms" then
    Some (AtomSection, s')
  else None

let token_rulesection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "- rules" then
    Some (RuleSection, s')
  else None

let token_basesortsection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "BASE" then
    Some (BaseSortSection, s')
  else None

let token_flowsortsection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "FLOW" then
    Some (FlowSortSection, s')
  else None

let token_programsortsection s =
  let taken, s' = take_line_char s in
  if string_of_char_list taken = "PROGRAM" then
    Some (ProgramSortSection, s')
  else None

let token_colon = function
  | More (':', f) -> begin
    match f() with
    | More (' ', _) as g -> Some (Colon, g)
    | _ -> None
  end
  | _ -> None

let token_times = function
  | More ('*', f) -> begin
    match f() with
    | More (' ', _) as g -> Some (Times, g)
    | _ -> None
  end
  | _ -> None

let token_arrow s =
  let taken, s' = take_n s 3 in
  if string_of_char_list taken = "-> " then
    Some (Arrow, s')
  else None

let token_lparen = function
  | More ('(', f) -> Some (LParen, f())
  | _ -> None

let token_rparen = function
  | More (')', f) -> Some (RParen, f())
  | _ -> None

let token_lsquarebracket = function
  | More ('[', f) -> Some (LSquareBracket, f())
  | _ -> None

let token_rsquarebracket = function
  | More (']', f) -> Some (RSquareBracket, f())
  | _ -> None

let token_hook = function
  | More ('H', f) -> begin
    match f() with
    | More ('(', _) as g -> Some (HookLex, g)
    | _ -> None
  end
  | _ -> None

let token_comma = function
  | More (',', f) -> Some (Comma, f())
  | _ -> None

let token_semicolon = function
  | More (';', f) -> begin
    match f() with
    | More (' ', _) as g -> Some (Semicolon, g)
    | _ -> None
  end
  | _ -> None

let token_branchstart s =
  let taken, s' = take_n s 2 in
  if string_of_char_list taken = "[|" then
    Some (BranchStart, s')
  else None

let token_branchchange s =
  let taken, s' = take_n s 2 in
  if string_of_char_list taken = "||" then
    Some (BranchChange, s')
  else None

let token_branchend s =
  let taken, s' = take_n s 2 in
  if string_of_char_list taken = "|]" then
    Some (BranchEnd, s')
  else None

let token_lcurlybracket = function
  | More ('{', f) -> Some (LCurlyBracket, f())
  | _ -> None

let token_rcurlybracket = function
  | More ('}', f) -> Some (RCurlyBracket, f())
  | _ -> None

let token_equalfilter s =
  let taken, s' = take_n s 2 in
  if string_of_char_list taken = "?>" then
    Some (EqualFilter, s')
  else None

let token_equal = function
  | More ('=', f) -> begin
    match f() with
    | More (' ', _) as g -> Some (Equal, g)
    | _ -> None
  end
  | _ -> None

let token_ident s =
  let r = Str.regexp "[a-z_][a-zA-Z0-9_']*" in
  match take_while_rgx r s with
  | "", _ -> None
  | taken, s' -> Some (Ident taken, s')

let token_constname s =
  let r = Str.regexp "[a-zA-Z0-9_&%!$=+:/;.?<>@#-']+" in
  match take_while_rgx r s with
  | "", _ -> None
  | taken, s' -> Some (ConstName taken, s')

(* Lexer state type : the lexer keeps in memory the line number and the
   unitlex stream it is building while lexing.
*)

type lexer_state =
  { line_number : int;
    lexemes : unitlex stream;
  }

(* General lexing function. It builds a unitlex stream from a char stream. *)
let analex s =
  (* get_token tries each tokeniser in the tokeniser list until one of them
     returns something. Otherwise it returns None, which means that the
     characters could not be recognised.
  *)
  let rec get_token s = function
    | [] -> None
    | t :: ts -> begin
      match t s with
      | None -> get_token s ts
      | u -> u
    end
  in
  (* all the possible tokenisers we can call *)
  let tokenisers =
    [ token_space; token_newline; token_constsection; token_filtersection;
      token_atomsection; token_rulesection; token_basesortsection;
      token_flowsortsection; token_programsortsection; token_colon;
      token_times; token_arrow; token_lparen; token_rparen; token_hook;
      token_comma; token_semicolon; token_branchstart; token_branchchange;
      token_branchend; token_lsquarebracket; token_rsquarebracket;
      token_lcurlybracket; token_rcurlybracket; token_equalfilter; token_equal;
      token_ident; token_constname ]
  in
  (* While the char stream is not empty, f builds the lexeme stream and updates
     the line number everytime it adds a 'Newline' lexeme to it. *)
  let rec f state = function
    | Empty -> state.lexemes
    | More (c, _) as s -> begin
      match get_token s tokenisers with
      | None ->
        (* error message with line number and unmatched character *)
        failwith
          (Printf.sprintf "Illegal character on line %d : %c" state.line_number c)
      | Some (token, s') -> begin
        match token with
        | Newline ->
          let state' =
            { line_number = state.line_number + 1;
              lexemes = append state.lexemes (one Newline) }
          in
          f state' s'
        | _ ->
          let state' =
            { line_number = state.line_number;
              lexemes = append state.lexemes (one token) }
          in
          f state' s'
      end
    end
  in
  let state = { line_number = 1; lexemes = Empty } in
  (* spaces are not meaningful in our case when parsing lexemes *)
  filter_stream (fun e -> e <> Space) (f state s)