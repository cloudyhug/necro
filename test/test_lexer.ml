open Common
open Lexer

let t1 () =
  let s = "a : ?>\n= \n* := -> g ; h [| [H(j)] || k |]{m, n}" in
  let result = dump (analex (stream_of_string s)) in
  let expected =
    [ Ident "a"; Colon; EqualFilter; Newline; Equal; Newline; Times; ConstName ":=";
      Arrow; Ident "g"; Semicolon; Ident "h"; BranchStart; LSquareBracket; HookLex;
      LParen; Ident "j"; RParen; RSquareBracket; BranchChange; Ident "k";
      BranchEnd; LCurlyBracket; Ident "m"; Comma; Ident "n"; RCurlyBracket ]
  in
  assert (result = expected)

let () =
  let test_func i test =
    try
      test();
      Printf.printf "Success %d\n" (i+1)
    with _ -> Printf.printf "Failure %d\n" (i+1)
  in
  let tests = [t1] in
  List.iteri test_func tests