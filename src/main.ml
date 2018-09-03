open Common
open Lexer
open Parser
open Typechecker
open Generator

let () =
  if Array.length Sys.argv = 3 then begin
    let bs, fs, ps, const, filt, atoms, rules =
      open_in Sys.argv.(1)
      |> getlines
      |> stream_of_string
      |> analex
      |> parse
    in
    well_formedness_interpretation const filt rules;
    generate_interpreter bs fs ps atoms const filt rules Sys.argv.(2);
    (* generate_user_input bs fs ps atoms const filt
      Sys.argv.(2) (Sys.argv.(2) ^ ".userinput") *)
  end else begin
    print_endline ("Usage : " ^ Sys.argv.(0) ^ " input_file output_file.");
    exit 1
  end