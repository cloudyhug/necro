open Types
open Easy_format

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====          Parameters & useful functions          ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

(* gets a filename's radix *)
let without_extension s =
  let r = Str.regexp "\\." in
  let i =
    try Str.search_backward r s (String.length s - 1)
    with Not_found -> String.length s
  in
  String.sub s 0 i

(* 2 spaces for OCaml indent level *)
let indent_level = 2

(* new line after each statement *)
let listparam =
  { list with
    wrap_body = `Force_breaks;
    indent_body = indent_level; }

(* just adds "type " before the sort's name and returns the string *)
let transform_sort = function
  | Base s | Flow s -> "type " ^ s
  | _ -> failwith "transform_sort"

(* makes an Easy_format.Atom from a string, with the default parameters *)
let atomise s = Atom(s, atom)

(* maps a (char -> string) function to each char of a string and returns the
   concatenation of all the results (basically map & concat at the same time)
*)
let smap f s =
  let l = String.length s in
  let rec smap_aux acc i =
    if i = l then acc
    else smap_aux (acc ^ (f s.[i])) (i+1)
  in
  smap_aux "" 0

(* converts a constructor name to a valid OCaml constructor name *)
let get_alpha_name s =
  let f = function
    | '&' -> "And"
    | '%' -> "Percent"
    | '!' -> "Bang"
    | '$' -> "Dollar"
    | '=' -> "Equal"
    | '+' -> "Plus"
    | ':' -> "Colon"
    | '/' -> "Slash"
    | ';' -> "Semicolon"
    | '.' -> "Dot"
    | '?' -> "QMark"
    | '<' -> "Lt"
    | '>' -> "Gt"
    | '@' -> "At"
    | '#' -> "Hash"
    | '-' -> "Minus"
    | '\'' -> "Quote"
    | c -> String.make 1 c
  in
  let s' = smap f s in
  let g = function
    | '_' -> "Underscore"
    | '0' -> "Zero"
    | '1' -> "One"
    | '2' -> "Two"
    | '3' -> "Three"
    | '4' -> "Four"
    | '5' -> "Five"
    | '6' -> "Six"
    | '7' -> "Seven"
    | '8' -> "Eight"
    | '9' -> "Nine"
    | letter -> String.make 1 (Char.uppercase_ascii letter)
  in
  let r = Str.regexp "[a-z0-9_]" in
  let len = String.length s' in
  if Str.string_match r (String.sub s' 0 1) 0 then
    (g s.[0]) ^ (String.sub s' 1 (len - 1))
  else s'

(* converts a sort to 'sortname string format *)
let generic_name_of_sort s = "'" ^ (string_of_sort s)

(* converts a string to snake case (OneTwoThree -> one_two_three) *)
let get_snake_case s =
  let f c =
    let cc = Char.code c in
    if cc >= 65 && cc <= 90 then
      "_" ^ (String.make 1 (Char.lowercase_ascii c))
    else String.make 1 c
  in
  let result = smap f s in
  match result.[0] with
  | '_' -> String.sub result 1 (String.length result - 1)
  | _ -> result

(* The x_sigma state, which is the beginning flow variable of every rule,
   is written as "x_s" in the rules, so we return "state" if the string is
   "x_s", otherwise we do not change it.
*)
let check_sigma str = if str = "x_s" then "state" else str

let string_of_vterm = function
  | Vterm vt -> vt
  | _ -> failwith "string_of_vterm"

(* turns a hook into a let [output] = f [state] [term] OCaml statement *)
let format_hook h =
  let term_fmt =
    match h.term with
    | Vterm vt -> vt
    | Cterm (c, tl) ->
      let args =
        match tl with
        | [] -> ""
        | [Vterm vt] -> " " ^ vt
        | _ -> " (" ^ (String.concat ", " (List.map string_of_vterm tl)) ^ ")"
      in
      "(" ^ (get_alpha_name c.c_name) ^ args ^ ")"
  in
  atomise @@ Printf.sprintf "let %s = f %s %s in"
    (string_of_var h.output) (check_sigma @@ string_of_var h.state) term_fmt

(* turns a filter into a pattern matching (if None, fails, otherwise returns
   the filter's output values)
*)
let format_filter f =
  let title =
    match f.f_outputs with
    | [] -> atomise "let () ="
    | outputs ->
      atomise @@ Printf.sprintf "let %s ="
        (String.concat ", " (List.map string_of_var outputs))
  in
  let g v = check_sigma @@ string_of_var v in
  let match_head =
    atomise @@ Printf.sprintf "match %s %s with"
      f.f_name (String.concat " " (List.map g f.f_inputs))
  in
  let pattern_matching =
    [ match_head;
      atomise @@ Printf.sprintf "| None -> failwith \"%s\"" f.f_name;
      atomise "| Some result -> result" ]
  in
  Label
    ((title, label),
    List (("", "", "in", listparam), pattern_matching))

(* processes a bone with one of the defined functions for
   hooks / filters / branchings
*)
let _format_bone fb = function
  | Hook h -> format_hook h
  | Filter f -> format_filter f
  | Branching b -> fb b

(* A branching will become an OCaml block of the following form :
   let [expected_branching_outputs] =
    try [branch1] with _ ->
    try [branch2] with _ ->
    ...
    try [lastbranch] with _ -> failure (all the branches have failed)
*)
let rec format_branching b =
  let outputs = List.map string_of_var b.outputs in
  let f br =
    let out =
      match outputs with
      | [t] -> t
      | _ -> "(" ^ (String.concat ", " outputs) ^ ")"
    in
    Label
      ((atomise "try", label),
      List (("", "", "with _ ->", listparam),
        (List.map (_format_bone format_branching) br) @ [atomise out]))
  in
  let branching_head =
    atomise @@ Printf.sprintf "let %s =" (String.concat ", " outputs)
  in
  let failmsg =
    (String.make indent_level ' ') ^ "failwith \"All branches have failed\""
  in
  Label
    ((branching_head, label),
    List (("", "", "in", listparam),
      (List.map f b.branches) @ [atomise failmsg]))

(* syntactic sugar (bone & branching functions are mutually recursive) *)
let format_bone = _format_bone format_branching

(* A rule will be translated as :
   let eval_rule_name {f} [args] state =
     {process every bone, returning x_o at the end}
*)
let format_rule r =
  let f v = " " ^ (string_of_var v) in
  let title =
    Printf.sprintf "let eval_%s {f}%s state ="
      (get_snake_case r.r_name)
      (String.concat "" (List.map f r.constructor_arguments))
  in
  Label
    ((atomise title, label),
    List (("", "", "", listparam),
      (List.map format_bone r.skeleton) @ [atomise "x_o"]))

(* The sort_str parameter in the few next functions corresponds to the
   base/flow sorts written as "base1, base2, flow1, flow2, ...", sometimes
   generic (with a simple quote at the beginning), it depends on the function.
   It is used to break the heavy polymorphism of the term type.
*)

(* writes the big eval function (calls functions associated to every rule) *)
let format_eval sort_str const rules =
  let match_head = atomise "match term with" in
  let f r =
    let constargs = List.map string_of_var r.constructor_arguments in
    let leftargs, rightargs =
      match constargs with
      | [] -> "", ""
      | [arg] -> let v = " " ^ arg in v, v
      | _ ->
        " (" ^ (String.concat ", " constargs) ^ ")",
        " " ^ (String.concat " " constargs)
    in
    atomise @@ Printf.sprintf "| %s%s -> eval_%s {f = eval}%s state"
      (get_alpha_name r.constructor.c_name) leftargs
      (get_snake_case r.r_name) rightargs
  in
  let match_body = List.map f rules in
  Label
    ((atomise (Printf.sprintf
      "let rec eval : type a b. a -> (%s, a, b) term -> b =" sort_str), label),
    Label
      ((atomise "fun state term ->", label),
      List (("", "", "", listparam), match_head :: match_body)))

(* transforms a program sort structure into an OCaml term type *)
let transform_program_sort sort_str = function
  | Program { ps_name; input_type; output_type } ->
    Printf.sprintf "(%s, %s, %s) term"
        sort_str (generic_name_of_sort input_type) (generic_name_of_sort output_type)
  | _ -> failwith "transform_program_sort"

(* same for a constructor, translating the program sorts from the right hand
   side of the expression (constructor outputs only) because OCaml does not
   allow changing the left hand side
*)
let format_constructor const sort_str ps =
  let constname = get_alpha_name const.c_name in
  let output = transform_program_sort sort_str const.c_output_sort in
  let f s =
    match s with
    | Program { ps_name; _ } -> Printf.sprintf "(%s) %s" sort_str ps_name
    | _ -> generic_name_of_sort s
  in
  let params =
    match const.c_input_sorts with
    | [] -> output
    | inputs ->
      let inputs_names = List.map f inputs in
      (String.concat " * " inputs_names) ^ " -> " ^ output
  in
  atomise (Printf.sprintf "| %s : %s" constname params)

(* finds all the sorts used as output types in program sorts *)
let get_output_sorts ps =
  let rec f out = function
    | [] -> out
    | Program { ps_name; input_type; output_type } :: s_r ->
      if List.mem output_type out then f out s_r
      else f (output_type :: out) s_r
    | _ -> []
  in
  f [] ps

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====                    Term type                    ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

(* type (...) term = ... *)
let generate_term_type sort_str ps const =
  let f c = format_constructor c sort_str ps in
  Label
    ((atomise (Printf.sprintf "type (%s, _, _) term =" sort_str), label),
    List (("", "", "", listparam), List.map f const))

(* and ... = (...) term *)
let generate_program_sorts_abbreviations sort_str ps =
  let f ps =
    atomise @@ Printf.sprintf "and (%s) %s = %s"
      sort_str (string_of_sort ps) (transform_program_sort sort_str ps)
  in
  List.map f ps

(* type x *)
let generate_base_flow_sorts_types bs fs =
  let f s = atomise @@ transform_sort s in
  List.map f (bs @ fs)

(* val atom : x -> y *)
let generate_atoms_signatures ps atoms =
  let k s = atomise ("val " ^ s) in
  let os = get_output_sorts ps in
  let mkatom s =
    let str = string_of_sort s in
    atomise @@ Printf.sprintf "val print_%s : %s -> unit" str str
  in
  (List.map k atoms) @ (List.map mkatom os)

(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====                Filter Signatures                ===== *)
(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)

(* generates the correct filter signature from the filter structure *)
let format_filter_sig sort_fmt { fs_name; fs_input_sorts; fs_output_sorts } =
  let f = function
    | Program { ps_name; _ } -> Printf.sprintf "(%s) %s" sort_fmt ps_name
    | s -> string_of_sort s
  in
  let inputs = List.map f fs_input_sorts in
  let outputs = List.map f fs_output_sorts in
  let output_str =
    match outputs with
    | [] -> "unit"
    | [output] -> output
    | outputs -> "(" ^ (String.concat " * " outputs) ^ ")"
  in
  atomise (Printf.sprintf "val %s : %s -> %s option"
      fs_name (String.concat " -> " inputs) output_str)
(* same for filters *)
let generate_filter_signatures bs fs filt =
  let sort_str = String.concat ", " (List.map string_of_sort (bs @ fs)) in
  List.map (format_filter_sig sort_str) filt

(* writes the FLOW module type *)
let generate_flow_module_type sort_str bs fs ps atoms filt =
  let sorts_fmt = generate_base_flow_sorts_types bs fs in
  let atom_fmt = generate_atoms_signatures ps atoms in
  let filtersig_fmt = generate_filter_signatures bs fs filt in
  let content = sorts_fmt @ atom_fmt @ filtersig_fmt in
  Label
    ((atomise "module type FLOW =", label),
    List (("sig", "", "end", listparam), content))

(* writes the INTERPRETER module type *)
let generate_interpreter_module_type bs fs ps atoms =
  let sorts_fmt = generate_base_flow_sorts_types bs fs in
  let atom_fmt = generate_atoms_signatures ps atoms in
  let eval_fmt =
    atomise (Printf.sprintf
      "val eval : 'a -> (%s, 'a, 'b) term -> 'b"
      (String.concat ", " (List.map string_of_sort (bs @ fs))))
  in
  let content = sorts_fmt @ atom_fmt @ [eval_fmt] in
  Label
    ((atomise "module type INTERPRETER =", label),
    List (("sig", "", "end", listparam), content))

(* writes the MakeInterpreter functor *)
let generate_makeinterpreter_functor bs fs const rules =
  let incl_fmt = atomise "include F" in
  let sort_str = String.concat ", " (List.map string_of_sort (bs @ fs)) in
  let hrp_decl =
    atomise (Printf.sprintf
      "type f = { f : 'a 'b. 'a -> (%s, 'a, 'b) term -> 'b }" sort_str)
  in
  let rules_fmt = List.map format_rule rules in
  let eval_fmt = format_eval sort_str const rules in
  let content = incl_fmt :: hrp_decl :: (rules_fmt @ [eval_fmt]) in
  Label
    ((atomise
      "module MakeInterpreter (F : FLOW) =",
      label),
    List (("struct", "", "end", listparam), content))

(* writes the interpreter generator modules to an output file *)
let generate_interpreter bs fs ps atoms const filt rules filename =
  let sort_str = String.concat ", " (List.map generic_name_of_sort (bs @ fs)) in
  let tt = generate_term_type sort_str ps const in
  let psa = generate_program_sorts_abbreviations sort_str ps in
  let fmt = generate_flow_module_type sort_str bs fs ps atoms filt in
  let imt = generate_interpreter_module_type bs fs ps atoms in
  let mif = generate_makeinterpreter_functor bs fs const rules in
  let oc = open_out filename in
  let mainlistparam = { list with indent_body = 0 } in
  let reg_space = Str.regexp "^ +$" in
  let f acc line =
    if line = "" || (Str.string_match reg_space line 0) then acc
    else line :: acc
  in
  let reg_end = Str.regexp "^end$" in
  let str =
    (List(("", "", "", mainlistparam), (tt :: (psa @ [fmt; imt; mif]))))
    |> Pretty.to_string
    |> String.split_on_char '\n'
    |> List.fold_left f []
    |> List.rev
    |> String.concat "\n"
    |> Str.global_replace reg_end "end\n"
  in
  output_string oc str;
  close_out oc