This is a documentation file explaining how to create an input for this project's main program, and defining the output functor as well as how it works.

## Semantics theory

The input file describes the semantics of a programming language as explained in the research article associated with this project, which you can find [here](linktoarticle). Have it in mind for the following parts of the documentation.

## Input file grammar

Here are some tokens defined with constant strings :

```
<newline> ::= "\n"
<basesortsection> ::= "BASE"
<flowsortsection> ::= "FLOW"
<progsortsection> ::= "PROGRAM"
<constsection> ::= "- constructors"
<filtersection> ::= "- filters"
<atomsection> ::= "- atoms"
<rulesection> ::= "- rules"
```

We can use them to explain the input file's general structure :

```
<inputfile> ::= <element> | <element> <inputfile>
<element> ::=
    | <newline> | <basesortsection> | <flowsortsection> | <progsortsection>
    | <constsection> | <filtersection> | <atomsection> | <rulesection>
    | <basesort> | <flowsort> | <programsort>
    | <constructor> | <filtersig> | <atom> | <rule>
```

The tokens describing identifiers, constructor names and types are the following :

```
<ident> ::= [a-z_][a-zA-Z0-9_']*
<const> ::= [a-zA-Z0-9_&%!$=+:/;.?<>@#-']+
<any> ::= <ident> | <const>
<types> ::= <ident> | <ident> "*" <types>
<atypes> ::= <ident> | <ident> "->" <atypes>
<stypes> ::= <ident> | <ident> " " <stypes>
<ctypes> ::= <ident> | <ident> "," <ctypes>
```

They are used to define the explicit content of the input file :

```
<basesort> ::= <ident>
<flowsort> ::= <ident>
<programsort> ::= <ident> "=" <ident> "*" <ident>
<constructor> ::= <any> ":" <types> "->" <ident>
<filtersig> ::= <ident> ":" <types> "->" <types>
<atom> ::= <ident> ":" <atypes>
```

The semantic rules are defined the following way :

```
<rule> ::= <any> "(" <any> <stypes> ") =" <skeleton>
<skeleton> ::= "[" <bones> "]"
<bones> ::=
    | <hook> ";" <bones>
    | <filter> ";" <bones>
    | <branching> ";" <bones>
<hook> ::= "H(" <ident> "," <any> <stypes> "," <ident> ")"
<filter> ::=
    | <ident> "(" <ctypes> ")"
    | <ident> "(" <ctypes> ") ?> (" <ctypes> ")"
<branching> ::= "[|" <skeletons> "|]{" <ctypes> "}"
<skeletons> ::= <skeleton> | <skeleton> <skeletons>
```

Some separating spaces between words and symbols may have been forgotten in the grammar. They are necessary for lexical analysis but they are removed before the parsing phase, so the parser does not process any whitespace lexeme. The necessary spaces are quite natural though. Do not mind glancing at the example input files in the **test** folder.

## Input module contents

The executable produces an OCaml file, which is a module, like all OCaml files. For instance, when running `./main.native myinput.txt myinput.ml`, a `myinput.ml` file will be created, which can be referred as a `Myinput` module in another OCaml file.

This module contains a `MakeInterpreter` functor that needs an input module and outputs another module which is the interpreter. The input module must contain the implementations of every sort found in the input file the user gave previously : base sorts, flow sorts and program sorts.

For example, an `ident` base sort for variable identifiers can be represented as a string in OCaml. The user will create an `ident` type equal to OCaml's `string` type in the module. Moreover, the user needs to provide implementations for every filter, every atom, and a print function for every flow sort. Here is an example :

```ocaml
module I = struct
  type ident = string (* base sort *)
  type state = (ident, int) list (* flow sort *)
  let initial_state () = [] (* atom *)
  let read ident state = snd (List.find (fun (i, v) -> i = ident) state) (* filter *)
  (* state is a flow sort, so we need to implement the following function : *)
  let print_state state = List.iter (fun (i, v) -> Printf.printf "%s : %d\n" i v) state
end
```

After writing this module, the user can call the functor to create the interpreter :

```ocaml
module Interp = Myinput.MakeInterpreter (I)
```

After that, the user can build terms with the interpreter's constructors, and evaluate them with the `eval` function. Do not hesitate having a look at the examples in the **test** folder.