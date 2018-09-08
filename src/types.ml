(* ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== *)
(* =====   Memory representation of semantics elements   ===== *)
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