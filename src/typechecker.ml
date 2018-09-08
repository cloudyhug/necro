open Types

(* a domain is a set of variables *)
module Domain = Set.Make
  (struct
    type t = var
    let compare = Pervasives.compare
  end)

(* This function takes a gamma, i.e. a list of (var, sort) pairs, and creates
   a gamma function, like in the paper.
*)
let make_gamma_f gamma = fun var ->
  match List.assoc_opt var gamma with
  | Some s -> s
  | None ->
    failwith (Printf.sprintf "Undefined variable '%s'" (string_of_var var))

(* works like iteri but calls a function with the int parameter and an element
   from each of the two lists
*)
let iteri2 f l1 l2 =
  let rec g i l1 l2 =
    match l1, l2 with
    | a :: r1, b :: r2 -> f i a b; g (i+1) r1 r2
    | [], [] -> ()
    | _ -> failwith "List lengths do not match"
  in
  g 0 l1 l2

(* Sort_gamma(t) function in the paper *)
let sort gamma t =
  let gamma_f = make_gamma_f gamma in
  let rec s j =
    let f cn i t s' =
      let st = s (j+1) t in
      if st <> s' then
        failwith
          ((Printf.sprintf "Variable type %s does not match expected "
            (string_of_sort st))
          ^ (Printf.sprintf "constructor type %s "
              (string_of_sort s'))
          ^ (Printf.sprintf "(constructor %s, variable %d, level %d)"
              cn (i+1) j))
    in
    function
    | Vterm vt -> gamma_f (TVar vt)
    | Cterm (c, tl) ->
      iteri2 (f c.c_name) tl c.c_input_sorts;
      c.c_output_sort
  in
  s 1 t

(* checks that a variable has the right type according to its sort :
   a term variable must have a base / program sort, and a flow variable
   must have a flow sort
*)
let check_sort_type var sort =
  match var, sort with
  | TVar _, (Base _ | Program _)
  | FVar _, Flow _ -> ()
  | TVar v, Flow f ->
    failwith (Printf.sprintf
      "Term variable '%s' does not match flow sort '%s'" v f)
  | FVar f, Base b ->
    failwith (Printf.sprintf
      "Flow variable '%s' does not match base sort '%s'" f b)
  | FVar f, Program { ps_name; _ } ->
    failwith (Printf.sprintf
      "Flow variable '%s' does not match program sort '%s'" f ps_name)

(* checks the previous condition on a whole gamma object *)
let check_well_formed_gamma gamma =
  let f = function
    | TVar _, (Base _ | Program _)
    | FVar _, Flow _ -> ()
    | _ -> failwith "gamma is not well formed"
  in
  List.iter f gamma

(* checks that a constructor is well formed : inputs are term sorts and output
   is a program sort
*)
let check_well_formed_constructor constructor =
  (* the inputs must be term sorts (base / program) *)
  let f = function
    | Flow s ->
      failwith (Printf.sprintf
        "Flow sort 'Flow %s' as the input of a constructor is not allowed" s)
    | _ -> ()
  in
  List.iter f constructor.c_input_sorts;
  match constructor.c_output_sort with
  | Program _ -> ()
  | _ ->
    failwith (Printf.sprintf
      "Output of constructor '%s' is not a program sort" constructor.c_name)

(* checks that a hook is well formed, according to the conditions listed in
   the paper
*)
let check_well_formed_hook hook gamma domain =
  let gamma_f = make_gamma_f gamma in
  (* condition 1 *)
  let () =
    let gamma_domain = Domain.of_list (List.map fst gamma) in
    if not (Domain.mem hook.state gamma_domain) then
      failwith (Printf.sprintf
        "Undefined variable '%s' in hook" (string_of_var hook.state))
    else if not (Domain.subset gamma_domain domain) then
      failwith "dom(gamma) is not a subset of D"
  in
  (* sort of the hook's term *)
  let term_sort = sort gamma hook.term in
  (* condition 2 *)
  let () =
    let expected_input_sort =
      match sort_in_opt term_sort with
      | Some i -> i
      | None -> failwith "The hook term's sort is not a program sort"
    in
    let actual_input_sort = gamma_f hook.state in
    if actual_input_sort <> expected_input_sort then
      failwith (Printf.sprintf
        "Hook input types do not match :\n  got '%s' instead of '%s'"
        (string_of_sort actual_input_sort)
        (string_of_sort expected_input_sort))
  in
  (* condition 3 *)
  let () =
    if Domain.mem hook.output domain then
      failwith (Printf.sprintf
        "Output variable '%s' in hook is already defined"
        (string_of_var hook.state))
  in
  (* input and output must be flow sorts *)
  let () =
    match hook.state with
    | TVar _ -> failwith "The hook's input sort is not a flow sort"
    | _ -> ()
  in
  let () =
    match hook.output with
    | TVar _ -> failwith "The hook's output sort is not a flow sort"
    | _ -> ()
  in
  (* returning new gamma and new domain *)
  let gamma' =
    match sort_out_opt term_sort with
    | Some o -> (hook.output, o) :: gamma
    | None -> failwith "The hook term's sort is not a program sort"
  in
  let domain' = Domain.add hook.output domain in
  (gamma', domain')

(* checks that a filter is well formed according to the paper *)
let check_well_formed_filter filter gamma domain filter_signatures =
  let gamma_f = make_gamma_f gamma in
  let filter_inputs, filter_outputs =
    Domain.of_list filter.f_inputs, Domain.of_list filter.f_outputs
  in
  (* condition 1 *)
  let () =
    let gamma_domain = Domain.of_list (List.map fst gamma) in
    if not (Domain.subset filter_inputs gamma_domain) then
      failwith "Filter inputs are not a subset of dom(gamma)"
    else if not (Domain.subset gamma_domain domain) then
      failwith "dom(gamma) is not a subset of D"
  in
  (* condition 2 *)
  let () =
    if Domain.exists (fun o -> Domain.mem o domain) filter_outputs then
      failwith "Some outputs are defined before the filter's execution"
  in
  (* condition 3 *)
  let p f = f.fs_name = filter.f_name in
  let fsort =
    match List.find_opt p filter_signatures with
    | Some f -> f
    | None ->
      failwith (Printf.sprintf
        "No filter matches the name '%s'" filter.f_name)
  in
  let () =
    if List.map gamma_f filter.f_inputs <> fsort.fs_input_sorts then
      failwith "The filter's inputs do not match the filter's signature"
  in
  let () =
    if List.compare_lengths filter.f_outputs fsort.fs_output_sorts <> 0 then
      failwith ("The filter does not have the same number "
        ^ "of outputs as its signature")
  in
  List.iter2 check_sort_type filter.f_outputs fsort.fs_output_sorts;
  let gamma' =
    let added = List.combine filter.f_outputs fsort.fs_output_sorts in
    added @ gamma
  in
  let domain' = List.fold_left (fun d o -> Domain.add o d) domain filter.f_outputs in
  (gamma', domain')

(* checks a skeleton (calls the functions for hooks, filter or branchings) *)
let rec _check_well_formed_skeleton cb skeleton gamma domain filter_signatures =
  match skeleton with
  | [] -> (gamma, domain)
  | bone :: skeleton_r ->
    let gamma', domain' =
      match bone with
      | Hook h -> check_well_formed_hook h gamma domain
      | Filter f -> check_well_formed_filter f gamma domain filter_signatures
      | Branching b ->
        cb b gamma domain filter_signatures
    in
    _check_well_formed_skeleton cb skeleton_r gamma' domain' filter_signatures

(* checks that a branching is well formed according to the paper *)
let rec check_well_formed_branching branching gamma domain filter_signatures =
  (* i is the number of the branch (for error messages),
     d' is the D' domain that we are building progressively
     diffs is the list of D_i / D values, to check condition 2
  *)
  let rec check_branch (i, d', diffs, gamma_v) branch =
    let gamma_i, domain_i =
      _check_well_formed_skeleton check_well_formed_branching
        branch gamma domain filter_signatures
    in
    (* condition 1 *)
    let () =
      let gamma_domain_i = Domain.of_list (List.map fst gamma_i) in
      if not (Domain.subset gamma_domain_i domain_i) then
        failwith (Printf.sprintf
          "dom(gamma_%d) is not a subset of D_%d" (i+1) (i+1))
    in
    (* condition 2 *)
    let diff_i = Domain.diff domain_i domain in
    let br_out = Domain.of_list branching.outputs in
    let () =
      let p diff = Domain.equal (Domain.inter diff_i diff) br_out in
      if not (List.for_all p diffs) then
        failwith ("The second condition is not fulfilled by D_"
          ^ string_of_int (i+1) ^ " : either V is not in D_"
          ^ string_of_int (i+1) ^ " or a local variable is present in D_"
          ^ string_of_int (i+1) ^ " and another D_j")
    in
    let gv =
      let gamma_f_i = make_gamma_f gamma_i in
      match gamma_v with
      | None -> List.map (fun o -> (o, gamma_f_i o)) branching.outputs
      | Some g -> g
    in
    (i+1, Domain.union d' domain_i, diff_i :: diffs, Some gv)
  in
  let _, d', _, gamma_v = List.fold_left check_branch (0, Domain.empty, [], None) branching.branches in
  match gamma_v with
  | Some gv -> (List.rev_append gv gamma, d')
  | None -> failwith "The branching is empty (no branches found)"

(* syntactic sugar for mutually recursive skeleton / branching functions *)
let check_well_formed_skeleton =
  _check_well_formed_skeleton check_well_formed_branching

(* checks that a rule is well formed : term variables as constructor inputs,
   program sort as output, well formed skeleton, and correct return type
*)
let check_well_formed_rule rule filter_signatures =
  (* inputs must be term variables *)
  let f = function
    | FVar s ->
      failwith (Printf.sprintf
        "Flow variable '%s' as an argument of a rule is not allowed" s)
    | _ -> ()
  in
  List.iter f rule.constructor_arguments;
  (* output sort must be a program sort *)
  let term_sort = rule.constructor.c_output_sort in
  let input_sort =
    match sort_in_opt term_sort with
    | Some i -> i
    | None ->
      failwith (Printf.sprintf
      "The rule constructor's output sort '%s' is not a program sort"
      (string_of_sort term_sort))
  in
  let output_sort =
    match sort_out_opt term_sort with
    | Some o -> o
    | None ->
      failwith (Printf.sprintf
      "The rule constructor's output sort '%s' is not a program sort"
      (string_of_sort term_sort))
  in
  let gamma =
    let l =
      List.combine rule.constructor_arguments rule.constructor.c_input_sorts
    in
    (FVar "x_s", input_sort) :: l
  in
  let domain = Domain.of_list (List.map fst gamma) in
  let gamma', _ =
    check_well_formed_skeleton rule.skeleton gamma domain filter_signatures
  in
  let gamma_f' = make_gamma_f gamma' in
  let x_o_sort =
    try gamma_f' (FVar "x_o")
    with Failure _ -> failwith "x_o is still undefined at the end of the rule"
  in
  if x_o_sort <> output_sort then
    failwith ("The type of x_o at the end of the rule ('"
      ^ (string_of_sort x_o_sort) ^ "') does not match the expected one ('"
      ^ (string_of_sort output_sort) ^ "')")

(* checks constructors, filters and rules for the well-formedness
   interpretation described in the paper
*)
let well_formedness_interpretation constructors filter_signatures rules =
  List.iter check_well_formed_constructor constructors;
  let f rule = check_well_formed_rule rule filter_signatures in
  List.iter f rules