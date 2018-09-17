(* lambda calculus test : needs an interpreter to be generated in "lambda.ml" *)

open Lambda

module Flow = struct
  type ident = string
  type clos = Clos of (ident * (ident, clos, env) lterm * env)
  and env = (ident * clos) list
  let rec string_of_lterm = function
    | Lam (id, lt) -> Printf.sprintf "λ%s.%s" id (string_of_lterm lt)
    | Var id -> id
    | App (Var id1, Var id2) -> id1 ^ " " ^ id2
    | App (Var id1, lt2) -> id1 ^ " (" ^ (string_of_lterm lt2) ^ ")"
    | App (lt1, Var id2) -> "(" ^ (string_of_lterm lt1) ^ ") " ^ id2
    | App (lt1, lt2) ->
      Printf.sprintf "(%s) (%s)" (string_of_lterm lt1) (string_of_lterm lt2)
  let rec string_of_clos (Clos (id, lt, env)) =
    let f acc (id, clos) = acc ^ id ^ " ==> " ^ (string_of_clos clos) ^ " " in
    Printf.sprintf "(%s, %s, [%s])"
      id (string_of_lterm lt) (List.fold_left f " " env)
  let print_clos clos = Printf.printf "%s" (string_of_clos clos)
  let mkClos id lt env = Some (Clos (id, lt, env))
  let getClos (Clos c) = Some c
  let extEnv env id clos = Some ((id, clos) :: env)
  let getEnv id env =
    match List.find_opt (fun (i, _) -> i = id) env with
    | None -> None
    | Some (_, c) -> Some c
  let initial_env () = []
  let rec freenames = function
    | Var x -> [x]
    | Lam (b, t) -> List.filter (fun n -> n <> b) (freenames t)
    | App (t1, t2) -> (freenames t1) @ (freenames t2)
  let freshname fn b =
    let rec f name = if List.mem name fn then f (name ^ "'") else name in
    f (b ^ "'")
  let rec unfold t env =
    match t with
    | Var x -> begin
      match getEnv x env with
      | Some (Clos c) ->
        let (b, t', env') = c in
        Lam (b, unfold t' env')
      | None -> t
    end
    | Lam (b, t') -> Lam (b, unfold t' env)
    | App (t1, t2) -> App(unfold t1 env, unfold t2 env)
  let rec subst b t1 t2 =
    match t1 with
    | Var x -> if x = b then t2 else t1
    | Lam (c, t) ->
      if c = b then t1 else
      let fn2 = freenames t2 in
      if List.mem c fn2 then
        let fn1 = freenames t1 in
        let fresh = freshname (b :: (fn1 @ fn2)) c in
        Lam (fresh, subst b (subst c t (Var fresh)) t2)
      else Lam (c, subst b t t2)
    | App (t1', t2') -> App (subst b t1' t2, subst b t2' t2)
  let rec simpl t n =
    if n < 0 then t else
    match t with
    | Var x -> Var x
    | Lam (b, t') -> Lam (b, simpl t' (n-1))
    | App (t1, t2) ->
      let t1' = simpl t1 n in
      let t2' = simpl t2 n in
      match t1' with
      | Lam (b, t') -> simpl (subst b t' t2') n
      | _ -> App (t1', t2')
  let print_env env =
    List.iter
      (fun (i, c) -> Printf.printf "(%s ==> %s)\n" i (string_of_clos c)) env
  let int_of_lterm t =
    let rec f n = function
      | Var "z" -> n
      | App (Var "s", t) -> f (n+1) t
      | _ -> failwith "int_of_term"
    in
    match t with
    | Lam ("s", Lam("z", t)) -> f 0 t
    | _ -> failwith "int_of_term"
end

module InterpLambda : (INTERPRETER with type ident = string and type clos = Flow.clos) = MakeInterpreter (Flow)

open InterpLambda

let l ident lterm : (ident,clos,env) lterm = Lam (ident, lterm)
let v ident = Var ident
let ( $ ) lt1 lt2 = App (lt1, lt2)

let pow f lt n =
  let rec w acc i = if i = n then f $ acc else w (f $ acc) (i+1) in
  w lt 1

let mkint i = l "s" @@ l "z" @@ (pow (v "s") (v "z") i)

let zero = l "s" @@ l "z" @@
  v "z"
let one = l "s" @@ l "z" @@
  (v "s" $ v "z")
let succ = l "n" @@
  l "s" @@ l "z" @@
    (v "s" $ (v "n" $ v "s" $ v "z"))
let plus = l "n1" @@ l "n2" @@
  (v "n1" $ succ $ v "n2")
let mult = l "n1" @@ l "n2" @@
  (v "n1" $ (plus $ v "n2") $ zero)
let b_true = l "x" @@ l "y" @@
  v "x"
let b_false = l "x" @@ l "y" @@
  v "y"
let id = l "x" @@
  v "x"
let b_true_lazy = l "x" @@ l "y" @@
  (v "x" $ id)
let b_false_lazy = l "x" @@ l "y" @@
  (v "y" $ id)
let b_if_lazy = l "c" @@ l "t" @@ l "f" @@
  ((v "c" $ b_true_lazy $ b_false_lazy) $ v "t" $ v "f")
let k = l "x" @@ l "y" @@
  v "x"
let eq0 = l "n" @@
  (v "n" $ (k $ b_false) $ b_true)
let pair = l "x" @@ l "y" @@ l "p" @@
  (v "p" $ v "x" $ v "y")
let first = l "p" @@
  (v "p" $ b_true)
let second = l "p" @@
  (v "p" $ b_false)
let shift = l "p" @@
  (pair $ (second $ v "p") $ (succ $ (second $ v "p")))
let pred = l "n" @@
  (first $ (v "n" $ shift $ (pair $ zero $ zero)))
let omega1 = l "x" @@
  (v "x" $ v "x")
let omega = omega1 $ omega1

let fact = l "f" @@ l "n" @@
  (b_if_lazy $ (eq0 $ v "n") $
    (l "_" one) $
    (l "_" (mult $ v "n" $ (v "f" $ (pred $ v "n")))))

let fixpoint_combinator = l "x" @@ l "y" @@
  (v "y" $ l "z" (v "x" $ v "x" $ v "y" $ v "z"))
let fix = fixpoint_combinator $ fixpoint_combinator

let factorial = l "n" @@
  (fix $ fact $ v "n")

let () =
  let e0 = initial_env() in
  let lt = factorial $ (mkint 6) in
  print_endline (Flow.string_of_lterm lt);
  print_newline();
  let c = eval e0 lt in
  let b, t, env =
    match Flow.getClos c with
    | Some (b, t, env) -> b, t, env
    | None -> failwith "noresult"
  in
  let ut = Flow.unfold (Lam (b, t)) env in
  let tf = Flow.simpl ut 2 in
  print_endline (Flow.string_of_lterm tf);
  Printf.printf "= %d\n" (Flow.int_of_lterm tf)