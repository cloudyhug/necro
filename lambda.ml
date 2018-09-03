type ('ident, 'clos, 'env, _, _) term = 
  | Lam : 'ident * ('ident, 'clos, 'env) lterm -> ('ident, 'clos, 'env, 'env, 'clos) term
  | Var : 'ident -> ('ident, 'clos, 'env, 'env, 'clos) term
  | App : ('ident, 'clos, 'env) lterm * ('ident, 'clos, 'env) lterm -> ('ident, 'clos, 'env, 'env, 'clos) term
and ('ident, 'clos, 'env) lterm = ('ident, 'clos, 'env, 'env, 'clos) term
module type FLOW = sig
  type ident
  type clos
  type env
  val initial_env : unit -> env
  val print_clos : clos -> unit
  val mkClos : ident -> (ident, clos, env) lterm -> env -> clos option
  val getClos : clos -> (ident * (ident, clos, env) lterm * env) option
  val extEnv : env -> ident -> clos -> env option
  val getEnv : ident -> env -> clos option
end

module type INTERPRETER = sig
  type ident
  type clos
  type env
  val initial_env : unit -> env
  val print_clos : clos -> unit
  val eval : 'a -> (ident, clos, env, 'a, 'b) term -> 'b
end

module MakeInterpreter (F : FLOW) = struct
  include F
  type f = { f : 'a 'b. 'a -> (ident, clos, env, 'a, 'b) term -> 'b }
  let eval_lambda {f} x_t1 x_t2 state = 
    let x_o = 
      match mkClos x_t1 x_t2 state with
      | None -> failwith "mkClos"
      | Some result -> result
    in
    x_o
  let eval_var {f} x_t state = 
    let x_o = 
      match getEnv x_t state with
      | None -> failwith "getEnv"
      | Some result -> result
    in
    x_o
  let eval_app {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_t3, x_t4, x_f2 = 
      match getClos x_f1 with
      | None -> failwith "getClos"
      | Some result -> result
    in
    let x_f3 = f state x_t2 in
    let x_f4 = 
      match extEnv x_f2 x_t3 x_f3 with
      | None -> failwith "extEnv"
      | Some result -> result
    in
    let x_o = f x_f4 x_t4 in
    x_o
  let rec eval : type a b. a -> (ident, clos, env, a, b) term -> b =
    fun state term -> 
      match term with
      | Lam (x_t1, x_t2) -> eval_lambda {f = eval} x_t1 x_t2 state
      | Var x_t -> eval_var {f = eval} x_t state
      | App (x_t1, x_t2) -> eval_app {f = eval} x_t1 x_t2 state
end
