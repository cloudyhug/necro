type ('literal, 'state, 'value, _, _) term = 
  | C : 'literal -> ('literal, 'state, 'value, 'state, 'value) term
  | P : ('literal, 'state, 'value) expr * ('literal, 'state, 'value) expr -> ('literal, 'state, 'value, 'state, 'value) term
  | M : ('literal, 'state, 'value) expr * ('literal, 'state, 'value) expr -> ('literal, 'state, 'value, 'state, 'value) term
  | T : ('literal, 'state, 'value) expr * ('literal, 'state, 'value) expr -> ('literal, 'state, 'value, 'state, 'value) term
  | D : ('literal, 'state, 'value) expr * ('literal, 'state, 'value) expr -> ('literal, 'state, 'value, 'state, 'value) term
and ('literal, 'state, 'value) expr = ('literal, 'state, 'value, 'state, 'value) term
module type FLOW = sig
  type literal
  type state
  type value
  val mkstate : unit -> state
  val print_value : value -> unit
  val litToVal : literal -> value option
  val valToLit : value -> literal option
  val plus : literal -> literal -> value option
  val minus : literal -> literal -> value option
  val times : literal -> literal -> value option
  val div : literal -> literal -> value option
end

module type INTERPRETER = sig
  type literal
  type state
  type value
  val mkstate : unit -> state
  val print_value : value -> unit
  val eval : 'a -> (literal, state, value, 'a, 'b) term -> 'b
end

module MakeInterpreter (F : FLOW) = struct
  include F
  type f = { f : 'a 'b. 'a -> (literal, state, value, 'a, 'b) term -> 'b }
  let eval_lit {f} x_t state = 
    let x_o = 
      match litToVal x_t with
      | None -> failwith "litToVal"
      | Some result -> result
    in
    x_o
  let eval_add {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_t3 = 
      match valToLit x_f1 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_f2 = f state x_t2 in
    let x_t4 = 
      match valToLit x_f2 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_o = 
      match plus x_t3 x_t4 with
      | None -> failwith "plus"
      | Some result -> result
    in
    x_o
  let eval_sub {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_t3 = 
      match valToLit x_f1 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_f2 = f state x_t2 in
    let x_t4 = 
      match valToLit x_f2 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_o = 
      match minus x_t3 x_t4 with
      | None -> failwith "minus"
      | Some result -> result
    in
    x_o
  let eval_mul {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_t3 = 
      match valToLit x_f1 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_f2 = f state x_t2 in
    let x_t4 = 
      match valToLit x_f2 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_o = 
      match times x_t3 x_t4 with
      | None -> failwith "times"
      | Some result -> result
    in
    x_o
  let eval_div {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_t3 = 
      match valToLit x_f1 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_f2 = f state x_t2 in
    let x_t4 = 
      match valToLit x_f2 with
      | None -> failwith "valToLit"
      | Some result -> result
    in
    let x_o = 
      match div x_t3 x_t4 with
      | None -> failwith "div"
      | Some result -> result
    in
    x_o
  let rec eval : type a b. a -> (literal, state, value, a, b) term -> b =
    fun state term -> 
      match term with
      | C x_t -> eval_lit {f = eval} x_t state
      | P (x_t1, x_t2) -> eval_add {f = eval} x_t1 x_t2 state
      | M (x_t1, x_t2) -> eval_sub {f = eval} x_t1 x_t2 state
      | T (x_t1, x_t2) -> eval_mul {f = eval} x_t1 x_t2 state
      | D (x_t1, x_t2) -> eval_div {f = eval} x_t1 x_t2 state
end
