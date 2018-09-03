type ('ident, 'lit, 'state, 'value, 'vint, 'vbool, _, _) term = 
  | Const : 'lit -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'value) term
  | Var : 'ident -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'value) term
  | Plus : ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'value) term
  | EqualEqual : ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'value) term
  | Bang : ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'value) term
  | Skip : ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'state) term
  | ColonEqual : 'ident * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'state) term
  | SemicolonSemicolon : ('ident, 'lit, 'state, 'value, 'vint, 'vbool) stat * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) stat -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'state) term
  | If : ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) stat * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) stat -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'state) term
  | While : ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr * ('ident, 'lit, 'state, 'value, 'vint, 'vbool) stat -> ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'state) term
and ('ident, 'lit, 'state, 'value, 'vint, 'vbool) expr = ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'value) term
and ('ident, 'lit, 'state, 'value, 'vint, 'vbool) stat = ('ident, 'lit, 'state, 'value, 'vint, 'vbool, 'state, 'state) term
module type FLOW = sig
  type ident
  type lit
  type state
  type value
  type vint
  type vbool
  val initial_state : unit -> state
  val print_state : state -> unit
  val print_value : value -> unit
  val litToVal : lit -> value option
  val read : ident -> state -> value option
  val isInt : value -> vint option
  val add : vint -> vint -> value option
  val eq : vint -> vint -> value option
  val isBool : value -> vbool option
  val neg : vbool -> value option
  val write : ident -> state -> value -> state option
  val id : state -> state option
  val isTrue : vbool -> unit option
  val isFalse : vbool -> unit option
end

module type INTERPRETER = sig
  type ident
  type lit
  type state
  type value
  type vint
  type vbool
  val initial_state : unit -> state
  val print_state : state -> unit
  val print_value : value -> unit
  val eval : 'a -> (ident, lit, state, value, vint, vbool, 'a, 'b) term -> 'b
end

module MakeInterpreter (F : FLOW) = struct
  include F
  type f = { f : 'a 'b. 'a -> (ident, lit, state, value, vint, vbool, 'a, 'b) term -> 'b }
  let eval_lit_int {f} x_t state = 
    let x_o = 
      match litToVal x_t with
      | None -> failwith "litToVal"
      | Some result -> result
    in
    x_o
  let eval_var {f} x_t state = 
    let x_o = 
      match read x_t state with
      | None -> failwith "read"
      | Some result -> result
    in
    x_o
  let eval_add {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_f1' = 
      match isInt x_f1 with
      | None -> failwith "isInt"
      | Some result -> result
    in
    let x_f2 = f state x_t2 in
    let x_f2' = 
      match isInt x_f2 with
      | None -> failwith "isInt"
      | Some result -> result
    in
    let x_o = 
      match add x_f1' x_f2' with
      | None -> failwith "add"
      | Some result -> result
    in
    x_o
  let eval_eq {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_f1' = 
      match isInt x_f1 with
      | None -> failwith "isInt"
      | Some result -> result
    in
    let x_f2 = f state x_t2 in
    let x_f2' = 
      match isInt x_f2 with
      | None -> failwith "isInt"
      | Some result -> result
    in
    let x_o = 
      match eq x_f1' x_f2' with
      | None -> failwith "eq"
      | Some result -> result
    in
    x_o
  let eval_neg {f} x_t state = 
    let x_f1 = f state x_t in
    let x_f1' = 
      match isBool x_f1 with
      | None -> failwith "isBool"
      | Some result -> result
    in
    let x_o = 
      match neg x_f1' with
      | None -> failwith "neg"
      | Some result -> result
    in
    x_o
  let eval_skip {f} state = 
    let x_o = 
      match id state with
      | None -> failwith "id"
      | Some result -> result
    in
    x_o
  let eval_asn {f} x_t1 x_t2 state = 
    let x_f2 = f state x_t2 in
    let x_o = 
      match write x_t1 state x_f2 with
      | None -> failwith "write"
      | Some result -> result
    in
    x_o
  let eval_seq {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_o = f x_f1 x_t2 in
    x_o
  let eval_if {f} x_t1 x_t2 x_t3 state = 
    let x_f1 = f state x_t1 in
    let x_f1' = 
      match isBool x_f1 with
      | None -> failwith "isBool"
      | Some result -> result
    in
    let x_o = 
      try 
        let () = 
          match isTrue x_f1' with
          | None -> failwith "isTrue"
          | Some result -> result
        in
        let x_o = f state x_t2 in
        x_o
      with _ ->
      try 
        let () = 
          match isFalse x_f1' with
          | None -> failwith "isFalse"
          | Some result -> result
        in
        let x_o = f state x_t3 in
        x_o
      with _ ->
        failwith "All branches have failed"
    in
    x_o
  let eval_while {f} x_t1 x_t2 state = 
    let x_f1 = f state x_t1 in
    let x_f1' = 
      match isBool x_f1 with
      | None -> failwith "isBool"
      | Some result -> result
    in
    let x_o = 
      try 
        let () = 
          match isTrue x_f1' with
          | None -> failwith "isTrue"
          | Some result -> result
        in
        let x_f2 = f state x_t2 in
        let x_o = f x_f2 (While (x_t1, x_t2)) in
        x_o
      with _ ->
      try 
        let () = 
          match isFalse x_f1' with
          | None -> failwith "isFalse"
          | Some result -> result
        in
        let x_o = 
          match id state with
          | None -> failwith "id"
          | Some result -> result
        in
        x_o
      with _ ->
        failwith "All branches have failed"
    in
    x_o
  let rec eval : type a b. a -> (ident, lit, state, value, vint, vbool, a, b) term -> b =
    fun state term -> 
      match term with
      | Const x_t -> eval_lit_int {f = eval} x_t state
      | Var x_t -> eval_var {f = eval} x_t state
      | Plus (x_t1, x_t2) -> eval_add {f = eval} x_t1 x_t2 state
      | EqualEqual (x_t1, x_t2) -> eval_eq {f = eval} x_t1 x_t2 state
      | Bang x_t -> eval_neg {f = eval} x_t state
      | Skip -> eval_skip {f = eval} state
      | ColonEqual (x_t1, x_t2) -> eval_asn {f = eval} x_t1 x_t2 state
      | SemicolonSemicolon (x_t1, x_t2) -> eval_seq {f = eval} x_t1 x_t2 state
      | If (x_t1, x_t2, x_t3) -> eval_if {f = eval} x_t1 x_t2 x_t3 state
      | While (x_t1, x_t2) -> eval_while {f = eval} x_t1 x_t2 state
end
