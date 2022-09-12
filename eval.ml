open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  |Value v -> eval_Value v
  |ID name -> eval_ID env name
  |Fun (name, expr) -> eval_Fun env name expr
  |Not expr -> eval_Not env expr
  |Binop (op, expr1, expr2) -> eval_Binop env op expr1 expr2
  |If (expr1, expr2, expr3) -> eval_If env expr1 expr2 expr3
  |FunctionCall (expr1, expr2) -> eval_FunctionCall env expr1 expr2
  |Let (name, b, expr1, expr2) -> eval_Let env name b expr1 expr2
and eval_Value value =
  match value with
  |Int i -> value
  |Bool b -> value
  |String s -> value
  |Closure (env, var, expr) -> value
and eval_ID env name = 
  lookup env name
and eval_Fun env name expr =
  Closure (env, name, expr)
and eval_Not env expr =
  let eval = eval_expr env expr in
  match eval with
  |Bool b -> Bool (not b)
  |_ -> raise (TypeError ":(")
and eval_Binop env op expr1 expr2 =
  let eval1 = eval_expr env expr1 in
  let eval2 = eval_expr env expr2 in
  match op with
  |Add -> eval_Add eval1 eval2
  |Sub -> eval_Sub eval1 eval2
  |Mult -> eval_Mult eval1 eval2
  |Div -> eval_Div eval1 eval2
  |Concat -> eval_Concat eval1 eval2
  |Greater -> eval_Greater eval1 eval2
  |Less -> eval_Less eval1 eval2
  |GreaterEqual -> eval_GreaterEqual eval1 eval2
  |LessEqual -> eval_LessEqual eval1 eval2
  |Equal -> eval_Equal eval1 eval2
  |NotEqual -> eval_NotEqual eval1 eval2
  |Or -> eval_Or eval1 eval2
  |And -> eval_And eval1 eval2
and eval_Add eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Int (i1 + i2)
  |_ -> raise (TypeError ":(")
and eval_Sub eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Int (i1 - i2)
  |_ -> raise (TypeError ":(")
and eval_Mult eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Int (i1 * i2)
  |_ -> raise (TypeError ":(")
and eval_Div eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> if i2 = 0 then raise (DivByZeroError) else Int (i1 / i2)
  |_ -> raise (TypeError ":(")
and eval_Concat eval1 eval2 =
  match (eval1, eval2) with
  |(String s1, String s2) -> String (s1 ^ s2)
  |_ -> raise (TypeError ":(")
and eval_Greater eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Bool (i1 > i2)
  |_ -> raise (TypeError ":(")
and eval_Less eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Bool (i1 < i2)
  |_ -> raise (TypeError ":(")
and eval_GreaterEqual eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Bool (i1 >= i2)
  |_ -> raise (TypeError ":(")
and eval_LessEqual eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Bool (i1 <= i2)
  |_ -> raise (TypeError ":(")
and eval_Equal eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Bool (i1 = i2)
  |(String s1, String s2) -> Bool (s1 = s2)
  |(Bool b1, Bool b2) -> Bool (b1 = b2)
  |_ -> raise (TypeError ":(")
and eval_NotEqual eval1 eval2 =
  match (eval1, eval2) with
  |(Int i1, Int i2) -> Bool (i1 <> i2)
  |(String s1, String s2) -> Bool (s1 <> s2)
  |(Bool b1, Bool b2) -> Bool (b1 <> b2)
  |_ -> raise (TypeError ":(")
and eval_Or eval1 eval2 =
  match (eval1, eval2) with
  |(Bool b1, Bool b2) -> Bool (b1 || b2)
  |_ -> raise (TypeError ":(")
and eval_And eval1 eval2 =
  match (eval1, eval2) with
  |(Bool b1, Bool b2) -> Bool (b1 && b2)
  |_ -> raise (TypeError ":(")
and eval_If env expr1 expr2 expr3 =
  let eval1 = eval_expr env expr1 in
  match eval1 with 
  |Bool b -> if b then eval_expr env expr2 else eval_expr env expr3
  |_ -> raise (TypeError ":(")
and eval_FunctionCall env expr1 expr2 =
  let eval1 = eval_expr env expr1 in
  let eval2 = eval_expr env expr2 in
  match (eval1, eval2) with
  |(Closure (sub_env, name, expr), Int i) -> let new_env = extend sub_env name eval2 in eval_expr new_env expr
  |(Closure (sub_env, name, expr), String s) -> let new_env = extend sub_env name eval2 in eval_expr new_env expr
  |(Closure (sub_env, name, expr), Bool b) -> let new_env = extend sub_env name eval2 in eval_expr new_env expr
  |(Closure (sub_env, name, expr), Closure (a, n, e)) -> let new_env = extend sub_env name eval2 in eval_expr new_env expr
  |_ -> raise (TypeError ":(")
and eval_Let env name b expr1 expr2 = 
  if b then eval_Let_Rec env name expr1 expr2 else eval_Let_No_Rec env name expr1 expr2
and eval_Let_No_Rec env name expr1 expr2 = 
  let eval1 = eval_expr env expr1 in
  match eval1 with
  |Int i -> let new_env = extend env name eval1 in eval_expr new_env expr2
  |Bool b -> let new_env = extend env name eval1 in eval_expr new_env expr2
  |String s -> let new_env = extend env name eval1 in eval_expr new_env expr2
  |Closure (a, n, e) -> let new_env = extend env name eval1 in eval_expr new_env expr2
and eval_Let_Rec env name expr1 expr2 =
  let temp_env = extend_tmp env name in
  let temp_eval1 = eval_expr temp_env expr1 in
  match temp_eval1 with
  |Int i -> (update temp_env name temp_eval1; eval_expr temp_env expr2)
  |String s -> (update temp_env name temp_eval1; eval_expr temp_env expr2)
  |Bool b -> (update temp_env name temp_eval1; eval_expr temp_env expr2)
  |Closure (a, n, e) -> (update temp_env name temp_eval1; eval_expr temp_env expr2)
;;

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let rec eval_mutop env m = 
  match m with
  |Def (var, expr) -> eval_Def env var expr
  |Expr expr -> eval_Expr env expr
  |NoOp -> (env, None)
and eval_Def env var expr =
  let new_env = extend_tmp env var in
  let eval1 = eval_expr new_env expr in
  (update new_env var eval1; (new_env, Some eval1))
and eval_Expr env expr = 
  (env, Some (eval_expr env expr))
;;