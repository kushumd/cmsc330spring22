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
  let eval_value env e = match e with 
  | Value (x) -> x
  | _ -> raise (TypeError ("bad value"))
  and eval_ID env e = match e with 
  | ID (x) -> lookup env x
  | _ -> raise (TypeError ("bad ID"))
  and eval_fun env e = match e with 
  | Fun (v, e1) -> Closure (env, v, e1)
  | _ -> raise (TypeError ("no fun"))
  and eval_not env e = match e with 
  | Not (e2) -> ( match (eval_expr env e2) with 
    | Bool (x) -> if x then Bool(false) else Bool(true)
    | _ -> raise (TypeError ("bad not")) )
  | _ -> raise (TypeError ("bad not"))
  and eval_binop env e = match e with 
  | Binop (o, e1, e2) -> ( match o, (eval_expr env e1), (eval_expr env e2) with 
    | Add, Int (x), Int (y) -> Int (x + y)
    | Sub, Int (x), Int (y) -> Int (x - y)
    | Mult, Int (x), Int (y) -> Int (x * y)
    | Div, Int (x), Int (y) -> if y = 0 then raise (DivByZeroError) else Int (x / y)
    | Concat, String (x), String (y) -> String (x ^ y)
    | Greater, Int (x), Int (y)-> Bool (x > y)
    | Less, Int (x), Int (y) -> Bool (x < y)
    | GreaterEqual, Int (x), Int (y) -> Bool (x >= y)
    | LessEqual, Int (x), Int (y) -> Bool (x <= y)
    | Equal, Int (x), Int (y) -> Bool (x = y)
    | Equal, String (x), String (y) -> Bool (x = y)
    | Equal, Bool (x), Bool (y) -> Bool (x = y)
    | NotEqual, Int (x), Int (y) -> Bool (x <> y)
    | NotEqual, String (x), String (y) -> Bool (x <> y)
    | NotEqual, Bool (x), Bool (y) -> Bool (x <> y)
    | Or, Bool (x), Bool (y) -> Bool (x || y) 
    | And, Bool (x), Bool (y) -> Bool (x && y)
    | _, _, _ -> raise (TypeError ("you did a binop wrong lmao")) )
  | _ -> raise (TypeError ("bad binary operation"))
  and eval_if env e = match e with 
  | If (g, t, f) -> ( match (eval_expr env g) with 
    | Bool (true) -> eval_expr env t
    | Bool (false) -> eval_expr env f
    | _ -> raise (TypeError ("guard not a boolean")) )
  | _ -> raise (TypeError ("bad if"))
  and eval_call env e = match e with 
  | FunctionCall (e1, e2) -> ( match (eval_expr env e1), (eval_expr env e2) with
    | Closure (a, x, e), v -> eval_expr (extend a x v) e
    | _, _ -> raise (TypeError ("bad function call")) )
  | _ -> raise (TypeError ("bad function call"))
  and eval_let env e = match e with 
  | Let (v, r, e1, e2) -> ( if r then (
      let env2 = (extend_tmp env v) in let x = (eval_expr env2 e1) in (update env2 v x); eval_expr env2 e2
    ) else let x = (eval_expr env e1) in eval_expr (extend env v x) e2 )
  | _ -> raise (TypeError ("bad let"))
  in 
  match e with 
  | Value (_) -> eval_value env e
  | ID (_) -> eval_ID env e
  | Fun (_, _) -> eval_fun env e
  | Not (_) -> eval_not env e
  | Binop (_, _, _) -> eval_binop env e
  | If (_, _, _) -> eval_if env e
  | FunctionCall (_, _) -> eval_call env e
  | Let (_, _, _, _) -> eval_let env e

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

let eval_mutop env m = 
  let eval_def env m = match m with 
  | Def (v, e) -> let env2 = (extend_tmp env v) in let x = (eval_expr env2 e) in (update env2 v x); (env2, Some x)
  | _ -> raise (TypeError ("bad def"))
  and eval_mexp env m = (env, Some (eval_expr env m))
  in match m with 
  | Def (_, _) -> eval_def env m
  | Expr (e) -> eval_mexp env e
  | NoOp -> ([], None)