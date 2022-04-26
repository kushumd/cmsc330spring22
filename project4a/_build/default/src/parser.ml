open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with 
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

let rec in_subset (toks: token list) (n: int) = match toks with 
| [] -> raise (InvalidInputException "bad let expression")
| a::b -> match a with 
  | Tok_Let -> a::(in_subset b (n+1))
  | Tok_In -> if n = 0 then [] else a::(in_subset b (n-1))
  | _ -> a::(in_subset b n)

let rec then_subset (toks: token list) (n: int) = match toks with 
| [] -> raise (InvalidInputException "bad fun expression")
| a::b -> match a with 
  | Tok_If -> a::(then_subset b (n+1))
  | Tok_Then -> if n = 0 then [] else a::(then_subset b (n-1))
  | _ -> a::(then_subset b n)
  
let rec else_subset (toks: token list) (n: int) = match toks with 
| [] -> raise (InvalidInputException "bad fun expression")
| a::b -> match a with 
  | Tok_If -> a::(else_subset b (n+1))
  | Tok_Else -> if n = 0 then [] else a::(else_subset b (n-1))
  | _ -> a::(else_subset b n)

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let rec parse_let toks = match (lookahead_many toks 1) with 
  | Some Tok_Rec -> ( match lookahead_many toks 2 with 
    | Some Tok_ID(x) -> let toks2 = match_many toks [Tok_Let; Tok_Rec; Tok_ID(x); Tok_Equal] in
        let t1, e1 = parse_expr toks2 in let toks3 = match_token t1 Tok_In in
        let t2, e2 = parse_expr toks3 in (t2, Let(x, true, e1, e2));
    | _ -> raise (InvalidInputException "bad let expression") )
  | Some Tok_ID(x) -> let toks2 = match_many toks [Tok_Let; Tok_ID(x); Tok_Equal] in
    let t1, e1 = parse_expr toks2 in let toks3 = match_token t1 Tok_In in
    let t2, e2 = parse_expr toks3 in (t2, Let(x, false, e1, e2))
  | _ -> raise (InvalidInputException "bad let expression") 
  and parse_fun toks = match lookahead_many toks 1 with 
  | Some Tok_ID (x) -> let toks2 = match_many toks [Tok_Fun; Tok_ID(x); Tok_Arrow] in let t1, e1 = parse_expr toks2 in (t1, Fun(x, e1))
  | _ -> raise (InvalidInputException "bad fun expression")
  and parse_if toks = let toks2 = match_token toks Tok_If in
    let t1, e1 = parse_expr toks2 in let toks3 = match_token t1 Tok_Then in
    let t2, e2 = parse_expr toks3 in let toks4 = match_token t2 Tok_Else in
    let t3, e3 = parse_expr toks4 in (t3, If(e1, e2, e3))
  and parse_or toks = let t1, e1 = parse_and toks in match lookahead t1 with 
  | Some Tok_Or -> let toks2 = match_token t1 Tok_Or in let t2, e2 = parse_or toks2 in (t2, Binop(Or, e1, e2))
  | _ -> (t1, e1)
  and parse_and toks = let t1, e1 = parse_equality toks in match lookahead t1 with 
  | Some Tok_And -> let toks2 = match_token t1 Tok_And in let t2, e2 = parse_and toks2 in (t2, Binop(And, e1, e2))
  | _ -> (t1, e1)
  and parse_equality toks = let t1, e1 = parse_relation toks in match lookahead t1 with 
  | Some Tok_Equal -> let toks2 = match_token t1 Tok_Equal in let t2, e2 = parse_equality toks2 in (t2, Binop(Equal, e1, e2))
  | Some Tok_NotEqual -> let toks2 = match_token t1 Tok_NotEqual in let t2, e2 = parse_equality toks2 in (t2, Binop(NotEqual, e1, e2))
  | _ -> (t1, e1)
  and parse_relation toks = let t1, e1 = parse_add toks in match lookahead t1 with 
  | Some Tok_Less -> let toks2 = match_token t1 Tok_Less in let t2, e2 = parse_relation toks2 in (t2, Binop(Less, e1, e2))
  | Some Tok_Greater -> let toks2 = match_token t1 Tok_Greater in let t2, e2 = parse_relation toks2 in (t2, Binop(Greater, e1, e2))
  | Some Tok_LessEqual -> let toks2 = match_token t1 Tok_LessEqual in let t2, e2 = parse_relation toks2 in (t2, Binop(LessEqual, e1, e2))
  | Some Tok_GreaterEqual -> let toks2 = match_token t1 Tok_GreaterEqual in let t2, e2 = parse_relation toks2 in (t2, Binop(GreaterEqual, e1, e2))
  | _ -> (t1, e1)
  and parse_add toks = let t1, e1 = parse_multiply toks in match lookahead t1 with 
  | Some Tok_Add -> let toks2 = match_token t1 Tok_Add in let t2, e2 = parse_add toks2 in (t2, Binop(Add, e1, e2))
  | Some Tok_Sub -> let toks2 = match_token t1 Tok_Sub in let t2, e2 = parse_add toks2 in (t2, Binop(Sub, e1, e2))
  | _ -> (t1, e1)
  and parse_multiply toks = let t1, e1 = parse_concat toks in match lookahead t1 with 
  | Some Tok_Mult -> let toks2 = match_token t1 Tok_Mult in let t2, e2 = parse_multiply toks2 in (t2, Binop(Mult, e1, e2))
  | Some Tok_Div -> let toks2 = match_token t1 Tok_Div in let t2, e2 = parse_multiply toks2 in (t2, Binop(Div, e1, e2))
  | _ -> (t1, e1)
  and parse_concat toks = let t1, e1 = parse_unary toks in match lookahead t1 with 
  | Some Tok_Concat -> let toks2 = match_token t1 Tok_Concat in let t2, e2 = parse_add toks2 in (t2, Binop(Concat, e1, e2))
  | _ -> (t1, e1)
  and parse_unary toks = match lookahead toks with 
  | Some Tok_Not -> let toks2 = match_token toks Tok_Not in let t1, e1 = parse_unary toks2 in (t1, Not(e1))
  | _ -> parse_call toks
  and parse_call toks = let t1, e1 = parse_primary toks in match t1 with 
  | [] -> (t1, e1)
  | _ -> let t2, e2 = parse_primary t1 in (t2, FunctionCall(e1, e2))
  and parse_primary toks = match lookahead toks with 
  | Some Tok_Int(x) -> let toks2 = match_token toks (Tok_Int(x)) in (toks2, Value(Int(x)))
  | Some Tok_Bool(x) -> let toks2 = match_token toks (Tok_Bool(x)) in (toks2, Value(Bool(x)))
  | Some Tok_String(x) -> let toks2 = match_token toks (Tok_String(x)) in (toks2, Value(String(x)))
  | Some Tok_ID(x) -> let toks2 = match_token toks (Tok_ID(x)) in (toks2, ID(x))
  | Some Tok_RParen -> let toks2 = match_token toks Tok_RParen in let t1, e1 = parse_expr toks2 in let toks3 = match_token t1 Tok_LParen in (toks3, e1)
  | _ -> raise (InvalidInputException "bad primary")
  in
  match lookahead toks with 
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"