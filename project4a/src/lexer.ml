open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec make_token input = 
  if (Str.string_match (Str.regexp "^-?[0-9]+") input 0) then
    let str = Str.matched_string input in [Tok_Int(int_of_string str)]
  else if Str.string_match (Str.regexp "(-?[0-9]+)") input 0 then 
    let str = (Str.matched_string input) in let value = (String.sub str 1 ((String.length str) - 2)) in [Tok_Int(int_of_string value)]
  else if Str.string_match (Str.regexp "(-?[0-9]+") input 0 then 
    let str = (Str.matched_string input) in let value = (String.sub str 1 ((String.length str) - 1)) in [Tok_LParen; Tok_Int(int_of_string value)]
  else if Str.string_match (Str.regexp "-?[0-9]+)") input 0 then 
    let str = (Str.matched_string input) in let value = (String.sub str 1 ((String.length str) - 2)) in [Tok_Int(int_of_string value); Tok_RParen]
  else if Str.string_match (Str.regexp "\"[^\"]*\"") input 0 then 
    let str = (Str.matched_string input) in let value = (String.sub str 1 ((String.length str) - 2)) in [Tok_String(value)]
  else if Str.string_match (Str.regexp "^true$") input 0 then [Tok_Bool(true)]
  else if Str.string_match (Str.regexp "^false$") input 0 then [Tok_Bool(false)]
  else if Str.string_match (Str.regexp "(") input 0 then [Tok_LParen] @ 
    (let str = (Str.matched_string input) in let x = (String.sub str 1 ((String.length str) - 1)) in if x <> "" then (make_token x) else [])
  else if Str.string_match (Str.regexp ")") input 0 then [Tok_RParen] @ 
    (let str = (Str.matched_string input) in let x = (String.sub str 1 ((String.length str) - 1)) in if x <> "" then (make_token x) else [])
  else if Str.string_match (Str.regexp "=") input 0 then [Tok_Equal]
  else if Str.string_match (Str.regexp "<>") input 0 then [Tok_NotEqual]
  else if Str.string_match (Str.regexp ">") input 0 then [Tok_Greater]
  else if Str.string_match (Str.regexp "<") input 0 then [Tok_Less]
  else if Str.string_match (Str.regexp ">=") input 0 then [Tok_GreaterEqual]
  else if Str.string_match (Str.regexp "<=") input 0 then [Tok_LessEqual]
  else if Str.string_match (Str.regexp "||") input 0 then [Tok_Or]
  else if Str.string_match (Str.regexp "&&") input 0 then [Tok_And]
  else if Str.string_match (Str.regexp "not") input 0 then [Tok_Not]
  else if Str.string_match (Str.regexp "if") input 0 then [Tok_If]
  else if Str.string_match (Str.regexp "then") input 0 then [Tok_Then]
  else if Str.string_match (Str.regexp "else") input 0 then [Tok_Else] @ 
    (let str = (Str.matched_string input) in let x = (String.sub str 4 ((String.length str) - 4)) in if x <> "" then (make_token x) else [])
  else if Str.string_match (Str.regexp "\\+") input 0 then [Tok_Add] @ 
    (let str = (Str.matched_string input) in let x = (String.sub str 1 ((String.length str) - 1)) in if x <> "" then (make_token x) else [])
  else if Str.string_match (Str.regexp "-") input 0 then [Tok_Sub]
  else if Str.string_match (Str.regexp "\\*") input 0 then [Tok_Mult]
  else if Str.string_match (Str.regexp "/") input 0 then [Tok_Div]
  else if Str.string_match (Str.regexp "//^") input 0 then [Tok_Concat]
  else if Str.string_match (Str.regexp "^let$") input 0 then [Tok_Let]
  else if Str.string_match (Str.regexp "^def$") input 0 then [Tok_Def]
  else if Str.string_match (Str.regexp "^in$") input 0 then [Tok_In]
  else if Str.string_match (Str.regexp "^rec$") input 0 then [Tok_Rec]
  else if Str.string_match (Str.regexp "^fun$") input 0 then [Tok_Fun]
  else if Str.string_match (Str.regexp "->") input 0 then [Tok_Arrow]
  else if Str.string_match (Str.regexp ";;") input 0 then [Tok_DoubleSemi]
  else if Str.string_match (Str.regexp "^[a-zA-Z][a-zA-Z0-9]*$") input 0 then [Tok_ID(input)]
  else raise (InvalidInputException "bad input")


let rec tokenize_aux input = match input with 
  | [] -> []
  | a::b -> (make_token a) @ (tokenize_aux b)

let tokenize input = tokenize_aux (Str.split (Str.regexp "[ \n\r\x0c\t]+") input)