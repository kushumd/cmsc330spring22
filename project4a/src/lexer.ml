open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec make_token input pos = 
  if pos < (String.length input) then 
    if Str.string_match (Str.regexp "(-?[0-9]+)") input pos then 
      let str = Str.matched_string input in Tok_Int(int_of_string (String.sub str 1 ((String.length str) - 2))) :: (make_token input (pos + (String.length str)))
    else if Str.string_match (Str.regexp "[0-9]+") input pos then 
      let str = Str.matched_string input in Tok_Int(int_of_string str) :: (make_token input (pos + (String.length str)))
    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then 
      let str = (String.sub (Str.matched_string input) 1 (String.length (Str.matched_string input) - 2)) in Tok_String(str) :: (make_token input (pos + (String.length str) + 2))
    else if Str.string_match (Str.regexp "true") input pos then Tok_Bool(true) :: (make_token input (pos+4))
    else if Str.string_match (Str.regexp "false") input pos then Tok_Bool(false) :: (make_token input (pos+5))
    else if Str.string_match (Str.regexp "=") input pos then Tok_Equal :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "<>") input pos then Tok_NotEqual :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp ">") input pos then Tok_Greater :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "<") input pos then Tok_Less :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp ">=") input pos then Tok_GreaterEqual :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "<=") input pos then Tok_LessEqual :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "||") input pos then Tok_Or :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "&&") input pos then Tok_And :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "not") input pos then Tok_Not :: (make_token input (pos+3))
    else if Str.string_match (Str.regexp "if") input pos then Tok_If :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "then") input pos then Tok_Then :: (make_token input (pos+4))
    else if Str.string_match (Str.regexp "else") input pos then Tok_Else :: (make_token input (pos+4)) 
    else if Str.string_match (Str.regexp "->") input pos then Tok_Arrow :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "\\+") input pos then Tok_Add :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "-") input pos then Tok_Sub :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "\\*") input pos then Tok_Mult :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "/") input pos then Tok_Div :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "\\^") input pos then Tok_Concat :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp "let") input pos then Tok_Let :: (make_token input (pos+3))
    else if Str.string_match (Str.regexp "def") input pos then Tok_Def :: (make_token input (pos+3))
    else if Str.string_match (Str.regexp "in") input pos then Tok_In :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "rec") input pos then Tok_Rec :: (make_token input (pos+3))
    else if Str.string_match (Str.regexp "fun") input pos then Tok_Fun :: (make_token input (pos+3))
    else if Str.string_match (Str.regexp ";;") input pos then Tok_DoubleSemi :: (make_token input (pos+2))
    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then 
      let new_id = (Str.matched_string input) in Tok_ID(new_id) :: (make_token input (pos+(String.length new_id)))
    else if Str.string_match (Str.regexp "(") input pos then Tok_LParen :: (make_token input (pos+1))
    else if Str.string_match (Str.regexp ")") input pos then Tok_RParen :: (make_token input (pos+1))
    else make_token input (pos+1)
  else []

let tokenize input = make_token input 0