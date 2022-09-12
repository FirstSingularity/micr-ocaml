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

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let (rem_toks, expr) = parse_Expr toks in
  if rem_toks <> [] then raise (InvalidInputException ":(")
  else (rem_toks, expr)
and parse_Expr toks =
  match lookahead toks with
  |Some Tok_Let -> parse_LetExpr toks
  |Some Tok_Fun -> parse_FunctionExpr toks
  |Some Tok_If -> parse_IfExpr toks
  |Some Tok_Not -> parse_OrExpr toks
  |Some Tok_Int i -> parse_OrExpr toks
  |Some Tok_Bool b -> parse_OrExpr toks
  |Some Tok_String s -> parse_OrExpr toks
  |Some Tok_ID d -> parse_OrExpr toks
  |Some Tok_LParen -> parse_OrExpr toks
  |_ -> raise (InvalidInputException ":(")
and parse_LetExpr toks =
  let tok2 = match_token toks Tok_Let in
  let (tok3, is_r) = parse_Recursion tok2 in
  match lookahead tok3 with
  |Some Tok_ID d -> let tok4 = match_token (match_token tok3 (Tok_ID d)) Tok_Equal in
    let (tok5, expr1) = parse_Expr tok4 in let (tok6, expr2) = parse_Expr (match_token tok5 Tok_In) in
    (tok6, Let(d, is_r, expr1, expr2))
  |_ -> raise (InvalidInputException ":(")
and parse_Recursion toks =
  match lookahead toks with
  |Some Tok_Rec -> (match_token toks Tok_Rec, true)
  |_ -> (toks, false)
and parse_FunctionExpr toks =
  let tok2 = match_token toks Tok_Fun in
  match lookahead tok2 with
  |Some Tok_ID d -> let tok3 = match_token (match_token tok2 (Tok_ID d)) Tok_Arrow in 
    let (tok4, expr) = parse_Expr tok3 in (tok4, Fun(d, expr))
  |_ -> raise (InvalidInputException ":(")
and parse_IfExpr toks =
  let (tok2, expr2) = parse_Expr (match_token toks Tok_If) in let (tok3, expr3) = parse_Expr (match_token tok2 Tok_Then) in
  let (tok4, expr4) = parse_Expr (match_token tok3 Tok_Else) in (tok4, If(expr2, expr3, expr4))
and parse_OrExpr toks =
  let (tok2, expr2) = parse_AndExpr toks in 
  match lookahead tok2 with
  |Some Tok_Or -> let tok3 = match_token tok2 Tok_Or in let (tok4, expr4) = parse_OrExpr tok3 in
    (tok4, Binop(Or, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_AndExpr toks =
  let (tok2, expr2) = parse_EqualityExpr toks in
  match lookahead tok2 with
  |Some Tok_And -> let tok3 = match_token tok2 Tok_And in let (tok4, expr4) = parse_AndExpr tok3 in
    (tok4, Binop(And, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_EqualityExpr toks =
  let (tok2, expr2) = parse_RelationalExpr toks in
  match lookahead tok2 with
  |Some Tok_Equal -> let tok3 = match_token tok2 Tok_Equal in let (tok4, expr4) = parse_EqualityExpr tok3 in
    (tok4, Binop(Equal, expr2, expr4))
  |Some Tok_NotEqual -> let tok3 = match_token tok2 Tok_NotEqual in let (tok4, expr4) = parse_EqualityExpr tok3 in
    (tok4, Binop(NotEqual, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_RelationalExpr toks =
  let (tok2, expr2) = parse_AdditiveExpr toks in
  match lookahead tok2 with
  |Some Tok_LessEqual -> let tok3 = match_token tok2 Tok_LessEqual in let (tok4, expr4) = parse_RelationalExpr tok3 in
    (tok4, Binop(LessEqual, expr2, expr4))
  |Some Tok_GreaterEqual -> let tok3 = match_token tok2 Tok_GreaterEqual in let (tok4, expr4) = parse_RelationalExpr tok3 in
    (tok4, Binop(GreaterEqual, expr2, expr4))
  |Some Tok_Less -> let tok3 = match_token tok2 Tok_Less in let (tok4, expr4) = parse_RelationalExpr tok3 in
    (tok4, Binop(Less, expr2, expr4))
  |Some Tok_Greater -> let tok3 = match_token tok2 Tok_Greater in let (tok4, expr4) = parse_RelationalExpr tok3 in
    (tok4, Binop(Greater, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_AdditiveExpr toks =
  let (tok2, expr2) = parse_MultiplicativeExpr toks in
  match lookahead tok2 with
  |Some Tok_Add -> let tok3 = match_token tok2 Tok_Add in let (tok4, expr4) = parse_AdditiveExpr tok3 in
    (tok4, Binop(Add, expr2, expr4))
  |Some Tok_Sub -> let tok3 = match_token tok2 Tok_Sub in let (tok4, expr4) = parse_AdditiveExpr tok3 in
    (tok4, Binop(Sub, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_MultiplicativeExpr toks =
  let (tok2, expr2) = parse_ConcatExpr toks in
  match lookahead tok2 with
  |Some Tok_Mult -> let tok3 = match_token tok2 Tok_Mult in let (tok4, expr4) = parse_MultiplicativeExpr tok3 in
    (tok4, Binop(Mult, expr2, expr4))
  |Some Tok_Div -> let tok3 = match_token tok2 Tok_Div in let (tok4, expr4) = parse_MultiplicativeExpr tok3 in
    (tok4, Binop(Div, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_ConcatExpr toks =
  let (tok2, expr2) = parse_UnaryExpr toks in
  match lookahead tok2 with
  |Some Tok_Concat -> let tok3 = match_token tok2 Tok_Concat in let (tok4, expr4) = parse_ConcatExpr tok3 in
    (tok4, Binop(Concat, expr2, expr4))
  |_ -> (tok2, expr2)
and parse_UnaryExpr toks =
  match lookahead toks with
  |Some Tok_Not -> let tok2 = match_token toks Tok_Not in let (tok3, expr3) = parse_UnaryExpr tok2 in
    (tok3, Not(expr3))
  |_ -> parse_FunctionCallExpr toks
and parse_FunctionCallExpr toks =
  let (tok2, expr2) = parse_PrimaryExpr toks in
  match lookahead tok2 with
  |Some Tok_Int i -> let (tok3, expr3) = parse_PrimaryExpr tok2 in (tok3, FunctionCall(expr2, expr3))
  |Some Tok_Bool b -> let (tok3, expr3) = parse_PrimaryExpr tok2 in (tok3, FunctionCall(expr2, expr3))
  |Some Tok_String s -> let (tok3, expr3) = parse_PrimaryExpr tok2 in (tok3, FunctionCall(expr2, expr3))
  |Some Tok_ID d -> let (tok3, expr3) = parse_PrimaryExpr tok2 in (tok3, FunctionCall(expr2, expr3))
  |Some Tok_LParen -> let (tok3, expr3) = parse_PrimaryExpr tok2 in (tok3, FunctionCall(expr2, expr3))
  |_ -> (tok2, expr2)
and parse_PrimaryExpr toks =
  match lookahead toks with
  |Some Tok_Int i -> let tok2 = match_token toks (Tok_Int i) in (tok2, Value(Int i))
  |Some Tok_Bool b -> let tok2 = match_token toks (Tok_Bool b) in (tok2, Value(Bool b))
  |Some Tok_String s -> let tok2 = match_token toks (Tok_String s) in (tok2, Value(String s))
  |Some Tok_ID d -> let tok2 = match_token toks (Tok_ID d) in (tok2, ID d)
  |Some Tok_LParen -> let tok2 = match_token toks Tok_LParen in let (tok3, expr3) = parse_Expr tok2 in
    let tok4 = match_token tok3 Tok_RParen in (tok4, expr3)
  |_ -> raise (InvalidInputException ":(")
;;


    

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let (rem_toks, expr) = parse_Mutop toks in
  if rem_toks <> [] then raise (InvalidInputException ":(")
  else (rem_toks, expr)
and parse_Mutop toks =
  match lookahead toks with
  |Some Tok_Def -> parse_DefMutop toks
  |Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  |Some Tok_Let -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_Fun -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_If -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_Not -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_Int i -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_Bool b -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_String s -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_ID d -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |Some Tok_LParen -> let (tok2, expr2) = parse_Expr toks in (match_token tok2 Tok_DoubleSemi, Expr(expr2))
  |_ -> raise (InvalidInputException ":(")
and parse_DefMutop toks =
  let tok2 = match_token toks Tok_Def in
  match lookahead tok2 with
  |Some Tok_ID d -> let tok3 = match_token tok2 (Tok_ID d) in let (tok4, expr4) = parse_Expr (match_token tok3 Tok_Equal) in
    (match_token tok4 Tok_DoubleSemi, Def(d, expr4))
  |_ -> raise (InvalidInputException ":(")
;;
