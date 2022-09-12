open String
open Str
open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
    let re_bool = Str.regexp "true\\|false"
    and re_white = Str.regexp " +"
    and re_pos_int = Str.regexp "[0-9]+"
    and re_neg_int = Str.regexp "(-[0-9]+)"
    and re_l_paren = Str.regexp "("
    and re_r_paren = Str.regexp ")"
    and re_equals = Str.regexp "="
    and re_not_equals = Str.regexp "<>"
    and re_geq = Str.regexp ">="
    and re_leq = Str.regexp "<="
    and re_greater = Str.regexp ">"
    and re_less = Str.regexp "<"
    and re_or = Str.regexp "||"
    and re_and = Str.regexp "&&"
    and re_not = Str.regexp "not"
    and re_if = Str.regexp "if"
    and re_then = Str.regexp "then"
    and re_else = Str.regexp "else"
    and re_add = Str.regexp "\\+"
    and re_sub = Str.regexp "-"
    and re_mult = Str.regexp "\\*"
    and re_div = Str.regexp "/"
    and re_concat = Str.regexp "\\^"
    and re_let = Str.regexp "let"
    and re_def = Str.regexp "def"
    and re_in = Str.regexp "in"
    and re_rec = Str.regexp "rec"
    and re_fun = Str.regexp "fun"
    and re_arrow = Str.regexp "->"
    and re_double_semi = Str.regexp ";;"
    and re_string = Str.regexp "\"[^\\\"]*\""
    and re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
    in 
    let rec tok pos s =
        if pos >= String.length s then
            []
        else
            if (Str.string_match re_white s pos) then
                let token = Str.matched_string s in 
                    (tok (pos + String.length token) s)
            else if (Str.string_match re_bool s pos) then
                let token = Str.matched_string s in 
                    (Tok_Bool (bool_of_string token))::(tok (pos + String.length token) s)
            else if (Str.string_match re_pos_int s pos) then
                let token = Str.matched_string s in 
                    (Tok_Int (int_of_string token))::(tok (pos + String.length token) s)
            else if (Str.string_match re_neg_int s pos) then
                let token = Str.matched_string s in 
                    (Tok_Int (int_of_string ((String.sub token 1 ((String.length token) - 2)))))::(tok (pos + String.length token) s)
            else if (Str.string_match re_arrow s pos) then
                let token = Str.matched_string s in 
                    (Tok_Arrow)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_l_paren s pos) then
                let token = Str.matched_string s in 
                    (Tok_LParen)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_r_paren s pos) then
                let token = Str.matched_string s in 
                    (Tok_RParen)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_equals s pos) then
                let token = Str.matched_string s in 
                    (Tok_Equal)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_not_equals s pos) then
                let token = Str.matched_string s in 
                    (Tok_NotEqual)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_geq s pos) then
                let token = Str.matched_string s in 
                    (Tok_GreaterEqual)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_leq s pos) then
                let token = Str.matched_string s in 
                    (Tok_LessEqual)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_greater s pos) then
                let token = Str.matched_string s in 
                    (Tok_Greater)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_less s pos) then
                let token = Str.matched_string s in 
                    (Tok_Less)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_or s pos) then
                let token = Str.matched_string s in 
                    (Tok_Or)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_and s pos) then
                let token = Str.matched_string s in 
                    (Tok_And)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_not s pos) then
                let token = Str.matched_string s in 
                    (Tok_Not)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_if s pos) then
                let token = Str.matched_string s in 
                    (Tok_If)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_then s pos) then
                let token = Str.matched_string s in 
                    (Tok_Then)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_else s pos) then
                let token = Str.matched_string s in 
                    (Tok_Else)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_add s pos) then
                let token = Str.matched_string s in 
                    (Tok_Add)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_sub s pos) then
                let token = Str.matched_string s in 
                    (Tok_Sub)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_mult s pos) then
                let token = Str.matched_string s in 
                    (Tok_Mult)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_div s pos) then
                let token = Str.matched_string s in 
                    (Tok_Div)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_concat s pos) then
                let token = Str.matched_string s in 
                    (Tok_Concat)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_let s pos) then
                let token = Str.matched_string s in 
                    (Tok_Let)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_def s pos) then
                let token = Str.matched_string s in 
                    (Tok_Def)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_in s pos) then
                let token = Str.matched_string s in 
                    (Tok_In)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_rec s pos) then
                let token = Str.matched_string s in 
                    (Tok_Rec)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_fun s pos) then
                let token = Str.matched_string s in 
                    (Tok_Fun)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_double_semi s pos) then
                let token = Str.matched_string s in 
                    (Tok_DoubleSemi)::(tok (pos + (String.length token)) s)
            else if (Str.string_match re_string s pos) then
                let token = Str.matched_string s in 
                    (Tok_String (String.sub token 1 ((String.length token) - 2)))::(tok (pos + String.length token) s)
            else if (Str.string_match re_id s pos) then
                let token = Str.matched_string s in 
                    (Tok_ID token)::(tok (pos + String.length token) s)
            else
                raise (InvalidInputException s)
    in tok 0 input
;;
            
