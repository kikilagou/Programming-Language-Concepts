{
open Parser
open Helper
}

let charRegExp = ['a'-'z''A'-'Z''_''0'-'9' ':']
let setRegExp = '{'(' '*(charRegExp+)' '*','' '*)*(charRegExp+)?' '*'}'
let intRegExp = ['0'-'9']
let stringRegExp = ['"'](charRegExp*[' ']*)*['"']
let var_idRegExp = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm
    

rule lexer_main = parse

    (* Ignores white space *)
    | [' ' '\t' '\n']           { lexer_main lexbuf }

    (* Comments are denoted by $$ *)
    | "$$"[^'\n']*              { COMMENT }

    (* Denotes the start and end of a program *)
    | "begin_speech"            { BEGIN }
    | "final_words"             { END }

    (* Arithmetic Operations *)
    | '+'                       { ADD }
    | '-'                       { MINUS }
    | '*'                       { MULTIPLY }
    | '/'                       { DIVIDE }    
    | '%'                       { MODULUS }
    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    | "as"                      { EQUAL }
   
    (* Boolean Operations *)
    | '<'                       { LESSTHAN }
    | '>'                       { GREATERTHAN }
    | "is_politically_correct"  { EQUALTO }

     (* Boolean Keywords *)
    | "honesty" as lxm          { BOOL(bool_of_string lxm) }
    | "liar" as lxm             { BOOL(bool_of_string lxm) }    
    
    (* Conditioinal statement *) 
    | "if_you_vote_for_me"      { IF }
    | "i_promise_to"            { THEN }
    | "else"                    { ELSE }

    (* Function keywords *)
    | "held_in"                 { IN }
    | "state"                   { FUNCTION }
    | "->"                      { EVAL_TO }
    | "elector"                 { VARIABLE }
    
    (* Set Operations *)
    | '{'                       { LBRACE }
    | '}'                       { RBRACE }
    | "head_of_state"           { HEAD }
    | "working_class"           { TAIL }
    | ":"                       { EPSILON("") }
    | "trade_union"             { UNION }
    | "intersection"            { INTERSECTION }
    | "tax_inflation"           { APPEND }
    | "postfix"                 { POSTFIX }
    | "wall_length"             { LENGTH }
    | "prefix"                  { PREFIX }
    | "hand_size"               { SIZE }
    | "economic_growth"         { KLEENE }
    | "work_together"           { CONCAT }
    | "sort_out_issues"         { SORT }
    | "uniq"                    { UNIQ }
    | "conference"              { CHECK }
    | eof                       { EOF }

   
    (* Defining tokens for the primitives in the language *)
    | setRegExp as lxm          { SET(lxm) }    
    | stringRegExp as lxm       { STRING(lxm) }
    | intRegExp+ as lxm         { INT(int_of_string lxm) }
    | var_idRegExp              { VAR_ID(lxm) }
    | ">k"                      { INT(int_of_string (get_last_line 10)) }
    | ">"intRegExp+ as lxm      { SET(get_line lxm) }
    
   
