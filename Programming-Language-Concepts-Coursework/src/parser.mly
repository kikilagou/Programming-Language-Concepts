%{
    open Main
%}
%token <int> INT
%token <bool> BOOL
%token <string> SET
%token <string> STRING
%token <string> VAR_ID
%token <string> EPSILON
%token COMMENT
%token BEGIN END
%token ADD MINUS LESSTHAN GREATERTHAN EQUALTO MODULUS DIVIDE MULTIPLY
%token IF THEN ELSE
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE
%token HEAD TAIL UNION APPEND CONCAT INTERSECTION POSTFIX PREFIX
%token SORT UNIQ CHECK SIZE
%token FUNCTION IN EVAL_TO VARIABLE EQUAL
%token LENGTH KLEENE
%token EOF
%start main
%type <Main.expr> main
%%

main:
    | expr EOF { $1 }
;

expr:
    | primitive { $1 }
    | prog_struct { $1 }
    | arithmetic { $1 }
    | boolean_expr  { $1 }
    | conditional { $1 }
    | funct { $1 }
    | apply_func { $1 }
    | LPAREN VARIABLE VAR_ID EQUAL expr IN expr RPAREN { VarDeclaration ($3, $5, $7) }
    | set_expr { $1 }
    | LPAREN UNIQ expr RPAREN                    { ElimRepeats $3 }
    | LPAREN CHECK expr expr RPAREN                { Check ($3, $4) }
;

primitive:
    | INT                                        { Integer $1 }
    | SET                                       { Set $1 }
    | STRING                                     { String $1 }
    | VAR_ID                                      { Var $1 }
    | EPSILON                                  { String $1 }
    | BOOL                                   { Boolean $1 }
;

arithmetic:
    | LPAREN expr ADD expr RPAREN                { AddOperation ($2, $4) }
    | LPAREN expr MINUS expr RPAREN              { MinusOperation ($2, $4) }
    | LPAREN expr MULTIPLY expr RPAREN           { MultiplyOperation ($2, $4) }
    | LPAREN expr DIVIDE expr RPAREN             { DivideOperation ($2, $4) }
    | LPAREN expr MODULUS expr RPAREN            { ModulusOperation ($2, $4) }
;

boolean_expr:
    | LPAREN expr LESSTHAN expr RPAREN           { LessThan ($2, $4) }
    | LPAREN expr GREATERTHAN expr RPAREN        { GreaterThan ($2, $4) }
    | LPAREN expr EQUALTO expr RPAREN            { EqualTo ($2, $4) }
;

conditional:
    | LPAREN IF expr THEN expr ELSE expr RPAREN  { Conditional ($3, $5, $7) }
;

apply_func:
	| LPAREN expr expr RPAREN                    { Execute_1_Param ($2, $3) }
    | LPAREN expr expr expr RPAREN               { Execute_2_Param ($2, $3, $4) }
    | LPAREN expr expr expr expr RPAREN          { Execute_3_Param ($2, $3, $4, $5) }
 ;;

set_expr:
    | LPAREN HEAD expr RPAREN                    { SetHead $3 }
    | LPAREN TAIL expr RPAREN                    { SetTail $3 }
    | LPAREN UNION  expr expr RPAREN             { SetUnion  ($3, $4) }
    | LPAREN INTERSECTION expr expr RPAREN       { SetIntersection  ($3, $4) }
    | LPAREN APPEND expr expr RPAREN             { SetAppend ($3, $4) }
    | LPAREN POSTFIX expr expr RPAREN            { SetPostfix ($3, $4) }
    | LPAREN PREFIX expr expr RPAREN             { SetPrefix ($3, $4) }
    | LPAREN CONCAT expr expr RPAREN             { SetConcat ($3, $4) }
    | LPAREN LENGTH expr RPAREN                  { SetLength $3 }
    | LPAREN KLEENE expr expr RPAREN         	 { SetKleene ($3, $4) }
    | LPAREN SIZE expr RPAREN                    { SetSize $3 }
    | LPAREN SORT expr RPAREN                    { SetSort $3 }
;

funct:
    | FUNCTION VAR_ID VAR_ID EVAL_TO expr IN expr  {Apply_Function_One_Param ($2, $3, $5, $7) }
    | FUNCTION VAR_ID VAR_ID VAR_ID EVAL_TO expr IN expr  {Apply_Function_Two_Param ($2, $3, $4, $6, $8) }
    | FUNCTION VAR_ID VAR_ID VAR_ID VAR_ID EVAL_TO expr IN expr  {Apply_Function_Three_Param ($2, $3, $4, $5, $7, $9) }
;

prog_struct:
    | COMMENT expr                               { $2 }
    | BEGIN expr END expr                        { ProgBegin ($2, $4) }
    | BEGIN expr END                             { $2 }
;