type expr =  
    | ProgBegin of expr * expr
    | Sequence of expr * expr
    | Set of string
    | String of string
    | Var of string
    | Integer of int
    | Boolean of bool
    | AddOperation of expr * expr
    | MultiplyOperation of expr * expr
    | DivideOperation of expr * expr
    | ModulusOperation of expr * expr
    | MinusOperation of expr * expr
    | LessThan of expr * expr
    | GreaterThan of expr * expr
    | EqualTo of expr * expr
    | Conditional of expr * expr * expr
    | Execute_1_Param of expr * expr
    | Apply_Function_One_Param of string * string * expr * expr
    | VarDeclaration of string * expr * expr
    | Execute_2_Param of expr * expr * expr
    | Apply_Function_Two_Param of string * string * string * expr * expr
    | SetHead of expr
    | SetTail of expr
    | SetAppend of expr * expr
    | SetPostfix of expr * expr
    | SetPrefix of expr * expr
    | SetUnion of expr * expr
    | SetIntersection of expr * expr
    | SetConcat of expr * expr
    | SetSize of expr
    | Execute_3_Param of expr * expr * expr * expr
    | Apply_Function_Three_Param of string * string * string * string * expr * expr
    | SetSort of expr
    | ElimRepeats of expr
    | Check of expr * expr
    | SetKleene of expr * expr
    | SetLength of expr


val eval : expr -> expr

val print : expr -> unit
