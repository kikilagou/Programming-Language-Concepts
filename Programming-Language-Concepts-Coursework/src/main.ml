open Printf
open Helper

(*Exceptions which are thrown given that an I/O error occurs*)
exception LookupError
exception EmptyList
exception TypeErrorException
exception UnrecognisedTypeException

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

 let lookup_error env elem = 
  let s = "Variable is not found. Perhaps variable hasn't been declared?"
  in match env with
  | [] -> failwith (s^elem)
  | (_, _) :: [] -> failwith (s^elem)
  | (_, _) :: (_, _) :: [] -> failwith (s^elem)
  | _ -> failwith (s^elem) 

(* Function to sort and compare elements of a list for duplicates *)
let sort_and_compare_list = List.sort compare
;;

let rec remove_repeat_elements set =
    let rec remove_symbol lst rep =
    match lst with
    | [] -> []
    | h::t -> if rep = h 
                then remove_symbol t rep 
                else h::(remove_symbol t rep)
    in match set with
         | [] -> []
         | h::t -> h :: (remove_symbol (remove_repeat_elements t) h)
;;

(* Checks if an element is member of a list *)
let rec check_member elem = function
  | [] -> false
  | h::t -> h = elem || check_member elem t
;;

(* Removes and element from a list *)
let rec remove elem = function
  | [] -> failwith "Elements cannot be removed from the empty list."
  | h::t -> if h = elem 
            then t 
            else h::(remove elem t)
;;

(* Fuction to convert from a set to a list of elements *)
let set_to_list set = List.rev (filter_list [] (get_list_from_string set))
;;


(* Adds to the start of an element in a list, does this recursively for every member of list *)
let rec prefix list elem = 
    match list with
    | [] -> []
    | h::t -> sort_and_compare_list ((elem^h)::prefix t elem)
;;

(* Adds to the end of an element in a list, does this recursively for every member of list *)
let rec postfix list elem = 
    match list with
    | [] -> []
    | h::t -> sort_and_compare_list ((h^elem)::postfix t elem)
;;

(* Generates kleen* of a specified letter up to speicfied max count and adds it all to the list *)
let kleene_star letter max_count =
    let rec add member count =
        (member ^ letter) :: (*Recursively add to list given that we are under to max count*)
        if count = max_count 
        then []
        else add (member ^ letter) (count + 1)
    in ":" :: (add "" 2)
;;

(* Intersection - takes 2 lists and returns their intersection *)
let rec intersection list1 list2 = match list1 with
  | [] -> if list2 = [] 
            then [] 
            else intersection list2 list1
  | h::t ->
      if check_member h list2 then
        let list2' = remove h list2 in
        h::(intersection t list2')
      else
        intersection t list2
;;

(* removing colons *)
let remove_colon_from_list list =
    match list with
        | [] -> []
        | h::t -> if (t = [':'])
                        then h::[]
                        else if (t = [])
                        then h :: t
                        else if h = ':' then t else h::t
;;


let remove_quotes_from_string str  =
    let rec remove_from_list lst = 
        match lst with
        | [] -> []
        | h::t -> if h = '\"' then remove_from_list t else  h::remove_from_list t
    in get_string_from_list (remove_from_list (get_list_from_string str))
;;

(* Union - takes 2 lists and returns their union *)
let union list1 list2 = sort_and_compare_list (List.merge compare list1 list2)
;;

let rec check n list =
    match (n, list) with
        | 0, h::t -> []
        | _, [] -> list
        | _, h::t -> h::check (n - 1) t
;;

let rec env_lookup env var = match env with
        | (var_id, var_value)::t -> (match (var = var_id) with 
                                            true -> var_value
                                            | false -> env_lookup t var)
        | _ -> lookup_error env var
    ;;

    let rec env_lookup_two_args env var  = match env with
       | (var_id, var_value)::(var_id2, var_value2)::t -> (match (var = var_id) with 
                                            true -> (var_value, var_value2)
                                            | false -> env_lookup_two_args t var)
       | _ -> lookup_error env var

      
    ;;

let rec env_lookup_three_args env var  = match env with
   | (var_id, var_value)::(var_id2, var_value2)::(var_id3, var_value3)::t ->  (match (var = var_id) with 
                                            true -> (var_value, var_value2, var_value3)
                                            | false -> env_lookup_three_args t var)
   | _ -> lookup_error env var

;;

let rec evaluator func_env arg_env var_to_find =
   match var_to_find with
        (*var_to_find is declared as a variable*)
        | (Var var) -> env_lookup arg_env var
        (*Primitives*)
        | (Integer num) -> Integer num
        | (Set set) -> Set set
        | (String str) -> String str
        | (Boolean bl) -> Boolean bl
        | (Sequence (h, t)) -> Sequence (h, t)
        (*Numerical Operations*)
        | (AddOperation (var1, var2) ) ->
          (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Integer (var1' + var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (MultiplyOperation (var1, var2) ) ->
          (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Integer (var1' * var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (DivideOperation (var1, var2) ) ->
          (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Integer (var1' / var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (ModulusOperation (var1, var2) ) ->
          (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Integer (var1' mod var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (MinusOperation (var1, var2) ) ->
          (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Integer (var1' - var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        (*Boolean Operations*)
        | (LessThan (var1, var2)) ->
            (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Boolean (var1' < var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (GreaterThan (var1, var2)) ->
            (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Boolean (var1' > var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (EqualTo (var1, var2)) ->
            (match (evaluator func_env arg_env var1) with
            | Integer var1' -> 
              (match (evaluator func_env arg_env var2) with
                | Integer var2' ->
                  (Boolean (var1' = var2'))
                | _ -> raise TypeErrorException)
          | _ -> raise TypeErrorException)
        | (ProgBegin (var1, var2)) ->
            let var1' = (evaluator func_env arg_env var1)
          in let var2' = (evaluator func_env arg_env var2)
        in Sequence (var1', var2')
        | (Conditional (cond, trueExpr, falseExpr)) ->
            (match evaluator func_env arg_env cond with
                  | (Boolean b) ->
                      evaluator func_env arg_env (if b then trueExpr else falseExpr)
                  | _ -> failwith "If-Then-Else statement is ill formed. Please ensure the condition is a boolean expression.")
        | (Apply_Function_One_Param (name, argName, body, inExpr)) ->
            evaluator ((name, (argName, body)) :: func_env) arg_env inExpr
        | (Apply_Function_Two_Param (name, arg0Name, arg1Name, body, inExpr)) ->
            evaluator ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: func_env) arg_env inExpr
        | (Apply_Function_Three_Param (name, arg0Name, arg1Name, arg2Name, body, inExpr)) ->
            evaluator ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: (name, (arg2Name, body)) :: func_env) arg_env inExpr
        | (VarDeclaration (name, value, inExpr)) ->
            evaluator func_env ((name, (evaluator func_env arg_env value)) :: arg_env) inExpr
        | (Execute_1_Param (func, arg)) ->
                (match func with
                     | (Var f) ->
                        (match env_lookup func_env f with
                             | (argName, body) ->
                                evaluator func_env ((argName, (evaluator func_env arg_env arg)) :: arg_env) body)
                             | _ -> failwith "Function cannot be found.")
         | (Execute_2_Param (func, arg0, arg1)) ->
                 (match func with
                      | (Var f) ->
                         (match (env_lookup_two_args func_env f) with
                              | ((argName0, body0), (argName1, body1)) ->
                                 evaluator func_env ((argName0, (evaluator func_env arg_env arg0)) :: (argName1, (evaluator func_env arg_env arg1)) :: arg_env) body0)
                              | _ -> failwith "Function cannot be found.")
          | (Execute_3_Param (func, arg0, arg1, arg2)) ->
                (match func with
                       | (Var f) ->
                          (match (env_lookup_three_args func_env f) with
                               | ((argName0, body0), (argName1, body1), (argName2, body2)) ->
                                  evaluator func_env ((argName0, (evaluator func_env arg_env arg0)) :: (argName1, (evaluator func_env arg_env arg1)) :: (argName2, (evaluator func_env arg_env arg2)) :: arg_env) body0)
                               | _ -> failwith "Function cannot be found.")
        | (SetHead var1) ->
            let var1' = (match evaluator func_env arg_env var1 with
              | Set var1' -> var1'
              | _ -> raise TypeErrorException)
          in let var1'_list = set_to_list var1'
            in let head list =
                (match list with
                | [] -> raise EmptyList
                | h :: t -> "\"" ^ h ^ "\"")
            in String (head var1'_list)
        | (SetTail var1) ->
            let var1' = (match evaluator func_env arg_env var1 with
              | Set var1' -> var1'
              | _ -> raise TypeErrorException)
          in let var1'_list = set_to_list var1'
            in let tail list =
                match list with
                | [] -> raise EmptyList
                | h :: t -> t
            in Set (list_to_set (tail var1'_list))
        (*String Operations*)
        | (SetPostfix (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | String var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | String var2' -> 
                    let list = (get_list_from_string (remove_quotes_from_string var2' ^ remove_quotes_from_string var1'))
                      in (String (get_string_from_list (remove_colon_from_list list)))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetPrefix (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | String var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | String var2' -> 
                      (String (get_string_from_list (remove_colon_from_list (get_list_from_string (remove_quotes_from_string var1' ^ remove_quotes_from_string var2')))))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetAppend (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | Set var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | String var2' -> 
                      (Set (list_to_set ((List.sort compare (remove_repeat_elements (postfix (set_to_list var1') (remove_quotes_from_string var2')))))))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        (*Set Operations*)
        | (SetUnion (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | Set var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | Set var2' -> 
                      (Set (list_to_set (List.sort compare (union (set_to_list var1') (set_to_list var2')))))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetIntersection (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | Set var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | Set var2' -> 
                      (Set (list_to_set (List.sort compare (intersection (set_to_list var1') (set_to_list var2')))))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetConcat (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | String var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | Set var2' -> 
                      (Set (list_to_set (remove_quotes_from_string var1' :: set_to_list var2')))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetSize var) ->
            (match evaluator func_env arg_env var with
              | Set var' -> 
                  Integer (List.length(set_to_list var'))
              | _ -> raise TypeErrorException)
        | (SetSort var) ->
            (match evaluator func_env arg_env var with
              | Set var' -> 
                  Set (list_to_set (List.sort compare (set_to_list var')))
              | _ -> raise TypeErrorException)        
        | (ElimRepeats var) ->
            (match evaluator func_env arg_env var with
              | Set var' -> 
                  Set (list_to_set (remove_repeat_elements (set_to_list var')))
              | _ -> raise TypeErrorException)           
        | (Check (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | Set var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | Integer var2' -> 
                      (Set (list_to_set (check var2' (set_to_list var1'))))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetKleene (var1, var2)) ->
              (match evaluator func_env arg_env var1 with
              | String var1' -> 
                  (match evaluator func_env arg_env var2 with
                    | Integer var2' -> 
                      (Set (list_to_set ( (*get_clean_string*) (List.sort compare (kleene_star (remove_quotes_from_string var1') var2')))))
                    | _ -> raise TypeErrorException)
              | _ -> raise TypeErrorException)
        | (SetLength var) ->
            (match evaluator func_env arg_env var with
              | String var' -> 
                  Integer (List.length (get_list_from_string (remove_quotes_from_string var')))
              | _ -> raise TypeErrorException)      
;;

let eval var_to_find = evaluator [] [] var_to_find ;;

let rec print output = match output with
    | (Set set) -> print_string set
    | (Sequence (var1, var2)) -> print var1; print_string "\n";print var2
    | (String str) -> print_string str
    | (Integer num) -> print_int num
    | (Boolean bl) -> if bl then print_string "true" else print_string "false"
    | _ -> raise TypeErrorException
