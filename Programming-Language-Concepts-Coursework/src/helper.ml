exception EmptyList
(*HELPER FUNCTIONS - Deal with the I/O from/to the user*)

(* Function to get the length of a list *)
let get_list_length lst = List.length lst
;;

(* Helper function to convert between lists sets and strings *)
(* Function to convert from a string to a list *)
let get_list_from_string str =
  let rec convert count list =
    if count < 0 
    then list 
    else convert (count - 1) (str.[count]::list) in
  convert (Bytes.length str - 1) []
;;


(* Function to convert from a list to a string *)
let get_string_from_list lst =
  let new_str = Bytes.create (get_list_length lst) in
  let rec convert count = 
  function
  | [] -> new_str
  | h::lst -> Bytes.set new_str count h;
    convert (count + 1) lst in
  convert 0 lst
;;

let clean_list lst =
    let rec filter_list_head lst =
    match lst with 
        | [] -> []
        | ' '::t -> []
        | '}'::t -> []
        | '{'::t -> []
        | '.'::t -> []
        | ':'::t -> []
        | ';'::t -> []
        | ','::t -> []
        | h::t -> h::(filter_list_head t) in
    get_string_from_list (filter_list_head lst)
;;

(* Helper function to remove unwated symbols from a list *)
let rec filter_list list1 list2 =
    let rec get_sub_of_lst n lst =
    match (n, lst) with
        | (_, []) -> []
        | (0, _) -> lst
        | (_, h::t) -> get_sub_of_lst (n - 1) t
    in match list2 with
        | [] -> list1
        | '}'::t -> list1
        | '('::t -> list1
        | '{'::t -> filter_list list1 t
        | ')'::t -> filter_list list1 t
        | ' '::t -> filter_list list1 t
        | ','::t -> filter_list list1 t
        | h::t -> let w = clean_list list2 in 
                    filter_list (w :: list1) (get_sub_of_lst (Bytes.length w) t)
;;


let list_to_set list =
    let rec aux list =
        match list with
        | [] -> "}"
        | [x] -> x ^ "}"
        | h :: t ->  h ^ ", " ^ (aux t)
    in "{" ^ (aux list)
;;




(* Function to read input *)
let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
        try 
         Some (input_line channel) 
        with End_of_file -> None)
;;

let rec last_element = function
    | [x] -> x
    | _::t -> last_element t
    | [] -> raise EmptyList
;;

(* Retrieve the value of the string *)
let get_arg str =
    let rec remove_identifier list =
        match list with
        | [] -> []
        | h::t -> t
 in get_string_from_list (remove_identifier (get_list_from_string str)) 
;;

let lines = line_stream_of_channel stdin;;

let get_line line_number = last_element (Stream.npeek (int_of_string (get_arg line_number)) lines)
;;

let get_last_line last_line =  last_element (Stream.npeek last_line lines)
;;