type expr =
  | Int of int
  | Symbol of string
  | List of expr list

let rec string_of_expr = function
  | Int n -> string_of_int n
  | Symbol s -> s
  | List exprs ->
      "(" ^ String.concat " " (List.map string_of_expr exprs) ^ ")"
