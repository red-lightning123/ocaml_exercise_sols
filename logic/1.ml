type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;


let substitute a_name b_name a b expr =
        let rec aux = function
                | Var name -> if name = "a" then a else b
                | Not x -> not (aux x)
                | And (x, y) -> (aux x) && (aux y)
                | Or (x, y) -> (aux x) || (aux y) in
        aux expr;;

let table2 a_name b_name expr =
        let l = [(false, false); (false, true); (true, false); (true, true)] in
        List.map (fun (a, b) -> (a, b, substitute a_name b_name a b expr)) l;;


And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;
