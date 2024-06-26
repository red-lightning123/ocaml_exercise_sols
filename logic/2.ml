type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let find_val name l =
        let rec aux = function
                | [] -> raise Not_found
                | (var_name, var_val) :: t -> if var_name = name then var_val else aux t in
        aux l;;

let substitute comb expr =
        let rec aux = function
                | Var name -> find_val name comb
                | Not x -> not (aux x)
                | And (x, y) -> (aux x) && (aux y)
                | Or (x, y) -> (aux x) || (aux y) in
        aux expr;;

let prepend_combs var b l =
        List.map (fun x -> (var, b)::x) l;;

let rec make_combs = function
        | [] -> [[]]
        | h :: t ->
                let next = make_combs t in
                (prepend_combs h false next) @ (prepend_combs h true next);;

let table vars expr =
        let combs = make_combs vars in
        List.map (fun comb -> comb, substitute comb expr) combs;;

table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;
