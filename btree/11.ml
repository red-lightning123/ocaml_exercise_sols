type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec nth l n = 
        match l with 
        | [] -> None 
        | h :: t -> if n = 0 then Some(h) else nth t (n-1);; 

let complete_binary_tree l =
        let rec aux i =
                match nth l (i - 1) with
                        | None -> Empty
                        | Some(v) -> Node (v, aux (2 * i), aux (2 * i + 1))
        in
        aux 1;;


let nil_heights tree =
        let rec aux level tree acc = match tree with
                | Empty -> level::acc
                | Node(_, left, right) -> aux (level+1) left (aux (level+1) right acc) in
        aux 1 tree [];;

let read_eq = function
        | [] -> raise Not_found
        | h :: t ->
                let rec aux = function
                        | [] -> []
                        | h2 :: t2 -> if h = h2 then aux t2 else t2 in
                h, aux t;;

let is_complete_binary_tree tree =
        let hs = nil_heights tree in
        let first, hs = read_eq hs in
        if hs = [] then true
        else
                let second, hs = read_eq hs in
                if hs = [] then first = second + 1
                else false;;

complete_binary_tree [1; 2; 3; 4; 5; 6];;

is_complete_binary_tree (complete_binary_tree [1; 2; 3; 4; 5; 6]);;
