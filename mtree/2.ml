type 'a mult_tree = T of 'a * 'a mult_tree list;;

let rec count_nodes (T (_, children)) = 1 + List.fold_left (fun acc child -> acc + count_nodes child) 0 children;;

count_nodes (T ('a', [T ('f', []) ]));;
