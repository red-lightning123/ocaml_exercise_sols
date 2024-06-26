type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

let paths g f t =
        let children node =
                List.filter_map (fun (a, b) -> if a = node then Some b else if b = node then Some a else None) g.edges
        in
        let contains a =
                List.exists (fun x -> x = a)
        in
        let rec aux path acc node =
                let acyclic_children = List.filter (fun child -> not(contains child path)) (children node) in
                let new_path = node::path in
                if node = t then
                        (List.rev new_path)::acc
                else
                        List.fold_left (aux new_path) acc acyclic_children
        in aux [] [] f;;

let example_graph =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;

paths example_graph 'f' 'b';;
