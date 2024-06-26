type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

let cycles g f =
        let children node =
                List.filter_map (fun (a, b) -> if a = node then Some b else if b = node then Some a else None) g.edges
        in
        let contains a =
                List.exists (fun x -> x = a)
        in
        let rec aux path acc node =
                let children = children node in
                let acyclic_children = List.filter (fun child -> not(contains child path)) children in
                let acc = List.fold_left (aux (node::path)) acc acyclic_children in
                if contains f children then
                        (List.rev (f::(node::path)))::acc
                else
                        acc
        in aux [] [] f;;

let example_graph =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;

cycles example_graph 'f';;
