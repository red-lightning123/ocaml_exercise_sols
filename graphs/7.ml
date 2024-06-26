type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

let degree graph node =
        List.fold_left (fun acc (a, b) -> if a = node || b = node then acc + 1 else acc) 0 graph.edges;;

let nodes_sorted_by_degree_dec graph =
        let nodes_degrees = List.map (fun node -> node, degree graph node) graph.nodes in
        List.map (fun (a, da) -> a) (List.sort (fun (a, da) (b, db) -> compare db da) nodes_degrees);;


let color graph =
        let nodes = nodes_sorted_by_degree_dec graph in
        let edges = graph.edges in
        let adjacent a b =
                List.exists (fun (ea, eb) -> ((ea = a) && (eb = b)) || ((ea = b) && (eb = a))) edges
        in
        let next_step color acc l =
                let maybe_color_v (group, new_l, acc) v =
                        if List.exists (fun node -> adjacent node v) group then
                                group, (v::new_l), acc
                        else
                                (v::group), new_l, ((v, color)::acc)
                in
                let group, l, acc = List.fold_left maybe_color_v ([], [], acc) l in
                acc, (List.rev l)
        in
        let rec wp color acc l =
                if l = [] then
                        acc
                else
                        let acc, l = next_step color acc l in
                        wp (color + 1) acc l
        in
        wp 0 [] nodes;;

let g = {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
         edges = [(1, 5); (1, 6); (1, 7); (2, 5); (2, 6); (2, 8); (3, 5);
                  (3, 7); (3, 8); (4, 6); (4, 7); (4, 8)]};;
degree g 1;;
nodes_sorted_by_degree_dec g;;
color g;;

(* from https://iq.opengenus.org/welsh-powell-algorithm/ *)
let eg = {nodes = [1; 2; 3; 4; 5; 6; 7];
        edges = [(1, 3); (2, 3); (2, 5); (3, 4); (3, 5); (3, 6); (5, 6); (5, 7); (6, 7)]};;

nodes_sorted_by_degree_dec eg;;
color eg;;
