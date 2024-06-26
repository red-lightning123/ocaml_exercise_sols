type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

let strip_node_names g =
        let find_index c l =
                let rec aux i = function
                        | [] -> raise Not_found
                        | h :: t -> if h = c then i else aux (i + 1) t
                in aux 0 l
        in
        List.map (fun (a, b) -> (find_index a g.nodes, find_index b g.nodes)) g.edges;;

let enrich_node_names edges nodes =
        List.map (fun (a, b) -> (List.nth nodes a, List.nth nodes b)) edges;;

let s_tree g =
        let g_nodes = List.init (List.length g.nodes) (fun x -> x)
        in
        let children_in edges node =
                List.filter_map (fun (a, b) -> if a = node then Some b else if b = node then Some a else None) edges
        in
        let contains a =
                List.exists (fun x -> x = a)
        in
        (* this is incomprehensibly inefficient but at least it doesn't use an array :) *)
        let subset_has_cycle edges =
                (* cycle within same connected component as f *)
                let has_cycle_from f =
                        let rec aux path node =
                                let children = children_in edges node in
                                let children = match path with
                                        | [] -> children
                                        | h :: _ -> List.filter (fun n -> h <> n) children
                                in
                                if contains node path then
                                        true
                                else
                                        List.exists (aux (node::path)) children
                        in aux [] f
                in
                List.exists has_cycle_from g_nodes
        in
        let edges_contain_node edges node =
                List.exists (fun (a, b) -> a = node || b = node) edges
        in
        let subset_is_spanning edges =
                List.for_all (fun node -> edges_contain_node edges node) g_nodes
        in
        let rec aux left_edges selected_edges acc =
                if subset_has_cycle selected_edges then
                        acc
                else if subset_is_spanning selected_edges then
                        selected_edges::acc
                else
                        match left_edges with
                        | [] -> acc
                        | h :: t -> aux t (h::selected_edges) (aux t selected_edges acc)
        in
        let edges = strip_node_names g
        in List.map (fun st -> enrich_node_names st g.nodes) (aux edges [] []);;

let is_connected g = match s_tree g with
        | [] -> true
        | _ -> false;;

let is_tree g = match s_tree g with
        | [t] -> true
        | _ -> false;;

let g = {nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
         edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
                  ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
                  ('e', 'h'); ('f', 'g'); ('g', 'h')]};;

s_tree g;;
is_connected g;;
is_tree g;;
