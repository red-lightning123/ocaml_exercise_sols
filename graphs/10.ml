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

type color =
        | Red
        | Blue;;

let bipartite g =
        let other_color c =
                match c with
                | Red -> Blue
                | Blue -> Red
        in
        let edges = strip_node_names g in
        let nodes = List.init (List.length g.nodes) (fun x -> x) in
        let children node =
                List.filter_map (fun (a, b) -> if a = node then Some b else if b = node then Some a else None) edges
        in
        let aux =
                let colors = Array.make (List.length nodes) None in
                let rec dfs node color =
                        colors.(node) <- Some color;
                        let aux child =
                                match colors.(child) with
                                | None -> dfs child (other_color color)
                                | Some child_color ->
                                        child_color <> color
                        in
                        List.for_all aux (children node)
                in
                let comp_bipartite node =
                        if Option.is_none colors.(node) then
                                dfs node Red
                        else
                                true
                in
                List.for_all comp_bipartite nodes (* due to side effects, this assumes that List.for_all executes sequentially, although the algorithm's results shouldn't be affected by the order *)
        in aux;;

let g = {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
         edges = [(1, 5); (2, 5); (2, 6); (2, 8); (2, 1); (3, 5);
                  (3, 7); (3, 8); (4, 6); (4, 7); (4, 8)]};;

bipartite g;;
