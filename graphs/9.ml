type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

(* union find helper functions *)

let uf_create len = Array.init len (fun x -> x);;

let uf_find i uf =
        let rec aux i =
                let parent = uf.(i) in
                if parent = i then
                        i
                else
                        let root = aux parent in
                        uf.(i) <- root;
                        root
        in aux i;;

let uf_union a b uf =
        uf.(uf_find a uf) <- uf_find b uf;;


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

(* groups nodes by connected components *)
let split_connected g =
        let uf = uf_create (List.length g.nodes) in
        let nodes = List.init (List.length g.nodes) (fun x -> x) in
        let edges = strip_node_names g in
        let comps =
                List.iter (fun (a, b) -> uf_union a b uf) edges;
                let nodes = List.map (fun n -> n, uf_find n uf) nodes in
                let comps = Array.make (List.length nodes) [] in
                List.iter (fun (n, un) -> comps.(un) <- (n::(comps.(un)))) nodes;
                let comps = Array.to_list comps in
                let comps = List.filter (fun c -> c <> []) comps in
                comps
        in List.map (fun l -> List.map (fun n -> List.nth g.nodes n) l) comps;;

let g = {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
         edges = [(*(1, 5); (1, 6); (1, 7); (2, 5); (2, 6); (2, 8);*) (2, 1); (3, 5);
                  (3, 7); (3, 8); (4, 6); (4, 7); (4, 8)]};;

split_connected g;;
