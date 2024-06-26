(* modified version of graph problem 6 *)
module Edge = struct
        type t = int * int
        let compare = compare
end
module EdgeSet = Set.Make (Edge);;
let isomorphic nodes g1_edges g2_edges_preprocessed =
        if List.length g1_edges = EdgeSet.cardinal g2_edges_preprocessed then
                let normalize_edge (a, b) =
                        if a < b then a, b else b, a in
                let check_iso f v =
                        let f = List.rev f in
                        let edges = List.filter (fun (a, b) -> a = v || b = v) g1_edges in
                        let edges = List.filter_map (fun (a, b) -> match List.nth_opt f a, List.nth_opt f b with | Some fa, Some fb -> Some (fa, fb) | _ -> None) edges in
                        let edges = List.map normalize_edge edges in
                        List.for_all (fun edge -> EdgeSet.mem edge g2_edges_preprocessed) edges
                in
                (* assumes the node iterator is of the form 0..len *)
                let rec aux f = function
                        | [] -> List.for_all (check_iso f) nodes
                        | (h :: t) as left ->
                                        let succ, _, _ =
                                                List.fold_left (
                                                        fun (ans, prev_rev, next) curr ->
                                                                let ans =
                                                                        if ans then true
                                                                        else if check_iso (curr::f) h then aux (curr::f) (List.rev_append prev_rev (List.tl next))
                                                                        else false
                                                                in (ans, curr::prev_rev, List.tl next)) (false, [], left) left in succ
                in aux [] nodes
        else false;;



(* another modified version of graph problem 6 *)
let isomorphic2 nodes g1_edges g2_edges_preprocessed =
        let normalize_edge (a, b) =
                if a < b then a, b else b, a in
        let check_iso f =
                let g1_edges = List.sort compare (List.map (fun (a, b) -> normalize_edge (List.nth f a, List.nth f b)) g1_edges) in
                g1_edges = g2_edges_preprocessed
        in
        let rec aux f = function
                | [] -> check_iso f
                | (_ :: t) as left -> let succ, _, _ = List.fold_left (fun (ans, prev_rev, next) curr -> let ans = if ans then true else aux (curr::f) (List.rev_append prev_rev (List.tl next)) in (ans, curr::prev_rev, List.tl next)) (false, [], left) left in succ
        in aux [] nodes;;

let gen_k_regular k n =
        let nodes = List.init n (fun x -> x) in
        let check edge_cnt empty filled (a, b) mark =
                let deg v filled = List.fold_left (fun acc ((a, b), mark) -> if mark && (v = a || v = b) then acc + 1 else acc) 0 filled in
                let mark_int = if mark then 1 else 0 in
                let dega = mark_int + deg a filled in
                let degb = mark_int + deg b filled in
                (2 * (mark_int + edge_cnt + (List.length empty)) >= n * k) && dega <= k && degb <= k in
        let add edge_cnt empty filled pos mark =
                if check edge_cnt empty filled pos mark then
                        let filled = (pos, mark) :: filled in
                        let mark_int = if mark then 1 else 0 in
                        Some (edge_cnt + mark_int, empty, filled)
                else
                        None
        in
        let empty = List.concat (List.init n (fun i -> List.init i (fun j -> i, j))) in
        let filled = [] in
        let mark_range = [false; true] in
        let rec aux edge_cnt acc empty filled =
                match empty with
                | [] ->
                        let filled = List.filter_map (fun (pos, mark) -> if mark then Some pos else None) filled in
                        if List.exists (isomorphic nodes filled) acc then
                                acc
                        else
                                let normalize_edge (a, b) =
                                        if a < b then a, b else b, a in
                                (* preprocess for subsequent isomorphism checks *)
                                let filled_preprocessed = List.map normalize_edge filled in
                                let filled_preprocessed = EdgeSet.of_list filled_preprocessed in
                                filled_preprocessed::acc
                | pos :: t ->
                        let fold_step acc mark =
                                match add edge_cnt t filled pos mark with
                                | None -> acc
                                | Some (edge_cnt, empty, filled) ->
                                        aux edge_cnt acc empty filled in
                        List.fold_left fold_step acc mark_range
        in let filled = aux 0 [] empty filled
        in List.map EdgeSet.elements filled;;

(* this is meant to be run as native compiled code, not from the toplevel, since the extra speed is necessary here *)
let print_solutions sols =
        List.iter (
        fun l ->
                List.iter (fun (a, b) -> Printf.printf " (%i, %i)" a b) l;
                Printf.printf "\n"
        ) sols;
        Printf.printf "\n";;


print_solutions (gen_k_regular 3 6);;
print_solutions (gen_k_regular 2 9);;
