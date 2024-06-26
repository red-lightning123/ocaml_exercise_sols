(* this is a brute force solution. it is impractical even for relatively small values of n *)
let knight_tour n =
        let gen_jump (x, y) flipx flipy conj =
                let xd = 1 in
                let yd = 2 in
                let xd = if flipx then -xd else xd in
                let yd = if flipy then -yd else yd in
                if conj then x+yd, y+xd else x+xd, y+yd in
        let jump_unbounded pos =
                let b_range = [false; true] in
                List.fold_left (fun acc flipx -> List.fold_left (fun acc flipy -> List.fold_left (fun acc conj -> (gen_jump pos flipx flipy conj)::acc) acc b_range) acc b_range) [] b_range in
        let jump pos =
                List.filter (fun (x, y) -> 1 <= x && x <= n && 1 <= y && y <= n) (jump_unbounded pos) in
        let rec aux path pos =
                if List.length path = n * n then
                        Some path
                else
                        let fold_step acc next_pos =
                                if Option.is_some acc then acc
                                else if List.mem next_pos path then None else aux (next_pos::path) next_pos in
                        List.fold_left fold_step None (jump pos) in
        (* reversal is technically not necessary as a reversed tour is also a tour*)
        List.rev (Option.get (aux [(1, 1)] (1, 1)));;

knight_tour 6;;
