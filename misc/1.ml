let queens_positions n =
        let y_range = List.init n (fun y -> y + 1) in
        let ver (_, y) (_, y2) =
                y = y2 in
        let diag1 (x, y) (x2, y2) =
                y - x = y2 - x2 in
        let diag2 (x, y) (x2, y2) =
                y + x = y2 + x2 in
        let test p pos =
                List.for_all (fun pos2 -> not (diag1 pos pos2 || diag2 pos pos2 || ver pos pos2)) p in
        let rec aux p x acc =
                if x = n + 1 then p::acc
                else
                        (* the validity of a queen position isn't affected by a horizontal flip, thus no need to reverse p*)
                        List.fold_left (fun acc y -> if test p (x, y) then aux ((x, y)::p) (x + 1) acc else acc) acc y_range
        in List.map (List.map snd) (aux [] 1 []);;

queens_positions 4;;
