(* is the 1d nonogram specified by widths solvable from the initial board (empty, filled) after adding mark at pos? *)
(* assumes the initial board is valid to avoid the need to recheck it *)
(* note that neither empty nor filled contain pos by convention *)
let check_solve_1d empty filled mark pos widths =
        (* + 1 because the current mark isn't included *)
        let size = List.length empty + List.length filled + 1 in
        let mark_range = [false; true] in
        let check mark pos empty filled =
                let gather_widths board =
                        let fold_step (curr, acc) mark =
                                if mark then
                                        curr + 1, acc
                                else
                                        if curr = 0 then
                                                curr, acc
                                        else
                                                0, curr::acc in
                        let curr, acc = List.fold_left fold_step (0, []) board in
                        let acc =
                                if curr = 0 then
                                        acc
                                else
                                        curr::acc in
                        List.rev acc in
                let win_condition board =
                        gather_widths board = widths in
                let concretize empty filled =
                        match empty with
                        | [] -> Some (List.init size (fun pos -> Option.get (List.find_map (fun (posf, markf) -> if pos = posf then Some markf else None) filled)))
                        | _ -> None in
                let filled = (pos, mark)::filled in
                match concretize empty filled with
                | None -> true
                | Some final_board -> win_condition final_board in
        let add mark pos empty filled =
                if check mark pos empty filled then
                        let filled = (pos, mark) :: filled in
                        Some (empty, filled)
                else
                        None
        in
        let rec aux empty filled =
                match empty with
                | [] -> Some filled
                | pos :: t ->
                        let fold_step acc mark =
                                if Option.is_some acc then acc
                                else
                                        match add mark pos t filled with
                                        | None -> None
                                        | Some (empty, filled) ->
                                                aux empty filled in
                        List.fold_left fold_step None mark_range
        in
        match add mark pos empty filled with
        | None -> false
        | Some (empty, filled) -> Option.is_some (aux empty filled);;

(* doesn't need to check for the validity of an initial board since it starts from scratch *)
let solve x_widths y_widths =
        (*
        size_x being equal to the length of y_widths may be counterintuitive
        it makes more sense when one considers that there is a column width specification
        for each row of the board
        *)
        let size_x = List.length y_widths in
        let size_y = List.length x_widths in
        let check empty filled pos mark =
                let row (x, y) = y in
                let col (x, y) = x in
                let same_row p1 p2 =
                        row p1 = row p2 in
                let same_col p1 p2 =
                        col p1 = col p2 in
                let row_check empty filled pos mark =
                        let row_empty = List.filter_map (fun posf -> if same_row pos posf then Some (col posf) else None) empty in
                        let row_filled = List.filter_map (fun (posf, markf) -> if same_row pos posf then Some (col posf, markf) else None) filled in
                        check_solve_1d row_empty row_filled mark (col pos) (List.nth x_widths (row pos)) in
                let col_check empty filled pos mark =
                        let col_empty = List.filter_map (fun posf -> if same_col pos posf then Some (row posf) else None) empty in
                        let col_filled = List.filter_map (fun (posf, markf) -> if same_col pos posf then Some (row posf, markf) else None) filled in
                        check_solve_1d col_empty col_filled mark (row pos) (List.nth y_widths (col pos)) in
                row_check empty filled pos mark && col_check empty filled pos mark in
        let add empty filled pos mark =
                if check empty filled pos mark then
                        let filled = (pos, mark) :: filled in
                        Some (empty, filled)
                else
                        None
        in
        let i_to_pos i =
                (i mod size_x, i / size_x) in
        let board = List.init (size_x * size_y) i_to_pos in
        let empty, filled = board, [] in
        let mark_range = [false; true] in
        let rec aux empty filled =
                match empty with
                | [] -> Some filled
                | pos :: t ->
                        let fold_step acc mark =
                                if Option.is_some acc then acc
                                else
                                        match add t filled pos mark with
                                        | None -> None
                                        | Some (empty, filled) ->
                                                aux empty filled in
                        List.fold_left fold_step None mark_range
        in let filled = aux empty filled
        in match filled with
        | None -> None
        | Some filled ->
                let flat_board = List.map (fun pos -> Option.get (List.find_map (fun (posf, markf) -> if pos = posf then Some markf else None) filled)) board in
                Some (List.fold_left (fun acc mark -> acc ^ (if mark then "#" else "_")) "" flat_board);;

(* easy, almost instant solution *)
solve [[3]; [2; 1]; [3; 2]; [2; 2]; [6]; [1; 5]; [6]; [1]; [2]]
      [[1; 2]; [3; 1]; [1; 5]; [7; 1]; [5]; [3]; [4]; [3]];;

(* hard, solution can take up to an hour *)
Printf.printf "%s" (Option.get (solve [[14]; [1; 1]; [7; 1]; [3; 3]; [2; 3; 2];
         [2; 3; 2]; [1; 3; 6; 1; 1]; [1; 8; 2; 1]; [1; 4; 6; 1]; [1; 3; 2; 5; 1; 1];
         [1; 5; 1]; [2; 2]; [2; 1; 1; 1; 2]; [6; 5; 3]; [12]]
        [[7]; [2; 2]; [2; 2]; [2; 1; 1; 1; 1]; [1; 2; 4; 2];
         [1; 1; 4; 2]; [1; 1; 2; 3]; [1; 1; 3; 2]; [1; 1; 1; 2; 2; 1]; [1; 1; 5; 1; 2];
         [1; 1; 7; 2]; [1; 6; 3]; [1; 1; 3; 2]; [1; 4; 3]; [1; 3; 1];
         [1; 2; 2]; [2; 1; 1; 1; 1]; [2; 2]; [2; 2]; [7]]));;
