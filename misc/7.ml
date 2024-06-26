let sudoku board =
        let check num pos filled =
                let same_row (x, y) (x2, y2) =
                        y = y2 in
                let same_col (x, y) (x2, y2) =
                        x = x2 in
                let same_square (x, y) (x2, y2) =
                        x / 3 = x2 / 3 && y / 3 = y2 / 3 in
                let row_check num =
                        List.for_all (fun (posf, vf) -> if same_row pos posf then num <> vf else true) filled in
                let col_check num =
                        List.for_all (fun (posf, vf) -> if same_col pos posf then num <> vf else true) filled in
                let square_check num =
                        List.for_all (fun (posf, vf) -> if same_square pos posf then num <> vf else true) filled in
                row_check num && col_check num && square_check num
        in
        let add empty filled num pos =
                if check num pos filled then
                        let filled = (pos, num) :: filled in
                        Some (empty, filled)
                else
                        None in
        let empty_board board =
                List.init (List.length board) (fun x -> 0) in
        (* ensures that the board is valid by rebuilding it from scratch *)
        let check_all filled =
                let fold_step acc (posf, numf) =
                        match acc with
                        | None -> None
                        | Some (empty, filled) -> add empty filled numf posf in
                Option.is_some (List.fold_left fold_step (Some (empty_board board, [])) filled) in
        let i_to_pos i =
                (i mod 9, i / 9) in
        let board = List.mapi (fun i v -> (i_to_pos i, v)) board in
        let empty, filled = List.partition (fun (_, v) -> v = 0) board in
        let empty = List.map (fun (pos, _) -> pos) empty in
        let num_range = List.init 9 (fun x -> x + 1) in
        let rec aux empty filled =
                match empty with
                | [] -> Some filled
                | pos :: t ->
                        let fold_step acc num =
                                if Option.is_some acc then acc
                                else
                                        match add t filled num pos with
                                        | None -> None
                                        | Some (empty, filled) ->
                                                aux empty filled in
                        List.fold_left fold_step None num_range
        in let filled =
                (* the special case where the board was already full requires special handling since checks are only done on addition *)
                match empty with
                | [] -> if check_all filled then Some filled else None
                | _ -> aux empty filled
        in match filled with
        | None -> None
        | Some filled ->
                Some (List.map (fun (pos, v) -> if v = 0 then Option.get (List.find_map (fun (posf, vf) -> if pos = posf then Some vf else None) filled) else v) board);;

(* the board is represented as a flattened array with 0 standing for an empty cell *)
let example_board =
        [
                0; 0; 4; 8; 0; 0; 0; 1; 7;
                6; 7; 0; 9; 0; 0; 0; 0; 0;
                5; 0; 8; 0; 3; 0; 0; 0; 4;
                3; 0; 0; 7; 4; 0; 1; 0; 0;
                0; 6; 9; 0; 0; 0; 7; 8; 0;
                0; 0; 1; 0; 6; 9; 0; 0; 5;
                1; 0; 0; 0; 8; 0; 3; 0; 6;
                0; 0; 0; 0; 0; 6; 0; 9; 1;
                2; 4; 0; 0; 0; 1; 5; 0; 0
        ];;
sudoku example_board;;
