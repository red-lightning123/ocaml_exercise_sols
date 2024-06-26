type site_pos = { len : int; cells_pos : int list };;

(* assumes the initial state is valid *)
let solve filled_frame empty_sites filled_sites words_left =
        let check filled_frame site_pos word =
                if site_pos.len <> String.length word then
                        false
                else
                        let fold_step (acc, i) cell_pos =
                                let acc =
                                        if acc then
                                                match List.find_map (fun (cell_posf, cell_charf) -> if cell_pos = cell_posf then Some cell_charf else None) filled_frame with
                                                | None -> acc
                                                | Some (cell_char) -> cell_char = '.' || word.[i] = cell_char
                                        else false in
                                acc, i + 1 in
                        let acc, _ = List.fold_left fold_step (true, 0) site_pos.cells_pos in
                        acc in
        let fill_cells site_pos word filled_frame =
                let fold_step (acc, i) cell_pos =
                        (cell_pos, word.[i])::acc, i + 1 in
                let acc, _ = List.fold_left fold_step (filled_frame, 0) site_pos.cells_pos in
                acc in
        let add filled_frame empty_sites filled_sites site_pos word =
                if check filled_frame site_pos word then
                        let filled_frame = fill_cells site_pos word filled_frame in
                        let filled_sites = (site_pos, word) :: filled_sites in
                        Some (filled_frame, empty_sites, filled_sites)
                else
                        None
        in
        let rec aux filled_frame empty_sites filled_sites words_left =
                match empty_sites with
                | [] -> Some filled_frame
                | site_pos :: t ->
                        let fold_step (acc, prev_words_left, next_words_left) word =
                                match next_words_left with
                                | [] -> raise Not_found
                                | word :: next_words_left_t ->
                                        let acc =
                                                if Option.is_some acc then acc
                                                else
                                                        match add filled_frame t filled_sites site_pos word with
                                                        | None -> None
                                                        | Some (filled_frame, empty_sites, filled_sites) ->
                                                                aux filled_frame empty_sites filled_sites (List.rev_append prev_words_left next_words_left_t) in
                                        acc, word::prev_words_left, next_words_left_t in
                        let acc, _, _ = List.fold_left fold_step (None, [], words_left) (List.init (List.length words_left) (fun x -> x)) in
                        acc
        in let filled_frame = aux filled_frame empty_sites filled_sites words_left
        in filled_frame;;

let read_crossword filepath =
        let read_words ic =
                let rec aux acc =
                        let line = input_line ic in
                        if line = "" then
                                acc
                        else
                                aux (line::acc) in
                List.rev (aux []) in
        let read_framework ic =
                let rec aux acc =
                        try
                                let line = input_line ic in
                                aux (line::acc)
                        with End_of_file -> acc in
                List.rev (aux []) in
        let string_to_list s =
                let rec aux acc at =
                        if at = -1 then
                                acc
                        else
                                aux (s.[at] :: acc) (at - 1)
                in aux [] (String.length s - 1) in
        let find_horizontal_sites framework =
                let gather_row y_i row =
                        let fold_step (x_i, curr, acc) c =
                                if c <> ' ' then
                                        x_i + 1, (x_i, y_i) :: curr, acc
                                else
                                        let len = List.length curr in
                                        if len = 0 || len = 1 then
                                                x_i + 1, [], acc
                                        else
                                                x_i + 1, [], (List.rev curr)::acc in
                        let _, curr, acc = List.fold_left fold_step (0, [], []) row in
                        let acc =
                                let len = List.length curr in
                                if len = 0 || len = 1 then
                                        acc
                                else
                                        (List.rev curr)::acc in
                        List.rev acc in
                List.concat (List.mapi (fun y_i row -> gather_row y_i row) framework) in
        let find_vertical_sites x_size y_size framework =
                let get_cell (x_i, y_i) =
                        match List.nth_opt framework y_i with
                        | None -> ' '
                        | Some row ->
                                match List.nth_opt row x_i with
                                | None -> ' '
                                | Some cell -> cell in
                let transposed = List.init x_size (fun y_i -> List.init y_size (fun x_i -> get_cell (y_i, x_i))) in
                List.map (List.map (fun (x_i, y_i) -> y_i, x_i)) (find_horizontal_sites transposed) in
        let ic = open_in filepath in
        let words = read_words ic in
        let framework = read_framework ic in
        let framework = List.map string_to_list framework in
        let x_size = List.length (List.hd framework) in
        let y_size = List.length framework in
        let pos_to_i (x, y) =
                x + x_size * y in
        let frame = List.concat (List.mapi (fun y_i row -> List.mapi (fun x_i c -> pos_to_i (x_i, y_i), c) row) framework) in
        let filled_frame = List.filter_map (fun (i, c) -> if c = ' ' then None else Some (i, c)) frame in
        let sites = List.rev_append (find_horizontal_sites framework) (find_vertical_sites x_size y_size framework) in
        let sites = List.map (fun site -> { len = List.length site; cells_pos = List.map pos_to_i site }) sites in
        x_size, y_size, filled_frame, sites, words;;

let crossword filepath =
        let x_size, y_size, filled_frame, sites, words = read_crossword filepath in
        let solved_filled_frame = solve filled_frame sites [] words in
        let pos_to_i (x, y) =
                x + x_size * y in
        match solved_filled_frame with
        | None -> None
        | Some solved_filled_frame ->
                let solved_filled_frame =
                        List.init y_size (
                        fun y_i -> List.init x_size (
                                fun x_i ->
                                        match List.find_map (
                                                fun (pos, c) ->
                                                        if pos_to_i (x_i, y_i) = pos then Some c else None
                                        ) solved_filled_frame with
                                        | None -> ' '
                                        | Some c -> c)) in
                Some (String.concat "\n" (List.map (fun row -> String.concat "" (List.map (fun cell -> String.make 1 cell) row)) solved_filled_frame));;
Printf.printf "%s" (Option.get (crossword "p99b.dat"));;
