type site_spec = { start_pos : int * int; horizontal : bool; len : int; ref_cells : (int * int) list };;


(* assumes the initial state is valid *)
let solve sites intersections empty_sites filled_sites filled_chars words_left =
        let find_cell_fill_opt cell_i filled_chars =
                List.find_opt (
                        fun (cell2_i, c) -> cell_i = cell2_i
                ) filled_chars in
        let check site_i word filled_chars =
                let site = sites.(site_i) in
                if site.len <> String.length word then
                        false
                else
                        List.for_all (
                                fun (str_i, cell_i) ->
                                        match find_cell_fill_opt cell_i filled_chars with
                                        | None -> true
                                        | Some (_, c) -> c = word.[str_i]
                        ) site.ref_cells in
        let fill_cells site_i word filled_chars =
                let fold_step acc (str_i, cell_i) =
                        if Option.is_some (find_cell_fill_opt cell_i filled_chars) then
                                acc
                        else
                                (cell_i, word.[str_i])::acc in
                let acc = List.fold_left fold_step filled_chars sites.(site_i).ref_cells in
                acc in
        let add empty_sites filled_sites filled_chars site_i word =
                if check site_i word filled_chars then
                        let filled_chars = fill_cells site_i word filled_chars in
                        let filled_sites = (site_i, word) :: filled_sites in
                        Some (empty_sites, filled_sites, filled_chars)
                else
                        None
        in
        let rec aux empty_sites filled_sites filled_chars words_left =
                match empty_sites with
                | [] -> Some filled_sites
                | site_i :: t ->
                        let fold_step (acc, prev_words_left, next_words_left) word =
                                match next_words_left with
                                | [] -> raise Not_found
                                | word :: next_words_left_t ->
                                        let acc =
                                                if Option.is_some acc then acc
                                                else
                                                        match add t filled_sites filled_chars site_i word with
                                                        | None -> None
                                                        | Some (empty_sites, filled_sites, filled_chars) ->
                                                                aux empty_sites filled_sites filled_chars (List.rev_append prev_words_left next_words_left_t) in
                                        acc, word::prev_words_left, next_words_left_t in
                        let acc, _, _ = List.fold_left fold_step (None, [], words_left) (List.init (List.length words_left) (fun x -> x)) in
                        acc
        in let filled_sites = aux empty_sites filled_sites filled_chars words_left
        in filled_sites;;

let top_sort sites intersections =
        let sites = Array.of_list sites in
        let intersections = Array.of_list intersections in
        let n_sites = Array.length sites in
        let visited = Array.make n_sites false in
        let rec dfs acc site_i =
                if visited.(site_i) then
                        acc
                else
                        (
                                visited.(site_i) <- true;
                                List.fold_left (
                                        fun acc (_, cell_i) ->
                                                List.fold_left dfs acc intersections.(cell_i)
                                ) (site_i::acc) sites.(site_i).ref_cells
                        ) in
        List.fold_left dfs [] (List.init n_sites (fun x -> x));;

(*
reads the crossword in filepath and returns
#0 site_is_top_sorted - indexes of sites topologically sorted for efficient solving
#1 sites - the sites, with type site_spec
#2 intersection - any points where two sites meet, with type cell_spec
#3 words - list of the words to be filled in
*)
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
        let frame = List.concat (List.mapi (fun y_i row -> List.mapi (fun x_i c -> (x_i, y_i), c) row) framework) in
        let filled_frame = List.filter_map (fun (i, c) -> if c = ' ' then None else Some (i, c)) frame in
        let sites = List.rev_append (find_horizontal_sites framework) (find_vertical_sites x_size y_size framework) in
        let count_site_pos pos sites = List.fold_left (fun acc site -> List.fold_left (fun acc pos2 -> if pos = pos2 then acc + 1 else acc) acc site) 0 sites in
        (*
        intersections are the "important" cells - those that have to be accounted for
        during the assignment of new characters
        there are two reasons why a cell might be "important"
        1. it is shared between several sites (hence the name)
        2. it already has a predefined character
        *)
        let intersections = List.filter (fun (pos, c) -> count_site_pos pos sites >= 2 || c <> '.') filled_frame in
        let findi_opt f l =
                let rec aux i = function
                        | [] -> None
                        | h :: t -> if f h then Some i else aux (i + 1) t
                in aux 0 l
        in
        let sites = List.map (
                fun site ->
                        let pos1 = List.hd site in
                        let pos2 = List.hd (List.tl site) in
                        let horizontal = if fst pos1 = fst pos2 then false else if snd pos1 = snd pos2 then true else raise (Failure "site was neither horizontal nor vertical") in
                        let len = List.length site in
                        let ref_cells =
                                List.filter_map (
                                        fun (str_i, cell) ->
                                                match findi_opt (
                                                        fun (cell2, _) ->
                                                                cell = cell2
                                                        ) intersections with
                                                | None -> None
                                                | Some cell_i -> Some (str_i, cell_i)
                                ) (List.mapi (fun i x -> i, x) site) in
                        {
                                start_pos = pos1;
                                horizontal;
                                len;
                                ref_cells
                        }
        ) sites in
        let sitesi = List.mapi (fun i site -> i, site) sites in
        let chars, intersections = List.split (
                List.mapi (
                        fun cell_i (_, c) ->
                                let ref_sites =
                                        List.filter_map (
                                                fun (site_i, site) ->
                                                        if List.exists (fun (_, cell2_i) -> cell2_i = cell_i) site.ref_cells then Some site_i
                                                        else None
                                        ) sitesi in
                                (cell_i, c), ref_sites
                ) intersections
        ) in
        let chars = List.filter (fun (_, c) -> c <> '.') chars in
        let site_is_top_sorted = top_sort sites intersections in
        x_size, y_size, sites, intersections, site_is_top_sorted, chars, words;;


let crossword filepath =
        let x_size, y_size, sites, intersections, site_is, chars, words = read_crossword filepath in
        let sites = Array.of_list sites in
        let solved_filled_sites = solve sites intersections site_is [] chars words in
        match solved_filled_sites with
        | None -> None
        | Some solved_filled_sites ->
                let board = Array.make (x_size * y_size) ' ' in
                let iter_step (site_i, word) =
                        let site = sites.(site_i) in
                        let x_s, y_s = site.start_pos in
                        List.iter (
                                fun a ->
                                        let x, y = if site.horizontal then x_s + a, y_s else x_s, y_s + a in
                                        board.(x + x_size * y) <- word.[a]
                        ) (List.init site.len (fun x -> x)) in
                List.iter iter_step solved_filled_sites;
                Some (
                        String.concat "\n" (
                                List.init y_size (
                                        fun y ->
                                                String.init x_size (fun x ->
                                                        board.(x + x_size * y)
                                                )
                                )
                        )
                );;
let print_crossword filepath =
        Printf.printf "%s\n" filepath;
        match crossword filepath with
        | None -> Printf.printf "None\n\n"
        | Some s -> Printf.printf "%s\n" s;;

print_crossword "p99a.dat";;
print_crossword "p99b.dat";;
print_crossword "p99c.dat";;
print_crossword "p99d.dat";;
