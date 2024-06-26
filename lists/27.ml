

let rec extract k list =
        if k <= 0 then [([], list)]
        else match list with
                | [] -> []
                | h :: t ->
                        let with_h = List.map (fun (vals, new_list) -> (h::vals, new_list)) (extract (k-1) t)
                        in
                        let without_h = List.map (fun (vals, new_list) -> (vals, h::new_list)) (extract k t)
                        in
                        with_h @ without_h;;

let rec group list = function
        | [] -> [[]]
        | h :: t ->
                        List.fold_left(fun acc (vals, rest) -> acc @ (List.map (fun x -> vals::x) (group rest t))) [] (extract h list);;

group ["a"; "b"; "c"; "d"] [2; 1];;
