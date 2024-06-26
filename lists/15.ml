let replicate l n =
        let rec add_times h n l = if n = 0 then l else add_times h (n-1) (h::l)
        in
        let rec aux n b = function
                | [] -> b
                | h :: t -> aux n (add_times h n b) t
        in
        List.rev (aux n [] l);;
replicate ["a"; "b"; "c"] 3;;
