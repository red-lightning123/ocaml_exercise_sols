let remove_at n l =
        let rec aux n r = function
                | [] -> r
                | h :: t -> aux (n-1) (if n = 0 then r else h::r) t
        in
        List.rev (aux n [] l);;

remove_at 1 ["a"; "b"; "c"; "d"];;
