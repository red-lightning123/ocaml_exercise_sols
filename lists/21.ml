let insert_at item n l =
        let rec aux n r = function
                | [] -> r
                | h :: t -> aux (n-1) (if n = 0 then h::item::r else h::r) t
        in
        List.rev (aux n [] l);;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
