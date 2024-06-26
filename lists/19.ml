let split n l =
        let rec aux r n = function
                | [] -> (r, [])
                | h :: t -> if n = 0 then (r, h::t) else aux (h::r) (n-1) t
        in
        let s, e = aux [] n l
        in
        List.rev s, e;;

let rotate l n =
        let len = List.length l
        in
        let shift = if len = 0 then 0 else ((n mod len) + len) mod len
        in
        let s, e = split shift l
        in
        e @ s;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
