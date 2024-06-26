let pop_at_rev n l =
        let rec aux n r = function
                | [] -> raise Not_found
                | h :: t ->
                        if n = 0 then (h, r @ t)
                        else aux (n-1) (h :: r) t
        in
        aux n [] l;;


let rand_select l n =
        let rec aux n len r l =
                if n = 0 then r
                else
                let chosen = Random.int len
                in
                let v, l = pop_at_rev chosen l
                in
                aux (n-1) (len - 1) (v :: r) l
        in
        let len = List.length l
        in
        aux n len [] l;;

let range left right =
        let rec aux left right r =
                if left = right then left :: r else aux (left + 1) right (left :: r)
        in
        if left < right then List.rev (aux left right [])
        else aux right left [];;

let lotto_select n m =
        rand_select (range 1 m) n;;
lotto_select 6 49;;
