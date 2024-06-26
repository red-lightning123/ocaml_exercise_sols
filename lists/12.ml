type 'a rle =
        | One of 'a
        | Many of int * 'a;;

(*let encode l =
        let map_rle (a, h) = if a = 1 then One h else Many (a, h)
        in
        let rec aux a b = function
                | [] -> b
                | [h] -> map_rle(a+1, h) :: b
                | h1 :: h2 :: t ->
                                if h1 = h2 then aux (a+1) b (h2::t)
                                else aux 0 (map_rle(a+1, h1)::b) (h2::t)
        in
        List.rev (aux 0 [] l);;*)

let decode l =
        let rec aux b = function
                | [] -> b
                | One h :: t -> aux (h :: b) t
                | Many (c, h) :: t -> if c = 0 then aux b t else aux (h :: b) (Many(c - 1, h) :: t)
        in
        List.rev (aux [] l);;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
