let cmp a b =
        if a < b then -1
        else if a > b then 1
        else 0;;

let length_sort l =
        List.sort (fun a b -> cmp (List.length a) (List.length b)) l;;

let pack_by f l =
        let rec aux a b = function
                | [] -> b
                | [h] -> (h::a) :: b
                | h1 :: h2 :: t -> if f h1 = f h2 then aux (h1::a) b (h2::t) else aux [] ((h1::a)::b) (h2::t)
        in List.rev (aux [] [] l);;

let frequency_sort l =
        let sorted = length_sort l in
        let packed = pack_by (fun x -> List.length x) sorted in
        List.flatten (length_sort packed);;

length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
                ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
