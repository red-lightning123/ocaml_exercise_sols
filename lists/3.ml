let rec nth l n =
        match l with
        | [] -> None
        | h :: t -> if n = 0 then Some(h) else nth t (n-1);;
nth ["a"; "b"; "c"; "d"; "e"] 2;;
nth ["a"] 2;;
