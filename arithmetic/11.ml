let is_prime n =
        let rec aux i =
                if i = 1 then true
                else if n mod i = 0 then false
                else aux (i - 1)
        in n <> 1 && aux (n-1);;

let goldbach n =
        let rec aux i =
                if is_prime i && is_prime (n - i) then (i, n - i)
                else aux (i+1) in
        aux 2;;

let goldbach_list low high =
        let rec aux low high r =
                if low > high then r
                else aux (low + 2) high ((low, goldbach low) :: r)
        in
        List.rev (aux (low + (low mod 2)) high []);;

let goldbach_limit low high =
        List.filter (fun (_, (a, b)) -> a > 50 && b > 50) (goldbach_list low high);;

goldbach_list 9 20;;

(*question request 2..300 but goldbach 2 hangs since there is no matching composition*)
goldbach_limit 3 3000;;
