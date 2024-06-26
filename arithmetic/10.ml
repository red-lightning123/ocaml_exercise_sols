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

goldbach 28;;
