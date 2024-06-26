let is_prime n =
        let rec aux i =
                if i = 1 then true
                else if n mod i = 0 then false
                else aux (i - 1)
        in n <> 1 && aux (n-1);;

let all_primes low high =
        let rec aux low high r =
                if low > high then r
                else aux (low + 1) high (if is_prime(low) then low::r else r) in
        aux low high [];;

List.length (all_primes 2 7920);;
