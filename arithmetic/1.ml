let is_prime n =
        let rec aux i =
                if i = 1 then true
                else if n mod i = 0 then false
                else aux (i - 1)
        in n <> 1 && aux (n-1);;
not (is_prime 1);;
is_prime 7;;
not (is_prime 12);;
