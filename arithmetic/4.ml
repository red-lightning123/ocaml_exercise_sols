let rec gcd a b = if a = 0 then b else gcd (b mod a) a;;
let coprime a b = gcd a b = 1;;
let phi n =
        let rec aux i r = if i = 0 then r else aux (i-1) (r + (if coprime n i then 1 else 0)) in
        aux n 0;;

phi 10;;
