let rec gcd a b = if a = 0 then b else gcd (b mod a) a;;
let coprime a b = gcd a b = 1;;

coprime 13 27;;
not (coprime 20536 7826);;
