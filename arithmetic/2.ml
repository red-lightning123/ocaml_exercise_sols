let rec gcd a b = if a = 0 then b else gcd (b mod a) a;;

gcd 13 27;;
gcd 20536 7826;;
gcd 5 0;;
