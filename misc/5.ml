let full_words n =
        let digit_repr n =
                let rec aux acc n =
                        if n = 0 then
                                acc
                        else
                                aux ((n mod 10)::acc) (n / 10)
                in
                if n = 0 then
                        [0]
                else
                        aux [] n
        in
        let digit_to_word = function
                | 0 -> "zero"
                | 1 -> "one"
                | 2 -> "two"
                | 3 -> "three"
                | 4 -> "four"
                | 5 -> "five"
                | 6 -> "six"
                | 7 -> "seven"
                | 8 -> "eight"
                | 9 -> "nine"
                | _ -> raise Not_found
        in String.concat "-" (List.map digit_to_word (digit_repr n));;


full_words 175;;
full_words 0;;
