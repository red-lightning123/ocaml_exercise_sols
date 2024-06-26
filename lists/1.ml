let rec last = function
| [] -> None
| [h] -> Some h
| _ :: t -> last t;;

last ["a"; "b"; "c"; "d"];;
last [];;