type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let layout_binary_tree_3 tree =
        let rec translate_x d = function
                | Empty -> Empty
                | Node ((v, x, y), l, r) -> Node ((v, x + d, y), translate_x d l, translate_x d r)
        in
        let rec rightest_in = function
                | [] -> 0
                | Empty :: t -> rightest_in t
                | Node ((_, x, _), _, _) :: t -> max x (rightest_in t)
        in
        let rec leftest_in = function
                | [] -> 0
                | Empty :: t -> leftest_in t
                | Node ((_, x, _), _, _) :: t -> min x (leftest_in t)
        in
        let rec next_level = function
                | [] -> []
                | Empty :: t -> next_level t
                | Node (_, l, r) :: t -> l :: (r :: (next_level t))
        in
        let rec max_dist ln rn =
                let rightest_l = rightest_in ln in
                let leftest_r = leftest_in rn in
                let dist = rightest_l + (-leftest_r) in
                let nextln = next_level ln in
                let nextrn = next_level rn in
                if nextln = [] || nextrn = [] then dist else max dist (max_dist nextln nextrn)
        in
        let rec aux level xc = function
                | Empty -> Empty
                | Node (v, Empty, Empty) ->
                        Node ((v, xc, level), Empty, Empty)
                | Node (v, l, Empty) ->
                        let adj_width = 1 in
                        Node ((v, xc, level), aux (level + 1) (xc - adj_width) l, Empty)
                | Node (v, Empty, r) ->
                        let adj_width = 1 in
                        Node ((v, xc, level), Empty, aux (level + 1) (xc + adj_width) r)
                | Node (v, l, r) ->
                        let tl = aux (level + 1) 0 l in
                        let tr = aux (level + 1) 0 r in
                        let dist = max_dist [tl] [tr] in
                        let adj_width = if dist mod 2 = 0 then 1 + dist / 2 else (dist + 1) / 2 in
                        Node ((v, xc, level), translate_x (xc - adj_width) tl, translate_x (xc + adj_width) tr)
        in
        let rec flatten = function
                | Empty -> []
                | Node (_, l, r) as node -> node :: (flatten l @ flatten r)
        in
        let t = aux 1 0 tree in translate_x (1 - leftest_in (flatten t)) t;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty));;

layout_binary_tree_3 example_layout_tree ;;
