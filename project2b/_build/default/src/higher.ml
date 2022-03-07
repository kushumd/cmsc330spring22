open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold_right (fun x a -> if x = e then true else a) lst false

let is_present lst x = map (fun i -> if i = x then 1 else 0) lst

let count_occ lst target = fold_right (fun x a -> if x = target then a+1 else a) lst 0

let uniq lst = fold_right (fun x a -> if contains_elem a x then a else x::a) lst []

let assoc_list lst = fold_right (fun x a -> (x, (count_occ lst x)) :: a) (uniq lst) []

let ap fns args = fold_right (fun f a -> (map f args) @ a) fns []
