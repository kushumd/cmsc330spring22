(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
  match tup with
    (x, y, z) -> (z, y, x)

let is_odd x = if x mod 2 == 1 then true else false

let area x y = 
  match x with (x1, y1)
  match y with (x2, y2) -> abs ((x2-x1) * (y2 - y1))

let volume x y = failwith "unimplemented"

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = failwith "unimplemented"

let rec pow x y = failwith "unimplemented"

let rec log x y = failwith "unimplemented"

let rec gcf x y = failwith "unimplemented"

let rec is_prime x = failwith "unimplemented"

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = failwith "unimplemented"

let larger lst1 lst2 = failwith "unimplemented"

let reverse lst = failwith "unimplemented"

let rec combine lst1 lst2 = failwith "unimplemented"

let rec merge lst1 lst2 = failwith "unimplemented"

let rec rotate shift lst = failwith "unimplemented"

let rec is_palindrome lst = failwith "unimplemented"