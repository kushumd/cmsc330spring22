(*************************)
(* Part 0: Aux Functions *)
(*************************)

let abs x = 
  if x < 0 then
    x * (-1)
  else 
    x

let rec is_prime_aux x y = 
  if x mod y = 0 then
    false
  else if (y*y) > x then
    true
  else
    is_prime_aux x (y+1)

let rec larger_aux lst = 
  match lst with 
  | [] -> 0
  | (x::y) -> 1 + larger_aux y

let rec reverse_aux lst = 
  match lst with 
  | [] -> []
  | (x::y) -> (reverse_aux y)@[x]

let rec is_palindrome_aux lst rev_lst =
  match (rev_lst) with
  | [] -> true
  | (rev_x::rev_y) -> (
    match lst with
    | [] -> failwith ("Somehow list reverse isn't empty when list is")
    | (x::y) -> (
      if x != rev_x then
        false
      else
        is_palindrome_aux y rev_y
    )
  )
  
(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup =
  match tup with
    (x, y, z) -> (z, y, x)

let is_odd x = 
  if abs x mod 2 = 1 then 
    true 
  else 
    false

let area x y = 
  match x,y with (x1, y1), (x2, y2) -> abs ((x2 - x1) * (y2 - y1))

let volume x y = 
  match x,y with (x1, y1, z1), (x2, y2, z2) -> abs ((x2 - x1) * (y2 - y1) * (z2 - z1))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  if n <= 2 then
    1
  else
    fibonacci(n-1) + fibonacci(n-2)

let rec pow x p = 
  if p = 0 then
    1
  else
    x * (pow x (p-1))

let rec log x y =
  if y/x < 1 then 
    0
  else
    1 + log x (y/x)

let rec gcf x y = 
  if y = 0 then
    x
  else 
    gcf y (x mod y)
  

let is_prime x = 
  if x <= 1 then
    false
  else if x = 2 || x = 3 then
    true
  else
    is_prime_aux x 2

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  match lst with 
  | (x::y) ->
    if idx = 0 then
      x
    else 
      get (idx - 1) y
  | [] -> failwith "Out of bounds"

let larger lst1 lst2 = 
  if larger_aux lst1 > larger_aux lst2 then
    lst1
  else
    lst2 
  
let reverse lst = reverse_aux lst

let rec combine lst1 lst2 = 
  match lst1 with 
  | (x1::y1) -> x1 :: combine y1 lst2
  | [] -> lst2

let rec merge lst1 lst2 = 
  match lst1 with 
  | [] -> lst2
  |(x1::y1) -> (
    match lst2 with
    | [] -> lst1
    | (x2::y2) -> (
      if x1 < x2 then
        x1 :: merge y1 lst2
      else 
        x2 :: merge lst1 y2
    )
  )

let rec rotate shift lst = 
  match lst with
  | [] -> []
  | (x::y) -> (
    if shift = 0 then
      lst
    else rotate (shift-1) y@[x]
  )

let rec is_palindrome lst = 
  match (reverse lst) with
  | [] -> true
  | (_::_) -> (
    match lst with
    | [] -> failwith ("Somehow list reverse isn't empty when list is")
    | (_::_) -> (is_palindrome_aux lst (reverse lst))
  )