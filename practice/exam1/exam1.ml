let merge_lists lst1 lst2 = match lst1 with
  | [] -> lst2
  | a::b -> ( match lst2 with
    | [] -> lst1
    | c::d -> a::c::merge_lists b d
  )

let add_helper k n = if n = 1 then [k] else k::add_helper k n-1
let add_k_n_times lst k n i = match lst with 
  | [] -> add helper k n
  | a::b -> if i = 0 then a::(add_helper k n @ b) else a::add_k_n_times b k n (i-1)

let x = 1 in
let f x y = 2 * x + 3 * y in 
let g = f x in 
let x = 4 in
g 3