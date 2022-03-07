open Funs

(*********************************)
(* Part 2: Three-Way Search Tree *)
(*********************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (a, b, c, d, e) -> match b with
    | None -> if x > a then IntNode (a, Some x, c, d, e) else (
      if x = a then t else IntNode (a, b, int_insert x c, d, e)
    )
    | Some v -> (
      if x = a || x = v then t else (
        if x < a then IntNode (a, b, int_insert x c, d, e) else (
          if x > v then IntNode (a, b, c, d, int_insert x e) else IntNode (a, b, c, int_insert x d, e)
        )
      )
    )

let rec int_mem x t = match t with 
  | IntLeaf -> false
  | IntNode (a, b, c, d, e) -> match b with 
    | None -> (
      if x = a then true else if x < a then int_mem x c else false
    )
    | Some v -> (
      if x = a || x = v then true else if x < a then int_mem x c else (
        if x < v then int_mem x d else int_mem x e
      )
    )

let rec int_size t = match t with 
  | IntLeaf -> 0
  | IntNode (a, b, c, d, e) -> match b with
    | None -> 1 + int_size c + int_size d + int_size e
    | Some v -> 2 + int_size c + int_size d + int_size e

let rec int_max t = match t with 
| IntLeaf -> raise (Invalid_argument("int_max"))
| IntNode (a, b, c, d, e) -> match b with
  | None -> a
  | Some v -> (
    try
      int_max e
    with Invalid_argument i -> v
  )

(*******************************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
  | MapLeaf -> MapNode ((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode (a, b, c, d, e) -> match a with (ak, av) ->
    if ak > k then MapNode (a, b, map_put k v c, d, e) else match b with
    | None -> if ak < k then MapNode (a, Some (k, v), c, d, e) else raise (Invalid_argument("map_put"))
    | Some (bk, bv) -> (
      if bk > k && ak < k then MapNode (a, b, c, map_put k v d, e) else if bk < k then MapNode (a, b, c, d, map_put k v e) else raise (Invalid_argument("map_put"))
    )

let rec map_contains k t = match t with
  | MapLeaf -> false
  | MapNode (a, b, c, d, e) -> match a with (ak, av) ->
    if ak = k then true else if ak > k then map_contains k c else match b with 
    | None -> false
    | Some (bk, bv) -> (
      if bk = k then true else if bk > k then map_contains k d else map_contains k e
    )

let rec map_get k t = match t with
| MapLeaf -> raise (Invalid_argument("map_get"))
| MapNode (a, b, c, d, e) -> match a with (ak, av) ->
  if ak = k then av else if ak > k then map_get k c else match b with 
  | None -> raise (Invalid_argument("map_get"))
  | Some (bk, bv) -> (
    if bk = k then bv else if bk > k then map_get k d else map_get k e
  )

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  | NoScope
  | Scope of (string * int) list * lookup_table

let empty_table = NoScope

let rec push_scope t = match t with 
  | NoScope -> Scope ([], NoScope)
  | Scope (a, b) -> Scope (a, push_scope b)

let rec pop_scope t = match t with
  | NoScope -> raise (Failure "No scopes remain!")
  | Scope (a, b) -> (
    try
      Scope (a, pop_scope b)
    with Failure f -> Scope (a, NoScope)
  )
  
let rec add_var_aux n v l = match l with
  | [] -> (n, v) :: []
  | a::b -> a :: add_var_aux n v b

let rec add_var n v t = match t with
  | NoScope -> raise (Failure "There are no scopes to add a variable to!")
  | Scope (a, b) -> (
    try
      Scope(a, add_var n v b)
    with Failure f -> Scope(add_var_aux n v a, b)
  )

let rec lookup_search n l = match l with
  | [] -> raise (Failure "Variable not found!")
  | a::b -> match a with (an, av) ->
    if an = n then av else lookup_search n b

let rec lookup_aux n t = match t with 
  | NoScope -> raise (Invalid_argument("Reached final scope!"))
  | Scope (a, b) -> 
    try
      lookup_aux n b
    with Invalid_argument r -> lookup_search n a

let lookup n t = match t with
  | NoScope -> raise (Failure "Variable not found!")
  | Scope (a, b) ->
    try
      lookup_aux n b
    with Invalid_argument r -> lookup_search n a
