open List
open Sets


(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(* contains checks if a list contains a certain value *)
let rec contains lst v = match lst with
  | [] -> false
  | a::b -> if v = a then true else contains b v 

(* overlap checks if two lists have any values that are the same *)
let rec overlap lst1 lst2 = match lst2 with
| [] -> false
| a::b -> if contains lst1 a then true else overlap lst1 b

(* get_new_states gets a list of possible new NFA states given a starting state and input character *)
let rec get_new_states (transitions: ('q,'s) transition list) (state: 'q) (sym: 's option) (lst : 'q list) : 'q list = match transitions with 
  | [] -> lst
  | a::b -> (
    match a with (x, y, z) -> if state = x && sym = y then get_new_states b state sym (Sets.union lst [z]) else get_new_states b state sym lst
  )

(****************)
(* Part 1: NFAs *)
(****************)

let rec move_aux (transitions: ('q,'s) transition list) (states: 'q list) (sym: 's option) : 'q list = match states with
  | [] -> []
  | a::b -> get_new_states transitions a sym (move_aux transitions b sym)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match s with
  | None -> move_aux nfa.delta qs s
  | Some x -> if contains nfa.sigma x then move_aux nfa.delta qs s else []

let rec e_closure_aux (transitions: ('q,'s) transition list) (qs: 'q list) : 'q list = match (move_aux transitions qs None) with
  | [] -> qs
  | a::b -> if Sets.union qs (a::b) = qs then qs else e_closure_aux transitions (Sets.union qs (a::b)) 

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = e_closure_aux nfa.delta qs

let rec accept_aux (nfa: ('q,char) nfa_t) (states: 'q list) (s: char list) : bool = match s with
  | [] -> overlap nfa.fs (e_closure nfa states)
  | a::b -> let x = (move nfa (e_closure nfa states) (Some a)) in accept_aux nfa x b


let accept (nfa: ('q,char) nfa_t) (s: string) : bool = accept_aux nfa [nfa.q0] (explode s)
  

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec new_states_aux (nfa: ('q,'s) nfa_t) (qs: 'q list) (alphabet: 's list) (lst: 'q list list) : 'q list list = match alphabet with
  | [] -> lst
  | a::b -> new_states_aux nfa qs b (Sets.union lst [e_closure nfa (move nfa qs (Some a))])

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = new_states_aux nfa qs nfa.sigma []
  
let rec new_trans_aux (nfa: ('q, 's) nfa_t) (qs: 'q list) (alphabet: 's list) : ('q list, 's) transition list = match alphabet with 
    | [] -> []
    | x::y -> match (new_states_aux nfa qs [x] []) with 
      | [] -> (new_trans_aux nfa qs y)
      | a::b -> [(qs, Some x, a)] @ (new_trans_aux nfa qs y)

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = if qs = [] then [] else new_trans_aux nfa qs nfa.sigma 

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = if overlap nfa.fs qs then [qs] else [] 

let rec list_of_finals (nfa: ('q,'s) nfa_t) (fs: 'q list list) : 'q list list = match fs with 
  | [] -> []
  | a::b -> (new_finals nfa a) @ (list_of_finals nfa b)

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (work: 'q list list) : ('q list, 's) nfa_t = match work with 
  | [] -> dfa
  | a::b -> let x = new_states nfa a in nfa_to_dfa_step nfa {sigma = dfa.sigma; qs = Sets.union dfa.qs [a]; q0 = dfa.q0; delta = Sets.union dfa.delta (new_trans nfa a); fs = Sets.union dfa.fs (new_finals nfa a)} (Sets.union b (Sets.diff x (Sets.union dfa.qs [a])))

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = nfa_to_dfa_step nfa {sigma = nfa.sigma; qs = [e_closure nfa [nfa.q0]]; q0 = e_closure nfa [nfa.q0]; fs = []; delta = []} [e_closure nfa [nfa.q0]]

(* I'm sorry my code sucks*)