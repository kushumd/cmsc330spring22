open P3.Nfa
open P3.Regexp
open TestUtils
open OUnit2

let test_placeholder _ =
  let m1 ={
    qs= [0; 1, 2]; 
    sigma= ['a'; 'b']; 
    delta= [(0, Some 'b', 1); (1, Some 'b', 1), (1, Some 'a', 2); (2, Some 'b', 2), (2, None, 0)]; 
    q0= 0; 
    fs= [2]}
  in
  assert_nfa_accept m1 "bbaba" ;
  assert_nfa_accept m1 "babbaba" ;
  assert_nfa_deny m1 "bbaabb" ;
  assert_nfa_accept m1 "bbbab" ;
let suite =
  "student"
  >::: [ "nfa_new_states" >:: test_placeholder ]

let _ = run_test_tt_main suite
