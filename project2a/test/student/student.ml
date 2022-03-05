open OUnit2
open Basics

let test_sanity _ =
  assert_equal ["a"; "b"; "c"; "d"] (rotate 0 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";
  assert_equal ["b"; "c"; "d"; "a"] (rotate 1 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";
  assert_equal ["c"; "d"; "a"; "b"] (rotate 2 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";
  assert_equal ["d"; "a"; "b"; "c"] (rotate 3 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";
  assert_equal ["a"; "b"; "c"; "d"] (rotate 4 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";
  assert_equal ["b"; "c"; "d"; "a"] (rotate 5 ["a"; "b"; "c"; "d"]) ~msg:"rotate (1)";


let suite =
  "student" >::: [
    "sanity" >:: test_sanity
  ]

let _ = run_test_tt_main suite
