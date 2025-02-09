(* Copyright 2016 Chetan Murthy *)

open OUnit2

let all = "all_tests" >:::
  [
    "somecode_int32_to_int" >::
      (fun ctxt ->
	assert_equal ~printer:string_of_int 45 (Ocaml_somecode.somecode_int32_to_int 45)) 
  ]
  
(* Run the tests in test suite *)
let _ = 
  run_test_tt_main all
;;
