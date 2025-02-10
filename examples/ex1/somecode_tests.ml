(* Copyright 2016 Chetan Murthy *)

open OUnit2

let all = "all_tests" >:::
  [
    "somecode_foo" >::
      (fun ctxt ->
	assert_equal "45" (Ocaml_somecode.somecode_foo 45))
  ; "somecode_int32_to_int" >::
      (fun ctxt ->
	assert_equal ~printer:string_of_int 45 (Ocaml_somecode.somecode_int32_to_int 45))
  ; "somecode_int_to_int32" >::
      (fun ctxt ->
	assert_equal ~printer:string_of_int 45 (Ocaml_somecode.somecode_int_to_int32 45))
(*
  ; "somecode_bar" >::
      (fun ctxt ->
	assert_equal ("foo", 42) (Ocaml_somecode.somecode_bar "foo" 42))
 *)
  ]
  
(* Run the tests in test suite *)
let _ = 
  run_test_tt_main all
;;
