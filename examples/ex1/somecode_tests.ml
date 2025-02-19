(* Copyright 2016 Chetan Murthy *)

open OUnit2

let all = "all_tests" >:::
  [
    "somecode_foo" >::
      (fun ctxt ->
	assert_equal "45" (Ocaml_somecode.somecode_foo 45))
  ; "somecode_int32_to_int" >::
      (fun ctxt ->
	assert_equal ~printer:string_of_int 45 (Ocaml_somecode.somecode_int32_to_int 45l))
  ; "somecode_int_to_int32" >::
      (fun ctxt ->
	assert_equal ~printer:Int32.to_string 45l (Ocaml_somecode.somecode_int_to_int32 45))
  ; "somecode_bar" >::
      (fun ctxt ->
	assert_equal ("foo", 42) (Ocaml_somecode.somecode_bar "foo" 42))
  ; "somecode_size_t_to_string" >::
      (fun ctxt ->
	assert_equal ~printer:(fun x -> x) "deadbeef" (Ocaml_somecode.somecode_size_t_to_string 0xdeadbeefL)
      ; assert_equal ~printer:(fun x -> x) "deadbeefdeadbeef" (Ocaml_somecode.somecode_size_t_to_string 0xdeadbeefdeadbeefL)
      )
  ; "somecode_wal_recovery_mode_from_int" >::
      (fun ctxt ->
	assert_equal '\000' (Ocaml_somecode.somecode_wal_recovery_mode_from_int 0)
       ; assert_equal '\003' (Ocaml_somecode.somecode_wal_recovery_mode_from_int 3)
      )
  ; "somecode_option_1" >::
      (fun ctxt ->
	assert_equal None (Ocaml_somecode.somecode_int32_option_to_int_option None)
      ; assert_equal (Some 1) (Ocaml_somecode.somecode_int32_option_to_int_option (Some 1l))
      )
  ; "somecode_roundtrip" >::
      (fun ctxt ->
        let open Ocaml_somecode in 
	assert_equal None (somecode_roundtrip_int_option None)
      ; assert_equal (Some 1) (somecode_roundtrip_int_option (Some 1))
      ; assert_equal None (somecode_roundtrip_string_option None)
      ; assert_equal (Some "foo") (somecode_roundtrip_string_option (Some "foo"))
      ; assert_equal ST0.{b=true;uc='a'} (somecode_roundtrip_ST0_t ST0.{b=true;uc='a'})
      ; let st = ST.{ b = true ;	uc = 'a' ; n = 1 ; nL = 1L ; s = "foo" ; sz = 0xdeadbeefdeadbeefL } in
        assert_equal st (somecode_roundtrip_ST_t st)
      )
  ]
  
(* Run the tests in test suite *)
let _ = 
  run_test_tt_main all
;;
