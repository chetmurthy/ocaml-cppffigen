(* Copyright 2016 Chetan Murthy *)

open OUnit2
open Rocks

let canon l = List.sort Pervasives.compare l

let all = "all_tests" >:::
  [
    "single-free" >::
      (fun ctxt ->
	let opts = DBOptions.create() in
	DBOptions.destroy opts ;
      ) ;
    "double-free" >::
      (fun ctxt ->
	let opts = DBOptions.create ~gc:false () in
	DBOptions.destroy opts ;
	assert_raises (Assert_failure("rocks.ml",31,26))
	(fun () -> DBOptions.destroy opts) ;
      ) ;
    "double-free-gc" >::
      (fun ctxt ->
	let opts = DBOptions.create() in
	DBOptions.destroy opts ;
	let n_double_frees0 = !DBOptions.double_frees in
	DBOptions.destroy opts ;
	let n_double_frees1 = !DBOptions.double_frees in
	assert_equal n_double_frees1 (1 + n_double_frees0) ;
      ) ;
    "list-0" >::
      (fun ctxt ->
	assert_raises (Failure "rocksdb_list_column_families: <IOError, None, IO error: While opening a file for sequentially reading: /tmp/CURRENT: No such file or directory>\n")
	  (fun () -> list_column_families "/tmp") ;
      ) ;
    "dboptions-attr" >::
      (fun ctxt ->
	let dboptions = DBOptions.create() in
	assert_bool "better be false" (not (DBOptions.get_create_if_missing dboptions)) ;
	assert_bool "better be false (2)" (not (DBOptions.export dboptions).create_if_missing) ;
	DBOptions.set_create_if_missing dboptions true ;
	assert_bool "better be true" (DBOptions.get_create_if_missing dboptions) ;
	assert_bool "better be true (2)" (DBOptions.export dboptions).create_if_missing ;
      ) ;
    "open-create-missing-fails" >::
      (fun ctxt ->
	assert_raises (Failure "rocksdb_open_column_families: <IOError, None, IO error: While mkdir if missing: /tmp/rocks_teste/aname-missing: No such file or directory>\n")
	  (fun () -> DB.opendb "/tmp/rocks_teste/aname-missing")
      ) ;
    "open-create-missing-no-default-column-family" >::
      (fun ctxt ->
	let dboptions = DBOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	assert_raises (Failure "rocksdb_open_column_families: <InvalidArgument, None, Invalid argument: Default column family not specified>\n")
	(fun () -> DB.opendb ~opts:dboptions "/tmp/rocks_tests/aname-missing") ;
      ) ;
    "open-create-missing-ok-default-column-family" >::
      (fun ctxt ->
	let dboptions = DBOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	let dbh = DB.opendb ~opts:dboptions
	  ~cfds:["default", CFOptions.create()]
	  "/tmp/rocks_tests/aname-missing-ok-default-column-family" in
	()
      ) ;
    "open-create-missing-ok-simple" >::
      (fun ctxt ->
	let options = Options.create() in
	Options.set_create_if_missing options true ;
	let dbh = DB0.opendb ~opts:options "/tmp/rocks_tests/aname-missing-ok-simple" in
	()
      ) ;
    "insert-get-0" >::
      (fun ctxt ->
	let options = Options.create() in
	Options.set_create_if_missing options true ;
	let dbh = DB0.opendb ~opts:options "/tmp/rocks_tests/aname-insert-get-0"  in
	assert_raises Not_found
	  (fun () -> DB0.get dbh "foo") ;
	DB0.delete dbh "foo" ;
	DB0.put dbh "foo" "bar" ;
	assert_equal "bar" (DB0.get dbh "foo") ;
	DB0.delete dbh "foo" ;
	assert_raises Not_found
	  (fun () -> DB0.get dbh "foo") ;
	()
      ) ;
    "insert-get-1" >::
      (fun ctxt ->
	let dboptions = DBOptions.create() in
	let cfoptions = CFOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	let cfname = "default" in
	let dbh = DB.opendb ~opts:dboptions
	  ~cfds:[cfname,cfoptions]
	  "/tmp/rocks_tests/aname-insert-get-1"  in
	assert_raises Not_found
	  (fun () -> DB.cf_get dbh ~cfname "foo") ;
	DB.cf_delete dbh ~cfname "foo" ;
	DB.cf_put dbh ~cfname "foo" "bar" ;
	assert_equal "bar" (DB.cf_get dbh ~cfname "foo") ;
	DB.cf_delete dbh ~cfname "foo" ;
	assert_raises Not_found
	  (fun () -> DB.cf_get dbh ~cfname "foo") ;
	()
      ) ;
    "cf-0" >::
      (fun ctxt ->
	let dboptions = DBOptions.create() in
	let cfoptions = CFOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	let cfname0 = "default" in
	let cfname1 = "cf1" in
	assert_raises (Failure "rocksdb_open_column_families: <InvalidArgument, None, Invalid argument: Column family not found: : cf1>\n")
	(fun () -> DB.opendb ~opts:dboptions
	  ~cfds:[cfname0, cfoptions; cfname1, cfoptions]
	  "/tmp/rocks_tests/aname-cf-0") ;
	()
      ) ;
    "cf-1" >::
      (fun ctxt ->
	let path = "/tmp/rocks_tests/aname-cf-1" in
	let dboptions = DBOptions.create() in
	let cfoptions = CFOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	let cfname0 = "default" in
	let cfname1 = "cf1" in
	DB.with_db ~opts:dboptions ~cfds:[cfname0, cfoptions] path
	  ~f:(fun dbh ->
	    assert_equal [cfname0] (list_column_families path) ;
	    DB.create_cf dbh ~opts:cfoptions cfname1 ;
	    assert_equal (canon [cfname0; cfname1]) (canon (list_column_families path)) ;
	    let cfname = cfname1 in
	    begin
	      assert_raises Not_found
		(fun () -> DB.cf_get dbh ~cfname "foo") ;
	      DB.cf_delete dbh ~cfname "foo" ;
	      DB.cf_put dbh ~cfname "foo" "bar" ;
	      assert_equal "bar" (DB.cf_get dbh ~cfname "foo") ;
	      DB.cf_delete dbh ~cfname "foo" ;
	      assert_raises Not_found
		(fun () -> DB.cf_get dbh ~cfname "foo") ;
	    end) ;
	DB.with_db ~opts:dboptions ~cfds:[cfname0, cfoptions; cfname1, cfoptions] path
	  ~f:(fun dbh ->
	    let cfname = cfname1 in
	    begin
	      assert_raises Not_found
		(fun () -> DB.cf_get dbh ~cfname "foo") ;
	      DB.cf_delete dbh ~cfname "foo" ;
	      DB.cf_put dbh ~cfname "foo" "bar" ;
	      assert_equal "bar" (DB.cf_get dbh ~cfname "foo") ;
	      DB.cf_delete dbh ~cfname "foo" ;
	      assert_raises Not_found
		(fun () -> DB.cf_get dbh ~cfname "foo") ;
	    end ;
	    DB.drop_cf dbh cfname1 ;
	    assert_equal [cfname0] (list_column_families path)
	  );
	()
      ) ;
    "iterator-0" >::
      (fun ctxt ->
	let dboptions = DBOptions.create() in
	let cfoptions = CFOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	let cfname = "default" in
	let dbh = DB.opendb ~opts:dboptions
	  ~cfds:[cfname,cfoptions]
	  "/tmp/rocks_tests/aname-iterator-0"  in
	DB.with_iterator dbh ~f:(fun it ->
	  assert_bool "better NOT be valid" (not (Iterator.valid it)) ;
	  Iterator.seek_to_first it ;
	  assert_bool "better NOT be valid (2)" (not (Iterator.valid it))
	);
	()
      ) ;
    "iterator-1" >::
      (fun ctxt ->
	let path = "/tmp/rocks_tests/aname-iterator-1" in
	let dboptions = DBOptions.create() in
	let cfoptions = CFOptions.create() in
	DBOptions.set_create_if_missing dboptions true ;
	let cfname0 = "default" in
	let cfname1 = "cf1" in
	DB.with_db ~opts:dboptions ~cfds:[cfname0,cfoptions] path
       ~f:(fun dbh ->
	 DB.create_cf dbh ~opts:cfoptions cfname1 ;
	 List.iter (fun s ->
	   DB.put dbh s s ;
	   DB.cf_put ~cfname:cfname1 dbh s s ;
	 )
	  ["a"; "aa"; "ab";
	   "c"; "ca"; "cb";
	   "e"] ;
	 ()
       ) ;

	let iter_test it =
	  assert_bool "better NOT be valid" (not (Iterator.valid it)) ;
	  Iterator.seek_to_first it ;
	  assert_bool "better BE valid" (Iterator.valid it) ;
	  assert_equal "a" (Iterator.key it) ;
	  assert_equal "a" (Iterator.value it) ;
	  Iterator.seek_for_prev it "b" ;
	  assert_equal "ab" (Iterator.key it) ;
	  Iterator.seek_for_prev it "c" ;
	  assert_equal "c" (Iterator.key it) ;
	  Iterator.seek_to_last it ;
	  Iterator.next it ;
	  assert_bool "better NOT be valid (2)" (not (Iterator.valid it)) ;
	  let l = (List.map fst (Iterator.to_list (Iterator.forward ~from:"a" ~ok:(fun k v -> k <= "ab") it))) in
	  assert_equal ~msg:"forward-1" ["a"; "aa"; "ab"]
	    (List.map fst (Iterator.to_list (Iterator.forward ~from:"a" ~ok:(fun k v -> k <= "ab") it))) ;
	  assert_equal ~msg:"reverse-1" ["ab"; "aa"; "a"]
	    (List.map fst (Iterator.to_list (Iterator.reverse ~from:"ab" ~ok:(fun k v -> k >= "a") it)))
	in

	DB.with_db ~opts:dboptions ~cfds:[cfname0,cfoptions; cfname1,cfoptions] path
	  ~f:(fun dbh ->
	    DB.with_iterator dbh ~f:iter_test ;
	    DB.with_iterator dbh ~cfname:cfname1 ~f:iter_test ;
	    ()
	  ) ;

	DB.with_db ~readonly:true ~opts:dboptions ~cfds:[cfname0,cfoptions; cfname1,cfoptions] path
	  ~f:(fun dbh ->
	    DB.with_iterator dbh ~f:iter_test ;
	    DB.with_iterator dbh ~cfname:cfname1 ~f:iter_test ;
	    ()
	  ) ;
      ) ;
  ]
  
(* Run the tests in test suite *)
let _ = 
  if Sys.file_exists "/tmp/rocks_tests" then ignore(Unix.system "rm -rf /tmp/rocks_tests") ;
  Unix.system "mkdir -p /tmp/rocks_tests" ;
  run_test_tt_main all
;;
