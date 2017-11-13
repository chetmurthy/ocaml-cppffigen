open Core_kernel.Exn

open Misc
open Rocks_types
open Ocaml_rocksdb
open Ocaml_rocksdb.Types

let __default_cfname = "default"

module type GCABLE = sig
  type t
  val double_frees : int ref
  val destroy : t -> unit
end

module GCBox(T : sig
  type args
  type _t
  val create_no_gc : args -> _t
  val destroy : _t -> unit
  val name : string
end) = struct
  type t  = { it : T._t ; destroy : T._t -> unit ; gc : bool ; mutable closed : bool }
  let double_frees = ref 0
 
  let destroy t =
    if not t.closed then begin
      t.closed <- true ;
      t.destroy t.it
    end
    else if not t.gc then assert false
    else begin
      Printf.fprintf stderr "double free detected during GC finalization of %s\n" T.name;
      incr double_frees
    end

  let _raw_create ~gc x =
    { it = T.create_no_gc x ; destroy = T.destroy ; gc ; closed = false }
      
  let create ?(gc=true) x =
    let t = _raw_create ~gc x in
    if gc then Gc.finalise destroy t ;
    t
      
  let it t = t.it
end
  
module Options = struct
  module C = GCBox(struct
    let name = "options_id"
    type _t = options_id
    type args = unit
    let create_no_gc () = rocksdb_options_create()
    let destroy it = rocksdb_options_destroy it
  end)
  include C

  let default = create()
  let unopt = function
    | None -> default
    | Some o -> o
  
  let set_create_if_missing options b =
    rocksdb_options_id_set_create_if_missing options.it b
      
  let get_create_if_missing options =
    rocksdb_options_id_get_create_if_missing options.it
end
  
module DBOptions = struct
  module C = GCBox(struct
    let name = "dboptions_id"
    type _t = dboptions_id
    type args = unit
    let create_no_gc () = rocksdb_dboptions_create()
    let destroy it = rocksdb_dboptions_destroy it
  end)
  include C

  let default = create()
  let unopt = function
    | None -> default
    | Some o -> o

  let set_create_if_missing options b =
    rocksdb_dboptions_id_set_create_if_missing options.it b

  let get_create_if_missing options =
    rocksdb_dboptions_id_get_create_if_missing options.it

  let export options =
    rocksdb_export_dboptions options.it
end

module Cmp = struct
  type _cmpty =
      Bytewise
    | Myrocks
  type cmpty = bool * _cmpty

  let bytewise = rocksdb_bytewise_comparator_create()
  let rev_bytewise = rocksdb_reverse_bytewise_comparator_create()
  let myrocks = rocksdb_myrocks_comparator_create()
  let rev_myrocks = rocksdb_rev_myrocks_comparator_create()

  let create = function
    | false, Bytewise -> bytewise
    | true, Bytewise -> rev_bytewise
    | false, Myrocks -> myrocks
    | true, Myrocks -> rev_myrocks
end

module CFOptions = struct
  module C = GCBox(struct
    let name = "cfoptions_id"
    type _t = cfoptions_id
    type args = unit
    let create_no_gc () = rocksdb_cfoptions_create()
    let destroy it = rocksdb_cfoptions_destroy it
  end)
  include C

  let default = create()
  let unopt = function
    | None -> default
    | Some o -> o

  let set_comparator opts cmp =
    rocksdb_cfoptions_set_comparator opts.it cmp

  let create ?cmp ?(gc=true) () =
    let t = C.create ~gc () in
    do_option (set_comparator t) cmp ;
    t
end

module ROptions = struct
  module C = GCBox(struct
    let name = "roptions_id"
    type _t = roptions_id
    type args = unit
    let create_no_gc () = rocksdb_roptions_create()
    let destroy it = rocksdb_roptions_destroy it
  end)
  include C

  let default = create()
  let unopt = function
    | None -> default
    | Some o -> o
end

module WOptions = struct
  module C = GCBox(struct
    let name = "woptions_id"
    type _t = woptions_id
    type args = unit
    let create_no_gc () = rocksdb_woptions_create()
    let destroy it = rocksdb_woptions_destroy it
  end)
  include C

  let default = create()
  let unopt = function
    | None -> default
    | Some o -> o
end
  
let list_column_families ?opts name =
  let options = DBOptions.unopt opts in
  rocksdb_list_column_families options.it name
  |> status2_to_result
  |> error_to_failure ~msg:"rocksdb_list_column_families"
  |> Array.to_list

module CFH = struct
  type t = Ocaml_rocksdb.Types.cfhandle_id
  let destroy dbh it =
    rocksdb_cfhandle_destroy dbh it
	   |> status_to_result
	   |> error_to_assert_failure
end

module WriteBatch = struct
  module C = GCBox(struct
    let name = "writebatch_id"
    type _t = writebatch_id
    type args = unit
    let create_no_gc () = rocksdb_writebatch_create()
    let destroy it = rocksdb_writebatch_destroy it
  end)
  include C

  let create = C.create

  let put t key value = rocksdb_writebatch_put t.it key value
  let cf_put t cfh key value = rocksdb_writebatch_cf_put t.it cfh key value

  let delete t key = rocksdb_writebatch_delete t.it key
  let cf_delete t cfh key = rocksdb_writebatch_cf_delete t.it cfh key

  let single_delete t key = rocksdb_writebatch_single_delete t.it key
  let cf_single_delete t cfh key = rocksdb_writebatch_cf_single_delete t.it cfh key

  let delete_range t bkey ekey = rocksdb_writebatch_delete_range t.it bkey ekey
  let cf_delete_range t cfh bkey ekey = rocksdb_writebatch_cf_delete_range t.it cfh bkey ekey

end

module Iterator = struct
  module C = GCBox(struct
    let name = "iterator_id"
    type _t = iterator_id
    type args = db_id * ROptions.t option * cfhandle_id option
    let create_no_gc (dbh,ropts, cfh_opt) =
      let readoptions = ROptions.unopt ropts in
      match cfh_opt with
	None ->
	  rocksdb_iterator_create dbh readoptions.ROptions.it
      | Some cfh ->
	  rocksdb_iterator_cf_create dbh readoptions.ROptions.it cfh
    let destroy it = rocksdb_iterator_destroy it
  end)
  include C

  let valid it = rocksdb_iter_valid it.it

  let seek_to_first it = rocksdb_iter_seek_to_first it.it
  let seek_to_last it = rocksdb_iter_seek_to_last it.it
  let seek it k = rocksdb_iter_seek it.it k
  let seek_for_prev it k = rocksdb_iter_seek_for_prev it.it k
  let next it = rocksdb_iter_next it.it
  let prev it = rocksdb_iter_prev it.it
  let key it = rocksdb_iter_key it.it
  let value it = rocksdb_iter_value it.it
  let status it =
    rocksdb_iter_status it.it
    |> status_to_result

  let _forward ~from ~ok it ~f =
    begin
      match from with
	None -> seek_to_first it
      | Some k -> seek it k
    end ;
    if not (valid it) then () else begin
      let rec itrec () =
	let k = key it in
	let v = value it in
	if not(ok k v) then () else begin
	  f k v ;
	  next it ;
	  if not (valid it) then () else itrec ()
	end
      in itrec ()
    end

  let forward ?from ?(ok=(fun _ _ -> true)) it ~f =
    _forward ~from ~ok it ~f

  let _reverse ~from ~ok it ~f =
    begin
      match from with
	None -> seek_to_last it
      | Some k -> seek_for_prev it k
    end ;
    if not (valid it) then () else begin
      let rec itrec () =
	let k = key it in
	let v = value it in
	if not(ok k v) then () else begin
	  f k v ;
	  prev it ;
	  if not (valid it) then () else itrec ()
	end
      in itrec ()
    end

  let reverse ?from ?(ok=(fun _ _ -> true)) it ~f =
    _reverse ~from ~ok it ~f

  let to_list itplan =
    let acc = ref [] in
    let () = itplan ~f:(fun k v -> push acc (k,v)) in
    List.rev !acc

end

module DB0 = struct

  let _opendb_no_gc (readonly, error_if_log_file_exist, opts, name) =
    let options = Options.unopt opts in
    (if readonly then
      rocksdb_open_for_readonly options.it name error_if_log_file_exist
    else
      rocksdb_open options.it name)
    |> status2_to_result |> error_to_failure ~msg:"rocksdb_open"
    |> none_to_failure ~msg:"rocksdb_open"

  let destroy it = rocksdb_db_destroy it

  module C = GCBox(struct
    let name = "simplified db handle"
    type _t = db_id
    type args = bool * bool * Options.t option * string
    let create_no_gc = _opendb_no_gc
    let destroy = destroy
  end)
  include C

  let opendb ?(readonly=false) ?(error_if_log_file_exist=false) ?opts ?(gc=true) name =
    C.create ~gc (readonly, error_if_log_file_exist, opts, name)

  let with_db ?(readonly=false) ?(error_if_log_file_exist=false) ?opts name ~f =
    let dbh = C.create ~gc:false (readonly, error_if_log_file_exist, opts, name) in
    protect ~f:(fun () -> f dbh)
      ~finally:(fun () -> destroy dbh)

  let write dbh ?wopts wb =
    let wopts = WOptions.unopt wopts in
    rocksdb_write dbh.it wopts.WOptions.it wb.WriteBatch.it
    |> status_to_result |> error_to_failure ~msg:"rocksdb_write"

  let get dbh ?ropts key =
    let ropts = ROptions.unopt ropts in
    rocksdb_get dbh.it ropts.ROptions.it key
    |> status2_raise_not_found
    |> status2_to_result |> error_to_failure ~msg:"rocksdb_get"

  let put dbh ?wopts key v =
    let wopts = WOptions.unopt wopts in
    rocksdb_put dbh.it wopts.WOptions.it key v
    |> status_to_result |> error_to_failure ~msg:"rocksdb_put"

  let delete dbh ?wopts key =
    let wopts = WOptions.unopt wopts in
    rocksdb_delete dbh.it wopts.WOptions.it key
    |> status_to_result |> error_to_failure ~msg:"rocksdb_delete"

  let iterator ?(gc=true) ?opts dbh =
    Iterator.C.create ~gc (dbh.it, opts, None)

  let with_iterator ?opts dbh ~f =
    let it = Iterator.C.create ~gc:false (dbh.it, opts, None) in
    protect ~f:(fun () -> f it)
      ~finally:(fun () -> Iterator.destroy it)

end

module DB = struct
  type dbh = {
    dbh : db_id ;
    cfhs : (string, cfhandle_id) Hashtbl.t ;
  }

  let _opendb_no_gc (readonly, error_if_log_file_exist, opts, cfopts, cfds, name) =
    let options = DBOptions.unopt opts in
    let cfoptions = CFOptions.unopt cfopts in
    let cfds =
      match cfds with Some a -> a
      | None -> begin
	try
	  list_column_families ~opts:options name
	with Failure _ -> []
      end
      |>  List.map (fun n -> (n, cfoptions)) in
    let cfds = List.map (fun (n, o) -> (n, o.CFOptions.it)) cfds in
    (if readonly then
	rocksdb_open_column_families_for_readonly options.it name (Array.of_list  cfds) error_if_log_file_exist
     else
	rocksdb_open_column_families options.it name (Array.of_list  cfds))
    |> status3_to_result |> error_to_failure ~msg:"rocksdb_open_column_families"
    |> (function cfds, None -> failwith "rocksdb_open_column_families: OK status, but no dbh!"
      | (cfhs, Some dbh) ->
	 let cfhs_tbl = Hashtbl.create 5 in
	 List.iter2 (fun (n, _) cfh -> Hashtbl.add cfhs_tbl n cfh)
	   cfds (Array.to_list cfhs) ;
	 {cfhs = cfhs_tbl ; dbh})

  let destroy it =
    let destroy1 _ cfh =
      CFH.destroy it.dbh cfh in
    Hashtbl.iter destroy1 it.cfhs ;
    rocksdb_db_destroy it.dbh

  module C = GCBox(struct
    let name = "full db handle"
    type _t = dbh
    type args = bool * bool * DBOptions.t option * CFOptions.t option * (string * CFOptions.t) list option * string
    let create_no_gc = _opendb_no_gc
    let destroy = destroy
  end)
  include C
  let opendb ?(readonly=false) ?(error_if_log_file_exist=false) ?opts ?cfopts ?cfds ?(gc=true) name =
    C.create ~gc (readonly, error_if_log_file_exist, opts, cfopts, cfds, name)

  let with_db ?(readonly=false) ?(error_if_log_file_exist=false) ?opts ?cfopts ?cfds name ~f =
    let dbh = C.create ~gc:false (readonly, error_if_log_file_exist, opts, cfopts, cfds, name) in
    protect ~f:(fun () -> f dbh)
      ~finally:(fun () -> destroy dbh)

  let cfh dbh cfname = Hashtbl.find dbh.it.cfhs cfname

  let _create_cf dbh opts cfname =
    let opts = CFOptions.unopt opts in
    rocksdb_create_column_family dbh.it.dbh opts.CFOptions.it cfname
    |> status2_to_result
    |> error_to_failure ~msg:"rocksdb_create_column_family"

  let create_cf dbh ?opts cfname =
    let cfh = _create_cf dbh opts cfname in
    Hashtbl.add dbh.it.cfhs cfname cfh
  let _drop_cf dbh cfh =
    rocksdb_drop_column_family dbh.it.dbh cfh
    |> status_to_result
    |> error_to_failure ~msg:"rocksdb_drop_column_family"

  let drop_cf dbh cfname =
    let cfh = cfh dbh cfname in
    _drop_cf dbh cfh ;
    Hashtbl.remove dbh.it.cfhs cfname

  let write dbh ?wopts wb =
    let wopts = WOptions.unopt wopts in
    rocksdb_write dbh.it.dbh wopts.WOptions.it wb.WriteBatch.it
    |> status_to_result |> error_to_failure ~msg:"rocksdb_write"

  let get dbh ?ropts key =
    let ropts = ROptions.unopt ropts in
    rocksdb_get dbh.it.dbh ropts.ROptions.it key
    |> status2_raise_not_found
    |> status2_to_result |> error_to_failure ~msg:"rocksdb_get"

  let cf_get dbh ?ropts ~cfname key =
    let ropts = ROptions.unopt ropts in
    let cfh = cfh dbh cfname in
    rocksdb_cf_get dbh.it.dbh ropts.ROptions.it cfh key
    |> status2_raise_not_found
    |> status2_to_result |> error_to_failure ~msg:"rocksdb_cf_get"

  let put dbh ?wopts key v =
    let wopts = WOptions.unopt wopts in
    rocksdb_put dbh.it.dbh wopts.WOptions.it key v
    |> status_to_result |> error_to_failure ~msg:"rocksdb_put"

  let cf_put dbh ?wopts ~cfname key v =
    let wopts = WOptions.unopt wopts in
    let cfh = cfh dbh cfname in
    rocksdb_cf_put dbh.it.dbh wopts.WOptions.it cfh key v
    |> status_to_result |> error_to_failure ~msg:"rocksdb_cf_put"

  let delete dbh ?wopts key =
    let wopts = WOptions.unopt wopts in
    rocksdb_delete dbh.it.dbh wopts.WOptions.it key
    |> status_to_result |> error_to_failure ~msg:"rocksdb_delete"

  let cf_delete dbh ?wopts ~cfname key =
    let wopts = WOptions.unopt wopts in
    let cfh = cfh dbh cfname in
    rocksdb_cf_delete dbh.it.dbh wopts.WOptions.it cfh key
    |> status_to_result |> error_to_failure ~msg:"rocksdb_cf_delete"

  let iterator ?(gc=true) ?opts ?cfname dbh =
    let cfname = match cfname with None -> __default_cfname | Some n -> n in
    Iterator.C.create ~gc (dbh.it.dbh, opts, Some (cfh dbh cfname))

  let with_iterator ?opts ?cfname dbh ~f =
    let cfname = match cfname with None -> __default_cfname | Some n -> n in
    let it = Iterator.C.create ~gc:false (dbh.it.dbh, opts, Some (cfh dbh cfname)) in
    protect ~f:(fun () -> f it)
      ~finally:(fun () -> Iterator.destroy it)
end
