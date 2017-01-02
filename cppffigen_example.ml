
open Cppffigen

let t = {
  cpp_prologue = "
#include <stdio.h>
#include <string>
#include <unistd.h>
#include <ostream>
#include <iostream>
#include <vector>

#include \"rocksdb/comparator.h\"
#include \"rocksdb/db.h\"
#include \"ocaml_rocksdb.inc\"
#include \"cppffi.h\"
" ;

  cpp_epilogue = "" ;
  ml_prologue = "
type status_t = {code : int ; subcode : int ; msg : string option }
let print_status st =
  Printf.printf \"<%d, %d, %s>\\n\" st.code st.subcode
    (match st.msg with None -> \"<>\"
    | Some s -> Printf.sprintf \"\\\"%s\\\"\" (String.escaped s)) ;;
" ;
  ml_epilogue = "" ;
  mli_prologue = "" ;
  mli_epilogue = "" ;
  stanzas = [
    TYPEDEF{
      name="status";
      cpptype=ID"rocksdb::Status";
      mltype=EXP"status_t";
    };
    TYPEDEF{
      name="cfhandle_id";
      cpptype=PTR(ID"rocksdb::ColumnFamilyHandle") ;
      mltype=GEN "cfhandle_id";
    } ;
    CPP2ML(ID"cfhandle_id",
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID"cfhandle_id",
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);");

    TYPEDEF{
      name="db_id";
      cpptype=PTR(ID"rocksdb::DB") ;
      mltype=GEN "db_id";
    } ;
    CPP2ML(ID"db_id",
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID"db_id",
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    TYPEDEF{
      name="dboptions_id";
      cpptype=PTR(ID"rocksdb::DBOptions") ;
      mltype=GEN "dboptions_id";
    } ;
    CPP2ML(ID"dboptions_id",
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID"dboptions_id",
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    ATTRIBUTE Attribute.{
      target = "dbooptions_id" ;
      aname = "create_if_missing" ;
      fprefix = "rocksdb_" ;
      cpptype = ID "bool" ;
    } ;
    TYPEDEF{
      name="cfoptions_id";
      cpptype=PTR(ID"rocksdb::ColumnFamilyOptions") ;
      mltype=GEN "cfoptions_id";
    } ;
    CPP2ML(ID"cfoptions_id",
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID"cfoptions_id",
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    TYPEDEF{
      name="cfdescriptor_id";
      cpptype=PTR(ID"rocksdb::ColumnFamilyDescriptor") ;
      mltype=GEN "cfdescriptor_id";
    } ;
    CPP2ML(ID"cfdescriptor_id",
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID"cfdescriptor_id",
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    TYPEDEF{
      name="comparator_id";
      cpptype=PTR(ID"rocksdb::Comparator") ;
      mltype=GEN "comparator_id";
    } ;
    CPP2ML(ID"comparator_id",
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID"comparator_id",
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    ML2CPP(ID"rocksdb::ColumnFamilyDescriptor",
	   "cfdescriptor_id cfd_id ;
  ml2c(_mlvalue, &cfd_id) ;
  *_cvaluep = *cfd_id ;") ;

    CPP "
std::string demarsh_state(const char *state) {
  assert(NULL != state) ;
  uint32_t size;
  memcpy(&size, state, sizeof(size));
  const char* body = &(state[4]);
  return std::string(body, size) ;
}
" ;
    CPP2ML(TYCON("Opt",[PRIM CHAR]),
	   "
  assert(NULL != _cvalue.it) ;
  _mlvalue = c2ml(demarsh_state(_cvalue.it)) ;
") ;
    CPP2ML(ID"rocksdb::Status",
	   "
  OptWrap<char> w(_cvalue.getState()) ;
  _mlvalue = c2ml(_cvalue.code(), _cvalue.subcode(), w.p()) ;
") ;

    FOREIGN([ID"cfoptions_id"],
	    "rocksdb_cfoptions_create",
	    [],
	    "
  _res0 = new rocksdb::ColumnFamilyOptions() ;
") ;

    FOREIGN([],
	    "rocksdb_cfoptions_destroy",
	    [ID"cfoptions_id","opth"],
	    "
    delete opth ;
") ;

    FOREIGN([ID"dboptions_id"],
	    "rocksdb_dboptions_create",
	    [],
	    "
  _res0 = new rocksdb::DBOptions() ;
") ;

    FOREIGN([],
	    "rocksdb_dboptions_destroy",
	    [ID"dboptions_id","opth"],
	    "
    delete opth ;
") ;

    FOREIGN([ID"status"; TYCON("std::vector", [ID"std::string"])],
	    "rocksdb_list_column_families",
	    [ID"dboptions_id", "opth";
	     ID"std::string","name"],
	    "
  _res0 = rocksdb::DB::ListColumnFamilies(*opth, name, &_res1);
  if (!_res0.ok()) _res1.clear() ;
") ;

  ]
}

let _ =
  Printexc.catch (fun () ->  
    if Sys.argv.(1) = "-cpp" then 
      CPP.gen stdout (expand_attributes t)
    else if Sys.argv.(1) = "-ml" then
      ML.gen stdout (expand_attributes t)
    else if Sys.argv.(1) = "-dump-sexp" then
      Sexplib.Sexp.output_hum stdout(sexp_of_t t)
    else failwith "unrecognized argument")
    ()
