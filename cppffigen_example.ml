open Pa_ppx_located_sexp
open Cppffigen

let t = {
  stanzas = [
  CPP(PROLOGUE,"
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
") ;

  ML(PROLOGUE,"
type status_t = {code : int ; subcode : int ; msg : string option }
let print_status st =
  Printf.printf \"<%d, %d, %s>\\n\" st.code st.subcode
    (match st.msg with None -> \"<>\"
    | Some s -> Printf.sprintf \"\\\"%s\\\"\" (String.escaped s)) ;;
") ;
    STRUCT Struct.{
      modname = "Triple" ;
      name = "triple" ;
      members = [
	(PRIM INT, "a") ;
	(ID (CPPID.mk "std::string"), "b") ;
	(TYCON("std::vector", [PRIM BOOL]), "c") ;
      ];
      code = ""
    };
    TYPEDEF{
      name="status";
      cpptype=ID(CPPID.mk "rocksdb::Status");
      mltype=MLTYPE.CONCRETE (OTHER (MLID.mk "status_t"));
    };
    TYPEDEF{
      name="cfhandle_id";
      cpptype=PTR(ID(CPPID.mk "rocksdb::ColumnFamilyHandle")) ;
      mltype= MLTYPE.ABSTRACT "cfhandle_id";
    } ;
    CPP2ML(ID(CPPID.mk "cfhandle_id"),
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID(CPPID.mk "cfhandle_id"),
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);");

    TYPEDEF{
      name="db_id";
      cpptype=PTR(ID(CPPID.mk "rocksdb::DB")) ;
      mltype= MLTYPE.ABSTRACT "db_id";
    } ;
    CPP2ML(ID(CPPID.mk "db_id"),
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID(CPPID.mk "db_id"),
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    TYPEDEF{
      name="dboptions_id";
      cpptype=PTR(ID(CPPID.mk "rocksdb::DBOptions")) ;
      mltype= MLTYPE.ABSTRACT "dboptions_id";
    } ;
    CPP2ML(ID(CPPID.mk "dboptions_id"),
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID(CPPID.mk "dboptions_id"),
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    ATTRIBUTE Attribute.{
      target = "dbooptions_id" ;
      aname = "create_if_missing" ;
      fprefix = "rocksdb_" ;
      cpptype = ID (CPPID.mk "bool") ;
    } ;
    TYPEDEF{
      name="cfoptions_id";
      cpptype=PTR(ID(CPPID.mk "rocksdb::ColumnFamilyOptions")) ;
      mltype= MLTYPE.ABSTRACT "cfoptions_id";
    } ;
    CPP2ML(ID(CPPID.mk "cfoptions_id"),
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID(CPPID.mk "cfoptions_id"),
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    TYPEDEF{
      name="cfdescriptor_id";
      cpptype=PTR(ID(CPPID.mk "rocksdb::ColumnFamilyDescriptor")) ;
      mltype= MLTYPE.ABSTRACT "cfdescriptor_id";
    } ;
    CPP2ML(ID(CPPID.mk "cfdescriptor_id"),
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID(CPPID.mk "cfdescriptor_id"),
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    TYPEDEF{
      name="comparator_id";
      cpptype=PTR(ID(CPPID.mk "rocksdb::Comparator")) ;
      mltype=MLTYPE.ABSTRACT "comparator_id";
    } ;
    CPP2ML(ID(CPPID.mk "comparator_id"),
	   "_mlvalue = c2ml_owned_pointer(_cvalue);") ;
    ML2CPP(ID(CPPID.mk "comparator_id"),
	   "ml2c_owned_pointer(_mlvalue, _cvaluep);") ;

    ML2CPP(ID(CPPID.mk "rocksdb::ColumnFamilyDescriptor"),
	   "cfdescriptor_id cfd_id ;
  ml2c(_mlvalue, &cfd_id) ;
  *_cvaluep = *cfd_id ;") ;

    CPP(HERE, "
std::string demarsh_state(const char *state) {
  assert(NULL != state) ;
  uint32_t size;
  memcpy(&size, state, sizeof(size));
  const char* body = &(state[4]);
  return std::string(body, size) ;
}
") ;
    CPP2ML(TYCON("Opt",[PRIM CHAR]),
	   "
  assert(NULL != _cvalue.it) ;
  _mlvalue = c2ml(demarsh_state(_cvalue.it)) ;
") ;
    CPP2ML(ID(CPPID.mk "rocksdb::Status"),
	   "
  OptWrap<char> w(_cvalue.getState()) ;
  _mlvalue = c2ml(_cvalue.code(), _cvalue.subcode(), w.p()) ;
") ;

    FOREIGN([ID(CPPID.mk "cfoptions_id")],
	    "rocksdb_cfoptions_create",
	    [],
	    "
  _res0 = new rocksdb::ColumnFamilyOptions() ;
") ;

    FOREIGN([],
	    "rocksdb_cfoptions_destroy",
	    [ID(CPPID.mk "cfoptions_id"),"opth"],
	    "
    delete opth ;
") ;

    FOREIGN([ID(CPPID.mk "dboptions_id")],
	    "rocksdb_dboptions_create",
	    [],
	    "
  _res0 = new rocksdb::DBOptions() ;
") ;

    FOREIGN([],
	    "rocksdb_dboptions_destroy",
	    [ID(CPPID.mk "dboptions_id"),"opth"],
	    "
    delete opth ;
") ;

    FOREIGN([ID(CPPID.mk "status"); TYCON("std::vector", [ID(CPPID.mk "std::string")])],
	    "rocksdb_list_column_families",
	    [ID(CPPID.mk "dboptions_id"), "opth";
	     ID(CPPID.mk "std::string"),"name"],
	    "
  _res0 = rocksdb::DB::ListColumnFamilies(*opth, name, &_res1);
  if (!_res0.ok()) _res1.clear() ;
") ;
  ]
}

let _ =
  Printexc.catch (fun () ->  
    if Sys.argv.(1) = "-dump-sexp" then
      Fmt.(pf stdout "%a@." Sexp.pp_hum (located_sexp_of_t t))
    else failwith "unrecognized argument")
    ()
