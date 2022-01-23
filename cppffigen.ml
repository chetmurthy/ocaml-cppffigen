open Sexplib
open Sexplib.Std

let third3 (a,b,c)  = c
let push l x = (l := x :: !l)

let version = "0.002"

type primtype =
  | INT | UINT
  | INT64 | UINT64
  | INT32 | UINT32
  | CHAR | UCHAR
  | BOOL
      [@@deriving sexp]

type cpptype =
    PTR of cpptype
  | ID of string
  | TYCON of string * cpptype list
  | PRIM of primtype [@@deriving sexp]

type mltype =
  | GEN of string
  | EXP of string [@@deriving sexp]

type def_t =
  { name : string ;
    mltype : mltype ;
    cpptype : cpptype ;
  } [@@deriving sexp]

module Attribute = struct
type t =
  {
    target : string ;
    aname : string ;
    fprefix : string ;
    cpptype : cpptype ;
  } [@@deriving sexp]
end

module Struct = struct
  type t =
    {
      modname : string ;
      name : string ;
      members : (cpptype * string) list ;
    } [@@deriving sexp]
end

type loc = PROLOGUE | EPILOGUE | HERE [@@deriving sexp]

type stanza_t =
  | TYPEDEF of def_t
  | STRUCT of Struct.t
  | ATTRIBUTE of Attribute.t
  | CPP2ML of cpptype * string
  | ML2CPP of cpptype * string
  | CPP of loc * string
  | ML of loc * string
  | MLI of loc * string
  | FOREIGN of cpptype list * string * (cpptype * string) list * string [@@deriving sexp]

let expand_attribute {Attribute.target ; aname ; fprefix ; cpptype } =
    [FOREIGN([], Printf.sprintf "%s%s_set_%s" fprefix target aname,
	     [(ID target), "rcvr"; cpptype, aname],
	     Printf.sprintf "rcvr->%s = %s;" aname aname) ;
     FOREIGN([cpptype], Printf.sprintf "%s%s_get_%s" fprefix target aname,
	     [(ID target), "rcvr"], Printf.sprintf "_res0 = rcvr->%s;" aname)
  ]

let prim2mltype = function
  | (INT | UINT) -> "int"
  | (INT64 | UINT64) ->"int64"
  | (INT32 | UINT32) -> "int32"
  | (CHAR | UCHAR) -> "char"
  | BOOL -> "bool"

let ctype2mltype tmap cty =
  let rec crec = function
    | PRIM t -> prim2mltype t
    | ID "std::string" -> "string"
    | ID s -> begin
      if not (List.mem_assoc s tmap) then
	failwith (Printf.sprintf "typename %s not found in map" s) ;
      match List.assoc s tmap with
      | GEN s -> s
      | EXP s -> "("^s^")"
    end
    | TYCON("std::vector",[cty]) -> Printf.sprintf "(%s array)" (crec cty)
    | TYCON("std::tuple",[a;b]) -> Printf.sprintf "(%s * %s)"  (crec a) (crec b)
    | TYCON("std::tuple",l) ->
       Printf.sprintf "(%s)" (String.concat " * " (List.map crec l))
    | PTR (TYCON("Opt",[cty])) -> Printf.sprintf "(%s option)" (crec cty)
    | TYCON _ -> failwith "unrecognized C++ type-constructor"
    | PTR _ -> failwith "cannot map a PTR type to an ML type (should use typedef)"
  in  crec cty

let fmt_primcpptype = function
  | INT -> "int"
  | UINT -> "unsigned int"
  | INT64 -> "int64_t"
  | UINT64 -> "uint64_t"
  | INT32 -> "int32_t"
  | UINT32 -> "uint32_t"
  | CHAR -> "char"
  | UCHAR -> "unsigned char"
  | BOOL -> "bool"
  
let fmt_cpptype ty =
  let rec frec = function
    | ID s -> s
    | PTR t -> Printf.sprintf "%s*" (frec t)
    | TYCON (id, l) ->
       Printf.sprintf "%s< %s >" id (String.concat ", " (List.map frec l))
    | PRIM t -> fmt_primcpptype t
  in frec ty

let expand_struct tmap { Struct.modname; name ; members } =
  [
    ML(PROLOGUE,
       Printf.sprintf "
module %s = struct
  type t = { %s\n}
end
"
	 modname
	 (String.concat ""
	    (List.map (fun (cty,n) -> Printf.sprintf "\n    %s : %s ;" n (ctype2mltype tmap cty)) members))) ;
    MLI(PROLOGUE,
       Printf.sprintf "
module %s : sig
  type t = { %s\n}
end
"
	 modname
	 (String.concat ""
	    (List.map (fun (cty,n) -> Printf.sprintf "\n    %s : %s ;" n (ctype2mltype tmap cty)) members))) ;
    CPP(PROLOGUE,
	Printf.sprintf "
#ifndef %s_t_DEFINED
#define %s_t_DEFINED
struct %s_t {\n%s} ;
#endif
"
	  name name
	  name
	  (String.concat ""
	     (List.map (fun (cty, n) -> Printf.sprintf "  %s %s ;\n" (fmt_cpptype cty) n) members))) ;
    TYPEDEF {
      name ;
      cpptype = ID(Printf.sprintf "struct %s_t" name) ;
      mltype = EXP(Printf.sprintf "%s.t" modname) ;
    } ;
    ML2CPP(ID name,
	   String.concat "\n  "
	     (List.mapi (fun i (cty, n) ->
	       Printf.sprintf "ml2c(Field(_mlvalue,%d), &(_cvaluep->%s));" i n) members)) ;
    CPP2ML(ID name,
	   Printf.sprintf "
  _mlvalue = caml_alloc(%d, 0) ;
%s
"
	     (List.length members)
	     (String.concat "\n"
		(List.mapi (fun i (cty, n) ->
		  Printf.sprintf "  Store_field(_mlvalue, %d, c2ml(_cvalue.%s));" i n)
		   members))) ;
  ]

type t = {
  stanzas : stanza_t  list;
} [@@deriving sexp]

module CPP = struct

let prologues t =
  List.concat (List.map (function
  | CPP(PROLOGUE, s) -> [s]
  | _ -> []) t.stanzas)

let epilogues t =
  List.concat (List.map (function
  | CPP(EPILOGUE, s) -> [s]
  | _ -> []) t.stanzas)

let gen_stanza_forwards oc = function
  | (CPP _| ML _ | MLI _| FOREIGN _) -> ()
  | TYPEDEF t ->
     Printf.fprintf oc "typedef %s %s;\n" (fmt_cpptype t.cpptype) t.name ;
  | CPP2ML(cty, _) ->
     Printf.fprintf oc "value c2ml(const %s& _cvalue);\n" (fmt_cpptype cty)
  | ML2CPP(cty, _) ->
     Printf.fprintf oc "void ml2c(const value _mlvalue, %s *_cvaluep);\n" (fmt_cpptype cty)

let gen_stanza_bodies oc = function
  | (ML _ | MLI _| TYPEDEF _) -> ()
  | CPP(HERE, s) -> output_string oc s
  | CPP _  -> ()
  | CPP2ML(cty, body) ->
     Printf.fprintf oc "value c2ml(const %s& _cvalue) {
  CAMLparam0();
  CAMLlocal1(_mlvalue);
  %s ;
  CAMLreturn(_mlvalue);
}
" (fmt_cpptype cty) body
  | ML2CPP(cty, body) ->
     Printf.fprintf oc "void ml2c(const value _mlvalue, %s *_cvaluep) {
  %s ;
}
"
       (fmt_cpptype cty) body
  | FOREIGN(rtys, fname, argformals, body) ->
     let args = List.map (fun (cty,cid) ->
       (cty, cid, Printf.sprintf "_mlv_%s" cid)
     ) argformals in
     Printf.fprintf oc
"extern \"C\" value %s(%s) {
  CAMLparam%d(%s);
  CAMLlocal1(_mlv_res) ;
  /* ML->C*/
  %s
  %s
  /* BODY */
  %s
  /* C->ML*/
  %s
  CAMLreturn(_mlv_res) ;
}
"
     fname
  (String.concat ", " (List.map (fun (_,_,mlid) -> (Printf.sprintf "value %s" mlid)) args))
  (List.length argformals)
  (String.concat ", " (List.map third3 args))
  (* ML->C *)
  (String.concat "\n  " (List.map (fun (cty, cid, mlid) ->
    Printf.sprintf "%s %s;\n  ml2c(%s, &%s);" (fmt_cpptype cty) cid mlid cid) args))
  (match rtys with [] -> "" | ctys ->
    String.concat "\n  " (List.mapi (fun i cty ->
      Printf.sprintf "%s _res%d;" (fmt_cpptype cty) i)
			 ctys))
  (* BODY *)
  body
  (* C->ML *)
  (match rtys with [] -> "" | l ->
    Printf.sprintf "  _mlv_res = c2ml(%s);"
      (String.concat ", " (List.mapi (fun i cty ->
	Printf.sprintf "_res%d" i)
			     l)))

let gen oc t =
output_string oc "
#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
" ;
  List.iter (output_string oc) (prologues t) ;
  List.iter (gen_stanza_forwards oc) t.stanzas ;
  output_string oc "
#include \"cppffi.inc\"
";
  List.iter(fun s ->
    gen_stanza_bodies oc s;
    output_string oc "\n") t.stanzas ;
  List.iter (output_string oc) (epilogues t) ;
  ()
end

module ML = struct

let prologues t =
  List.concat (List.map (function
  | ML(PROLOGUE, s) -> [s]
  | _ -> []) t.stanzas)

let epilogues t =
  List.concat (List.map (function
  | ML(EPILOGUE, s) -> [s]
  | _ -> []) t.stanzas)

let setup_typedecls t =
  let tmap = ref [] in
  List.iter (function
  | TYPEDEF t ->
     if List.mem_assoc t.name !tmap then
       failwith (Printf.sprintf "typenae %s already previously typedef-ed" t.name) ;
    push tmap (t.name, t.mltype)
  | STRUCT t ->
     if List.mem_assoc t.Struct.name !tmap then
       failwith (Printf.sprintf "struct name %s already previously typedef-ed" t.Struct.name) ;
    push tmap (t.Struct.name, EXP (Printf.sprintf "%s.t" t.Struct.modname))
  | _ -> ()
  ) t.stanzas ;
  !tmap

let gen_typedecls ~ml oc tmap =
  let l = tmap in
  Printf.fprintf oc (if ml then "module Types = struct\n" else "module Types : sig\n");
  Printf.fprintf oc "type %s\n"
    (String.concat "\nand " (List.map (function
    | (id, EXP s) -> Printf.sprintf "%s = %s" id s
    | (id, GEN s) ->  s
     ) l)) ;
  Printf.fprintf oc "end\n" ;
  ()

let gen_stanza tmap oc = function
  | (CPP _|CPP2ML _|ML2CPP _|MLI _) -> ()
  | TYPEDEF _ -> ()
  | ML(HERE, s) -> output_string oc s
  | ML _ -> ()
  | FOREIGN(rtys, name, argformals, _) ->
     Printf.fprintf oc
       "external %s : %s -> %s\n\t=\"%s\"\n"
       name
       (match argformals with
       | [] -> "unit"
       | l -> String.concat " -> "
	  (List.map (fun (cty, _) -> ctype2mltype tmap cty) l))
       (match rtys with
	 [] -> "unit"
       | l -> String.concat " * "
	  (List.map (ctype2mltype tmap) l))
       name

let gen tmap oc t =
  List.iter (output_string oc) (prologues t) ;
  gen_typedecls ~ml:true oc tmap ;
  Printf.fprintf oc "open Types\n" ;
  List.iter (gen_stanza tmap oc) t.stanzas ;
  List.iter (output_string oc) (epilogues t) ;
  ()
end

module MLI = struct
let prologues t =
  List.concat (List.map (function
  | MLI(PROLOGUE, s) -> [s]
  | _ -> []) t.stanzas)

let epilogues t =
  List.concat (List.map (function
  | MLI(EPILOGUE, s) -> [s]
  | _ -> []) t.stanzas)

let gen_typedecls = ML.gen_typedecls ~ml:false
let gen_stanza = ML.gen_stanza
    
  let gen tmap oc t =
  List.iter (output_string oc) (prologues t) ;
  gen_typedecls oc tmap ;
  Printf.fprintf oc "open Types\n" ;
  List.iter (gen_stanza tmap oc) t.stanzas ;
  List.iter (output_string oc) (epilogues t) ;
  ()

end
