open Sexplib
open Sexplib.Std

let fst3 (a,b,c)  = a
let snd3 (a,b,c)  = b
let third3 (a,b,c)  = c
let push l x = (l := x :: !l)

let version = "0.003"

module CPPTYPE = struct
type primtype =
  | INT | UINT
  | INT64 | UINT64
  | INT32 | UINT32
  | CHAR | UCHAR
  | BOOL
  | STRING
      [@@deriving sexp]

type t =
    PTR of t
  | ID of string
  | TYCON of string * t list
  | PRIM of primtype [@@deriving sexp]
end

module MLTYPE = struct
  type concrete_type =
    INT
  | INT32
  | INT64
  | CHAR
  | BOOL
  | NATIVEINT
  | STRING
  | ARRAY of concrete_type
  | TUPLE of concrete_type list
  | OPTION of concrete_type
  | OTHER of string [@@deriving sexp]

  let rec concrete_to_mlstring cty =
    let rec crec = function
      INT -> "int"
    | INT32 -> "int32"
    | INT64 -> "int64"
    | CHAR -> "char"
    | BOOL -> "bool"
    | NATIVEINT -> "nativeint"
    | STRING -> "string"
    | ARRAY ty  -> Printf.sprintf "(%s array)" (crec ty)
    | TUPLE l  -> Printf.sprintf "(%s)" (String.concat " * " (List.map crec l))
    | OPTION ty  -> Printf.sprintf "(%s option)" (crec ty)
    | OTHER s -> Printf.sprintf "(%s)" s
    in crec cty

type t =
  | ABSTRACT of string
  | CONCRETE of concrete_type [@@deriving sexp]

let to_mlstring = function
    ABSTRACT s -> s
  | CONCRETE cty -> concrete_to_mlstring cty

end

type def_t =
  { name : string ;
    mltype : MLTYPE.t ;
    cpptype : CPPTYPE.t ;
  } [@@deriving sexp]

module Attribute = struct
type t =
  {
    target : string ;
    aname : string ;
    fprefix : string ;
    cpptype : CPPTYPE.t ;
  } [@@deriving sexp]
end

module Struct = struct
  type t =
    {
      modname : string ;
      name : string ;
      members : (CPPTYPE.t * string) list ;
    } [@@deriving sexp]
end

type loc = PROLOGUE | EPILOGUE | HERE [@@deriving sexp]

type stanza_t =
  | TYPEDEF of def_t
  | STRUCT of Struct.t
  | ATTRIBUTE of Attribute.t
  | CPP2ML of CPPTYPE.t * string
  | ML2CPP of CPPTYPE.t * string
  | CPP of loc * string
  | ML of loc * string
  | MLI of loc * string
  | FOREIGN of CPPTYPE.t list * string * (CPPTYPE.t * string) list * string [@@deriving sexp]

let expand_attribute {Attribute.target ; aname ; fprefix ; cpptype } =
    [FOREIGN([], Printf.sprintf "%s%s_set_%s" fprefix target aname,
	     [(ID target), "rcvr"; cpptype, aname],
	     Printf.sprintf "rcvr->%s = %s;" aname aname) ;
     FOREIGN([cpptype], Printf.sprintf "%s%s_get_%s" fprefix target aname,
	     [(ID target), "rcvr"], Printf.sprintf "_res0 = rcvr->%s;" aname)
  ]

let prim2mltype = function
  | (CPPTYPE.INT | UINT) -> MLTYPE. INT
  | (INT64 | UINT64) -> MLTYPE.INT64
  | (INT32 | UINT32) -> MLTYPE.INT32
  | (CHAR | UCHAR) -> MLTYPE.CHAR
  | BOOL -> MLTYPE.BOOL
module TMAP = struct
  type entry_t = {
      stanza : stanza_t
    ; id : string
    ; cpptype : CPPTYPE.t
    ; mltype : MLTYPE.t
    ; concretetype : MLTYPE.concrete_type
    }
  type t = (string * entry_t) list

  let typedef_to_entry tmap = function
      (TYPEDEF t) as stanza ->
       if List.mem_assoc t.name tmap then
         failwith (Printf.sprintf "typename %s already previously typedef-ed" t.name) ;
       { id = t.name
       ; cpptype = t.cpptype
       ; mltype = t.mltype
       ; stanza
       ; concretetype =
           match t.mltype with
             MLTYPE.CONCRETE t -> t
           | ABSTRACT s -> MLTYPE.OTHER s
       }

  let struct_to_entry tmap = function
      (STRUCT t) as stanza ->
       if List.mem_assoc t.Struct.name tmap then
         failwith (Printf.sprintf "struct name %s already previously typedef-ed" t.Struct.name) ;
       let concretetype = MLTYPE.OTHER (Printf.sprintf "%s.t" t.Struct.modname) in
       {
         id = t.Struct.name
       ; cpptype = CPPTYPE.ID t.Struct.name
       ; mltype = MLTYPE.CONCRETE concretetype
       ; concretetype
       ; stanza
       }

  let mk t =
    let rec mkrec tmap = function
        [] -> tmap
      | h::t ->
         match h with
           STRUCT _ ->
            let e = struct_to_entry tmap h in
            mkrec ((e.id, e)::tmap) t
         | TYPEDEF _ ->
            let e = typedef_to_entry tmap h in
            mkrec ((e.id, e)::tmap) t
         | _ -> mkrec tmap t
    in
    mkrec [] t

  let lookup tmap id =
    match List.assoc id tmap with
      e -> e
    | exception Not_found ->
       failwith (Printf.sprintf "id %s not found in type-map" id)

end

let ctype2concretetype tmap cty : MLTYPE.concrete_type =
  let rec crec = function
    | CPPTYPE.PRIM t -> prim2mltype t
    | ID "std::string" -> MLTYPE.STRING
    | ID s -> begin
      if not (List.mem_assoc s tmap) then
	failwith (Printf.sprintf "typename %s not found in map" s) ;
      (List.assoc s tmap).TMAP.concretetype
    end
    | TYCON("std::vector",[cty]) -> MLTYPE.(ARRAY (crec cty))
    | TYCON("std::tuple",[a;b]) -> MLTYPE.(TUPLE [crec a; crec b])
    | TYCON("std::tuple",l) -> MLTYPE.(TUPLE (List.map crec l))
    | TYCON("std::optional",[cty]) -> MLTYPE.(OPTION (crec cty))
    | TYCON _ -> failwith "unrecognized C++ type-constructor"
    | PTR _ -> failwith "cannot map a PTR type to an ML type (should use typedef)"
  in  crec cty

let fmt_primcpptype = function
  | CPPTYPE.INT -> "int"
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
    | CPPTYPE.ID s -> s
    | PTR t -> Printf.sprintf "%s*" (frec t)
    | TYCON (id, l) ->
       Printf.sprintf "%s< %s >" id (String.concat ", " (List.map frec l))
    | PRIM t -> fmt_primcpptype t
  in frec ty

let concretetype_to_sentineltype tmap mlty =
  let rec convrec = function
    MLTYPE.INT -> "sentinel_INT"
  | INT32 -> "sentinel_INT32"
  | INT64 -> "sentinel_INT64"
  | CHAR -> "sentinel_INT"
  | BOOL -> "sentinel_INT"
  | NATIVEINT -> "sentinel_NATIVEINT"
  | STRING -> "sentinel_GENERIC"
  | ARRAY ty -> Printf.sprintf "sentinel_ARRAY<%s>" (convrec ty)
  | TUPLE [t1;t2] -> Printf.sprintf "sentinel_TUPLE2<%s,%s>" (convrec t1) (convrec t2)
  | TUPLE [t1;t2;t3] -> Printf.sprintf "sentinel_TUPLE3<%s,%s,%s>" (convrec t1) (convrec t2) (convrec t3)
  | TUPLE _ -> failwith "mltype_to_sentineltype(tuple length > 2): unimplemented"
  | OPTION ty -> Printf.sprintf "sentinel_OPTION<%s>" (convrec ty)
  | OTHER id -> convrec (TMAP.lookup tmap id).TMAP.concretetype
  in
  convrec mlty

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
	    (List.map (fun (cty,n) -> Printf.sprintf "\n    %s : %s ;" n (MLTYPE.concrete_to_mlstring (ctype2concretetype tmap cty))) members))) ;
    MLI(PROLOGUE,
       Printf.sprintf "
module %s : sig
  type t = { %s\n}
end
"
	 modname
	 (String.concat ""
	    (List.map (fun (cty,n) -> Printf.sprintf "\n    %s : %s ;" n (MLTYPE.concrete_to_mlstring (ctype2concretetype tmap cty))) members))) ;
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
      mltype = CONCRETE(OTHER (Printf.sprintf "%s.t" modname)) ;
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

let gen_stanza_forwards tmap oc = function
  | (CPP _| ML _ | MLI _| FOREIGN _) -> ()
  | TYPEDEF t ->
     Printf.fprintf oc "typedef %s %s;\n" (fmt_cpptype t.cpptype) t.name ;
  | CPP2ML(cty, _) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
     Printf.fprintf oc "value c2ml(const %s& _s0, const %s& _cvalue);\n"
       sentinel_type
       (fmt_cpptype cty)
  | ML2CPP(cty, _) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
     Printf.fprintf oc "void ml2c(const %s& _s0, const value _mlvalue, %s *_cvaluep);\n"
       sentinel_type
       (fmt_cpptype cty)

let arg_snippets tmap (cty, cid) =
  let ml_cty = ctype2concretetype tmap cty in
  let formal_varname = Printf.sprintf "_mlv_%s" cid in
  let argdecl = Printf.sprintf "value %s" formal_varname in
  (argdecl, formal_varname)

let gen_stanza_bodies tmap oc = function
  | (ML _ | MLI _| TYPEDEF _) -> ()
  | CPP(HERE, s) -> output_string oc s
  | CPP _  -> ()
  | CPP2ML(cty, body) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
     Printf.fprintf oc "value c2ml(const %s& _s0, const %s& _cvalue) {
  CAMLparam0();
  CAMLlocal1(_mlvalue);
  %s ;
  CAMLreturn(_mlvalue);
}
"
  sentinel_type
  (fmt_cpptype cty) body
  | ML2CPP(cty, body) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
     Printf.fprintf oc "void ml2c(const %s& _s0, const value _mlvalue, %s *_cvaluep) {
  %s ;
}
"
       sentinel_type
       (fmt_cpptype cty) body
  | FOREIGN(rtys, fname, argformals, body) ->
     let ml_rtyl = List.map (ctype2concretetype tmap) rtys in
     let converted_l = List.map (arg_snippets tmap) argformals in
     let argdecl_l = List.map fst converted_l in
     let param_l = List.map snd converted_l in
     let args = List.map (fun (cty,cid) ->
       (cty, cid, Printf.sprintf "_mlv_%s" cid)
     ) argformals in
     Printf.fprintf oc
"extern \"C\" value %s(%s) {
  CAMLparam%d(%s);
  CAMLlocal1 (_mlv_res) ;
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
  (String.concat ", " argdecl_l)
  (List.length argformals)
  (String.concat ", " param_l)
  (* ML->C *)
  (String.concat "\n  " (List.map (fun (cty, cid, mlid) ->
    Printf.sprintf "%s %s;\n  %s _s_%s;\n  ml2c(_s_%s, %s, &%s);"
      (fmt_cpptype cty) cid
      (concretetype_to_sentineltype tmap (ctype2concretetype tmap cty)) cid
      cid mlid cid) args))
  (match rtys with [] -> "" | ctys ->
    String.concat "\n  " (List.mapi (fun i cty ->
      Printf.sprintf "%s _res%d;" (fmt_cpptype cty) i)
			 ctys))
  (* BODY *)
  body
  (* C->ML *)
  (match rtys with [] -> "" | l ->
    let res_vars = List.mapi (fun i _ -> Printf.sprintf "_res%d" i) ml_rtyl in
    let sentinel_types = List.map (concretetype_to_sentineltype tmap) ml_rtyl in
    let sentinel_vars = List.mapi (fun i _ -> Printf.sprintf "_s%d" i) sentinel_types in
    let sentinel_decls = List.mapi (fun i sty -> Printf.sprintf "%s _s%d" sty i) sentinel_types in
    let res_assignment =
      Printf.sprintf "  _mlv_res = c2ml(%s);" (String.concat ", " (sentinel_vars@res_vars)) in
    String.concat ";\n" (sentinel_decls@[res_assignment]))

let gen tmap oc t =
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
  List.iter (gen_stanza_forwards tmap oc) t.stanzas ;
  output_string oc "
#include \"cppffi.inc\"
";
  List.iter(fun s ->
    gen_stanza_bodies tmap oc s;
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

let gen_typedecls ~ml oc tmap =
  let l = tmap in
  Printf.fprintf oc (if ml then "module Types = struct\n" else "module Types : sig\n");
  Printf.fprintf oc "type %s\n"
    (String.concat "\nand " (List.map (fun (id, e) -> match e.TMAP.mltype with
    | MLTYPE.CONCRETE s -> Printf.sprintf "%s = %s" id (MLTYPE.concrete_to_mlstring s)
    | ABSTRACT s ->  s
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
	  (List.map (fun (cty, _) -> MLTYPE.concrete_to_mlstring (ctype2concretetype tmap cty)) l))
       (match rtys with
	 [] -> "unit"
       | l -> String.concat " * "
	  (List.map (fun t -> MLTYPE.concrete_to_mlstring (ctype2concretetype tmap t)) l))
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
