(**pp -syntax camlp5o -package pa_ppx_fmtformat*)
open Pa_ppx_utils
open Sexplib
open Sexplib.Std

let version = "0.003"

let fst3 (a,b,c)  = a
let snd3 (a,b,c)  = b
let third3 (a,b,c)  = c
let push l x = (l := x :: !l)

let fmt_list_i ~sep pp1 pps l =
  let pairs = List.mapi (fun i x -> (i,x)) l in
  Fmt.list ~sep pp1 pps pairs

let prepend firstpp secondpp pps arg =
  Fmt.(pf pps "%a%a" firstpp () secondpp arg)

let append firstpp secondpp pps arg =
  Fmt.(pf pps "%a%a" firstpp arg secondpp ())

let if_nil ~nil ~list (pps : Format.formatter) l : unit =
  if l = [] then
    nil pps ()
  else list pps l

module CPPID = struct
type t = CPPID of string
let pp pps (CPPID s) = Fmt.(pf pps "%s" s)
let show cppid = Fmt.(str "%a" pp cppid)
let mk s = CPPID s

let t_of_sexp = function
    (Sexp.Atom s) -> CPPID s
  | _ -> failwith "CPPID.t_of_sexp"

let sexp_of_t (CPPID s) = (Sexp.Atom s)
end

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
  | ID of CPPID.t
  | TYCON of string * t list
  | PRIM of primtype [@@deriving sexp]
end

module MLID = struct
type t = MLID of string
let pp pps (MLID s) = Fmt.(pf pps "%s" s)
let show mlid = Fmt.(str "%a" pp mlid)
let mk s = MLID s

let t_of_sexp = function
    (Sexp.Atom s) -> MLID s
  | _ -> failwith "MLID.t_of_sexp"

let sexp_of_t (MLID s) = (Sexp.Atom s)
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
  | OTHER of MLID.t [@@deriving sexp]

  let tuple_type pp1 pps l = Fmt.(pf pps "%a" (list ~sep:(const string " * ") pp1) l)

  let rec ppml_concrete pps cty =
    let rec crec pps = function
      INT -> {%fmt_pf|int|} pps
    | INT32 -> {%fmt_pf|int32|} pps
    | INT64 -> {%fmt_pf|int64|} pps
    | CHAR -> {%fmt_pf|char|} pps
    | BOOL -> {%fmt_pf|bool|} pps
    | NATIVEINT -> {%fmt_pf|nativeint|} pps
    | STRING -> {%fmt_pf|string|} pps
    | ARRAY ty -> {%fmt_pf|(${ty | crec} array)|} pps
    | TUPLE l -> {%fmt_pf|(${l | tuple_type crec})|} pps
    | OPTION ty -> {%fmt_pf|(${ty | crec} option)|} pps
    | OTHER s -> {%fmt_pf|(${s | MLID.pp})|} pps
    in {%fmt_pf|$(cty | crec)|} pps

type t =
  | ABSTRACT of string
  | CONCRETE of concrete_type [@@deriving sexp]

let ppml = function
    ABSTRACT s -> s
  | CONCRETE cty -> {%fmt_str|$(cty | ppml_concrete)|}

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
    [FOREIGN([], {%fmt_str|$(fprefix)$(target)_set_$(aname)|},
	     [(ID (CPPID.mk target)), "rcvr"; cpptype, aname],
	     {%fmt_str|rcvr->${aname} = ${aname};|}) ;
     FOREIGN([cpptype], {%fmt_str|${fprefix}${target}_get_${aname}|},
	     [(ID (CPPID.mk target)), "rcvr"], {%fmt_str|_res0 = rcvr->$(aname);|})
  ]

let prim2mltype = function
  | (CPPTYPE.INT | UINT) -> MLTYPE. INT
  | (INT64 | UINT64) -> MLTYPE.INT64
  | (INT32 | UINT32) -> MLTYPE.INT32
  | (CHAR | UCHAR) -> MLTYPE.CHAR
  | BOOL -> MLTYPE.BOOL
  | STRING -> MLTYPE.STRING

module TMAP = struct
  type entry_t = {
      stanza : stanza_t
    ; mlid : MLID.t
    ; cppid : CPPID.t
    ; cpptype : CPPTYPE.t
    ; mltype : MLTYPE.t
    ; concretetype : MLTYPE.concrete_type
    }
  type t = {
      mlid_map : (MLID.t * entry_t) list
    ; cppid_map : (CPPID.t * entry_t) list
    ; entries : entry_t list
    }

  let typedef_to_entry = function
      (TYPEDEF t) as stanza ->
       { mlid = MLID.mk t.name
       ; cppid = CPPID.mk t.name
       ; cpptype = t.cpptype
       ; mltype = t.mltype
       ; stanza
       ; concretetype =
           match t.mltype with
             MLTYPE.CONCRETE t -> t
           | ABSTRACT s -> MLTYPE.OTHER (MLID.mk s)
       }

  let struct_to_entry = function
      (STRUCT t) as stanza ->
       let mlid = MLID.mk {%fmt_str|$(t.Struct.modname).$(t.Struct.name)|} in
       let cppid = CPPID.mk {%fmt_str|$(t.Struct.modname)_$(t.Struct.name)|} in
       let concretetype = MLTYPE.OTHER mlid in
       {
         mlid
       ; cppid
       ; cpptype = CPPTYPE.ID cppid
       ; mltype = MLTYPE.CONCRETE concretetype
       ; concretetype
       ; stanza
       }

  let mk t =
    let entries =
      t
      |> List.filter_map (function
               STRUCT _ as h -> Some(struct_to_entry h)
             | TYPEDEF _ as h -> Some(typedef_to_entry h)
             | _ -> None
           ) in
    let mlid_map = List.map (fun e -> (e.mlid, e)) entries in
    let cppid_map = List.map (fun e -> (e.cppid, e)) entries in

    let mlids = List.map (fun e -> e.mlid) entries in
    let cppids = List.map (fun e -> e.cppid) entries in
    let repeated_mlids = Std2.hash_list_repeats mlids in
    if [] <> repeated_mlids then
      failwith {%fmt_str|TMAP.mk: repeated ML typeids (in structs/typedefs): [$(|repeated_mlids | list ~sep:(const string " ") MLID.pp|)]|} ;
    let repeated_cppids = Std2.hash_list_repeats cppids in
    if [] <> repeated_mlids then
      failwith {%fmt_str|TMAP.mk: repeated C++ typeids (in structs/typedefs): [$(|repeated_cppids | list ~sep:(const string " ") CPPID.pp|)]|} ;
    
    { mlid_map ; cppid_map ; entries }

  let lookup_mlid tmap s =
    match List.assoc s tmap.mlid_map with
      e -> e
    | exception Not_found ->
       failwith {%fmt_str|ML id $(s|MLID.pp) not found in type-map|}

  let lookup_cppid tmap s =
    match List.assoc s tmap.cppid_map with
      e -> e
    | exception Not_found ->
       failwith {%fmt_str|++ id $(s|CPPID.pp) not found in type-map|}

  let mem_mlid tmap s = List.mem_assoc s tmap.mlid_map
  let mem_cppid tmap s = List.mem_assoc s tmap.cppid_map

  let entries tmap = tmap.entries

  let typedefs tmap =
    tmap.entries |> List.filter_map (function
                          {stanza=TYPEDEF _} as e -> Some e
                        | _ -> None)

  let structs tmap =
    tmap.entries |> List.filter_map (function
                          {stanza=STRUCT _} as e -> Some e
                        | _ -> None)

end

let ctype2concretetype (tmap : TMAP.t) cty : MLTYPE.concrete_type =
  let rec crec = function
    | CPPTYPE.PRIM t -> prim2mltype t
    | ID (CPPID.CPPID "std::string") -> MLTYPE.STRING
    | ID s -> begin
      if not (TMAP.mem_cppid tmap s) then
	failwith {%fmt_str|typename $(s|CPPID.pp) not found in map|} ;
      (TMAP.lookup_cppid tmap s).TMAP.concretetype
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
  | STRING -> "std::string"
  
let comma_separated pp1 pps l = Fmt.(pf pps "%a" (list ~sep:(const string ", ") pp1) l)

let ppcpp_cpptype pps ty =
  let rec frec pps = function
    | CPPTYPE.ID s -> {%fmt_pf|$(s|CPPID.pp)|} pps
    | PTR t -> {%fmt_pf|$(t | frec)*|} pps
    | TYCON (s, l) ->
       {%fmt_pf|$(s)< $(l | comma_separated frec) >|} pps
    | PRIM t -> {%fmt_pf|${fmt_primcpptype t}|} pps
  in {%fmt_pf|$(ty | frec)|} pps

let concretetype_to_sentineltype tmap mlty =
  let rec convrec pps = function
    MLTYPE.INT -> {%fmt_pf|sentinel_INT|} pps
  | INT32 -> {%fmt_pf|sentinel_INT32|} pps
  | INT64 -> {%fmt_pf|sentinel_INT64|} pps
  | CHAR -> {%fmt_pf|sentinel_INT|} pps
  | BOOL -> {%fmt_pf|sentinel_INT|} pps
  | NATIVEINT -> {%fmt_pf|sentinel_NATIVEINT|} pps
  | STRING -> {%fmt_pf|sentinel_GENERIC|} pps
  | ARRAY ty -> {%fmt_pf|sentinel_ARRAY<$(ty | convrec)>|} pps
  | TUPLE [t1;t2] -> {%fmt_pf|sentinel_TUPLE2<$(t1 | convrec),$(t2 | convrec)>|} pps
  | TUPLE [t1;t2;t3] -> {%fmt_pf|sentinel_TUPLE3<$(t1 | convrec),$(t2 | convrec),$(t3 | convrec)>|} pps
  | TUPLE _ -> failwith "mltype_to_sentineltype(tuple length > 2): unimplemented"
  | OPTION ty -> {%fmt_pf|sentinel_OPTION<$(ty | convrec)>|} pps
  | OTHER id ->
     let e = TMAP.lookup_mlid tmap id in
     match e.TMAP.stanza with
       STRUCT _ -> {%fmt_pf|sentinel_GENERIC|} pps
     | TYPEDEF { mltype = ABSTRACT _ } -> {%fmt_pf|sentinel_GENERIC|} pps
     | TYPEDEF _ ->  convrec pps e.TMAP.concretetype
  in
  {%fmt_str|$(mlty | convrec)|}

let pp_ml_field_decl tmap pps (cty,n) = {%fmt_pf|$(n) : $(ctype2concretetype tmap cty | MLTYPE.ppml_concrete) ;|} pps

let pp_cpp_field_decl pps (cty, n) = {%fmt_pf|  $(cty | ppcpp_cpptype) $(n) ;|} pps

let expand_struct tmap { Struct.modname; name ; members } =
  let cppid = CPPID.mk {%fmt_str|$(modname)_$(name)|} in
  let mlid = MLID.mk {%fmt_str|$(modname).$(name)|} in
  [
    ML(PROLOGUE,
       {%fmt_str|
module $(modname) = struct
  type t = { ${members | list ~sep:(const string "\n\t") (pp_ml_field_decl tmap)}
}
end
|}
      ) ;
    MLI(PROLOGUE,
       {%fmt_str|
module $(modname) : sig
  type t = { ${members | list ~sep:(const string "\n\t") (pp_ml_field_decl tmap)}
}
end
|}
      ) ;
    CPP(PROLOGUE,
	{%fmt_str|
#ifndef $(modname)_$(name)_DEFINED
#define $(modname)_$(name)_DEFINED
struct $(modname)_$(name) {
${ members | list ~sep:(const string "\n\t") pp_cpp_field_decl }} ;
#endif
|}
      ) ;
    (let ml2c_field pps (i, (cty, n)) =
      let mlty = ctype2concretetype tmap cty in
      let sentinel_type = concretetype_to_sentineltype tmap mlty in
      {%fmt_pf|ml2c(${sentinel_type}(), Field(_mlvalue,${ i|%d }), &(_cvaluep->${n}));|} pps in
    ML2CPP(ID cppid,
           {%fmt_str|${members | fmt_list_i ~sep:(const string "\n  ") ml2c_field}|}
      )) ;
    (let c2ml_field pps (i, (cty, n)) =
      let mlty = ctype2concretetype tmap cty in
      let sentinel_type = concretetype_to_sentineltype tmap mlty in
      {%fmt_pf|Store_field(_mlvalue, ${ i|%d }, c2ml(${sentinel_type}(), _cvalue.${n}));|} pps in
    CPP2ML(ID cppid,
           {%fmt_str|
  _mlvalue = caml_alloc(${List.length members|%d}, 0) ;
  ${ members| fmt_list_i ~sep:(const string "\n  ") c2ml_field }|}
)) ;
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

let gen_stanza_forwards tmap pps = function
  | (CPP _| ML _ | MLI _| FOREIGN _) -> ()
  | TYPEDEF t ->
     {%fmt_pf|typedef $(t.cpptype | ppcpp_cpptype) $(t.name);
|} pps

  | CPP2ML(cty, _) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
       {%fmt_pf|value c2ml(const $(sentinel_type)& _s0, const $(cty | ppcpp_cpptype)& _cvalue);
|} pps
  | ML2CPP(cty, _) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
       {%fmt_pf|void ml2c(const $(sentinel_type)& _s0, const value _mlvalue, $(cty | ppcpp_cpptype) *_cvaluep);
|} pps


let arg_snippets tmap (cty, cid) =
  let ml_cty = ctype2concretetype tmap cty in
  let formal_varname = Printf.sprintf "_mlv_%s" cid in
  let argdecl = Printf.sprintf "value %s" formal_varname in
  (argdecl, formal_varname)

let gen_stanza_bodies tmap pps = function
  | (ML _ | MLI _| TYPEDEF _) -> ()
  | CPP(HERE, s) -> Fmt.(pf pps "%s" s)
  | CPP _  -> ()
  | CPP2ML(cty, body) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
       {%fmt_pf|value c2ml(const $(sentinel_type)& _s0, const $(cty | ppcpp_cpptype)& _cvalue) {
  CAMLparam0();
  CAMLlocal1(_mlvalue);
  $(body) ;
  CAMLreturn(_mlvalue);
}
|} pps

  | ML2CPP(cty, body) ->
     let mlty = ctype2concretetype tmap cty in
     let sentinel_type = concretetype_to_sentineltype tmap mlty in
       {%fmt_pf|void ml2c(const $(sentinel_type)& _s0, const value _mlvalue, $(cty | ppcpp_cpptype) *_cvaluep) {
  $(body) ;
}
|} pps

  | FOREIGN(rtys, fname, argformals, body) ->
     let ml_rtyl = List.map (ctype2concretetype tmap) rtys in
     let converted_l = List.map (arg_snippets tmap) argformals in
     let argdecl_l = List.map fst converted_l in
     let param_l = List.map snd converted_l in
     let args = List.map (fun (cty,cid) ->
       (cty, cid, Printf.sprintf "_mlv_%s" cid)
     ) argformals in
     let pp_arg_conversion pps (cty, cid, mlid) =
       {%fmt_pf|$(cty | ppcpp_cpptype) $(cid);
  ml2c(${concretetype_to_sentineltype tmap (ctype2concretetype tmap cty)}(), $(mlid), &$(cid));|} pps in

     let pp_rty_decls_i pps (i, cty) =
       {%fmt_pf|$(cty | ppcpp_cpptype) _res$(i | %d);|} pps in

     let res_var (i,rty) = {%fmt_str|_res$(i|%d)|} in

     let res_vars = List.mapi (fun i _ -> Printf.sprintf "_res%d" i) ml_rtyl in
     let sentinel_exprs = List.map (fun rty -> Printf.sprintf "%s()" (concretetype_to_sentineltype tmap rty)) ml_rtyl in
     let res_assignment =
       if ml_rtyl = [] then
         Fmt.(const string "")
       else
         (fun pps () -> {%fmt_pf|_mlv_res = c2ml(${sentinel_exprs@res_vars | list ~sep:(const string ", ") string});|} pps) in


{%fmt_pf|extern \"C\" value $(fname)(${argdecl_l | list ~sep:(const string ", ") string}) {
  CAMLparam${List.length argformals | %d}(${param_l | list ~sep:(const string ", ") string});
  CAMLlocal1 (_mlv_res) ;
  /* ML->C*/
  ${args | list ~sep:(const string "\n\t") pp_arg_conversion}
  ${rtys | fmt_list_i  ~sep:(const string "\n\t") pp_rty_decls_i}
  /* BODY */
  ${body}
  /* C->ML*/
  ${ () | res_assignment }
  CAMLreturn(_mlv_res) ;
}
|} pps

let gen tmap pps t =
{%fmt_pf|
#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
${ prologues t | list ~sep:(const string "") string }
${ t.stanzas | list ~sep:(const string "") (gen_stanza_forwards tmap) }
#include "cppffi.inc"
${ t.stanzas | list ~sep:(const string "") (append (gen_stanza_bodies tmap) (const string "\n")) }
${ epilogues t | list ~sep:(const string "") string }
|} pps

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

let pp_typedecl pps e =
  let open TMAP in
  match e.mltype with
  | MLTYPE.CONCRETE s ->  {%fmt_pf|$(e.mlid | MLID.pp) = $(s | MLTYPE.ppml_concrete)|} pps
  | ABSTRACT s ->  {%fmt_pf|$(s)|} pps

let gen_typedecls ~ml pps tmap =
  {%fmt_pf| ${ if ml then "module Types = struct\n" else "module Types : sig\n" }
  type ${TMAP.typedefs tmap | list ~sep:(const string "\nand ") pp_typedecl}
  end
|} pps

let pp_argformals tmap pps argformals =
  match argformals with
  | [] -> {%fmt_pf|unit|} pps
  | l ->
     let l = List.map fst l in
     {%fmt_pf|${List.map (ctype2concretetype tmap) l | list ~sep:(const string " -> ") MLTYPE.ppml_concrete}|} pps

let pp_rtys tmap pps rtys =
  match rtys with
    [] -> {%fmt_pf|unit|} pps
  | l ->
     {%fmt_pf|${List.map (ctype2concretetype tmap) l | list ~sep:(const string " * ") MLTYPE.ppml_concrete}|} pps

let gen_stanza tmap pps = function
  | (CPP _|CPP2ML _|ML2CPP _|MLI _) -> ()
  | TYPEDEF _ -> ()
  | ML(HERE, s) -> Fmt.(pf pps "%s" s)
  | ML _ -> ()
  | FOREIGN(rtys, name, argformals, _) ->
       {%fmt_pf|external $(name) : $(argformals | pp_argformals tmap) -> $(rtys | pp_rtys tmap)
          ="$(name)"
|} pps

let gen (tmap : TMAP.t) pps t =
  {%fmt_pf|
  ${ prologues t | list ~sep:(const string "") string }
  ${ tmap | gen_typedecls ~ml:true }
  open Types
  ${ t.stanzas | list ~sep:(const string "") (gen_stanza tmap) }
  ${ epilogues t | list ~sep:(const string "") string }
|} pps
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
    
  let gen tmap pps t =
    {%fmt_pf|
  ${ prologues t | list ~sep:(const string "") string }
  ${ tmap | gen_typedecls }
  open Types
  ${ t.stanzas | list ~sep:(const string "") (gen_stanza tmap) }
  ${ epilogues t | list ~sep:(const string "") string }
|} pps

end
