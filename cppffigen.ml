open Misc
open Sexplib
open Sexplib.Std

let version = "0.001"

type primtype = INT | CHAR | BOOL [@@deriving sexp]

type cpptype =
    PTR of cpptype
  | ID of string
  | TYCON of string * cpptype list
  | PRIM of primtype [@@deriving sexp]

type mltype =
    GEN of string
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

type stanza_t =
  | TYPEDEF of def_t
  | ATTRIBUTE of Attribute.t
  | CPP2ML of cpptype * string
  | ML2CPP of cpptype * string
  | CPP of string
  | ML of string
  | MLI of string
  | FOREIGN of cpptype list * string * (cpptype * string) list * string [@@deriving sexp]

let expand_attribute {Attribute.target ; aname ; fprefix ; cpptype } =
    [FOREIGN([], Printf.sprintf "%s%s_set_%s" fprefix target aname,
	     [(ID target), "rcvr"; cpptype, aname],
	     Printf.sprintf "rcvr->%s = %s;" aname aname) ;
     FOREIGN([cpptype], Printf.sprintf "%s%s_get_%s" fprefix target aname,
	     [(ID target), "rcvr"], Printf.sprintf "_res0 = rcvr->%s;" aname)
  ]

type t = {
  cpp_prologue : string ;
  cpp_epilogue : string ;
  ml_prologue : string ;
  ml_epilogue : string ;
  mli_prologue : string ;
  mli_epilogue : string ;

  stanzas : stanza_t  list;
} [@@deriving sexp]

module CPP = struct

let fmt_cpptype ty =
  let rec frec = function
    | ID s -> s
    | PTR t -> Printf.sprintf "%s*" (frec t)
    | TYCON (id, l) ->
       Printf.sprintf "%s< %s >" id (String.concat ", " (List.map frec l))
    | PRIM INT -> "int"
    | PRIM CHAR -> "char"
    | PRIM BOOL -> "bool"
  in frec ty

let gen_stanza_forwards oc = function
  | (CPP _| ML _ | MLI _| FOREIGN _) -> ()
  | TYPEDEF t ->
     Printf.fprintf oc "typedef %s %s;\n" (fmt_cpptype t.cpptype) t.name
  | CPP2ML(cty, _) ->
     Printf.fprintf oc "value c2ml(const %s& _cvalue);\n" (fmt_cpptype cty)
  | ML2CPP(cty, _) ->
     Printf.fprintf oc "void ml2c(const value _mlvalue, %s *_cvaluep);\n" (fmt_cpptype cty)

let gen_stanza_bodies oc = function
  | (ML _ | MLI _| TYPEDEF _) -> ()
  | CPP s -> output_string oc s
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
  output_string oc t.cpp_prologue ;
  List.iter (gen_stanza_forwards oc) t.stanzas ;
  output_string oc "
#include \"cppffi.inc\"
";
  List.iter(fun s ->
    gen_stanza_bodies oc s;
    output_string oc "\n") t.stanzas ;
  output_string oc t.cpp_epilogue ;
  ()
end

module ML = struct

let ctype2mltype tmap cty =
  let rec crec = function
    | PRIM INT -> "int"
    | PRIM CHAR -> "char"
    | PRIM BOOL -> "bool"
    | ID "std::string" -> "string"
    | ID s -> begin
      if not (List.mem_assoc s tmap) then
	failwith (Printf.sprintf "typename %s not found in map" s) ;
      match List.assoc s tmap with
	GEN s -> s
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

let setup_typedecls t =
  let tmap = ref [] in
  List.iter (function
  | TYPEDEF t ->
     if List.mem_assoc t.name !tmap then
       failwith (Printf.sprintf "typenae %s already previously typedef-ed" t.name) ;
    push tmap (t.name, t.mltype)
  | _ -> ()
  ) t.stanzas ;
  tmap

let gen_typedecls oc tmap =
  let l = !tmap in
  Printf.fprintf oc "type %s\n"
    (String.concat "\nand " (List.map (function
    | (id, EXP s) -> Printf.sprintf "%s = %s" id s
    | (id, GEN s) -> s) l))

let gen_stanza tmap oc = function
  | (CPP _|CPP2ML _|ML2CPP _|MLI _) -> ()
  | TYPEDEF _ -> ()
  | ML s -> output_string oc s
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

let gen oc t =
  let tmap = setup_typedecls t in
  output_string oc t.ml_prologue ;
  gen_typedecls oc tmap ;
  List.iter (gen_stanza !tmap oc) t.stanzas ;
  output_string oc t.ml_epilogue ;
  ()
end

open Cmdliner

let expand_attributes t =
  let expand1 = function
    | ATTRIBUTE t -> expand_attribute t
    | t -> [t] in
  { t with stanzas = List.concat (List.map expand1 t.stanzas) }

let gen_f mode =
  let t = t_of_sexp (Sexplib.Sexp.input_sexp stdin) in
  let t = expand_attributes t in
  match mode with
  | `CPP -> CPP.gen stdout t
  | `ML -> ML.gen stdout t
  | `SEXP -> Sexplib.Sexp.output_hum stdout(sexp_of_t t)

let opts_sect = "OPTIONS"

let gen_cmd =
  let doc = "C++ FFI generator" in
  let man = [] in
  let ftype =
    let doc = "output file type (cpp or ml)" in
    let docs = opts_sect in
    Arg.(value & opt (enum ["cpp",`CPP; "ml", `ML; "sexp", `SEXP]) `CPP & info ["output"] ~docs ~docv:"OUTPUT-FILE-TYPE" ~doc) in
  Term.(const gen_f $ ftype),
  Term.info "cppffigen" ~version ~sdocs:opts_sect ~doc ~man

let main () =
  match Term.eval ~catch:true gen_cmd with
  | `Error _ -> exit 1 | _ -> exit 0
;;

if invoked_with "cppffigen" then
  main()
;;
