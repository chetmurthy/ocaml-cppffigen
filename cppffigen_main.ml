
open Cppffigen
open Cmdliner

let expand_composite tmap t =
  let expand1 = function
    | ATTRIBUTE t -> expand_attribute t
    | STRUCT t -> expand_struct tmap t
    | t -> [t] in
  { stanzas = List.concat (List.map expand1 t.stanzas) }

let gen_f mode =
  let t = t_of_sexp (Sexplib.Sexp.input_sexp stdin) in
  let typedecls = ML.setup_typedecls t in
  let tmap = List.map (function (id, MLTYPE.CONCRETE t) -> (id, t) | (id, ABSTRACT s) -> (id, MLTYPE.OTHER s)) typedecls in

  let t = expand_composite tmap t in
  match mode with
  | `CPP -> CPP.gen stdout t
  | `ML -> ML.gen (typedecls,tmap) stdout t
  | `MLI -> MLI.gen (typedecls,tmap) stdout t
  | `SEXP -> Sexplib.Sexp.output_hum stdout(sexp_of_t t)

let opts_sect = "OPTIONS"

let gen_cmd =
  let doc = "C++ FFI generator" in
  let man = [] in
  let ftype =
    let doc = "output file type (cpp or ml)" in
    let docs = opts_sect in
    Arg.(value & opt (enum ["cpp",`CPP; "ml", `ML; "mli", `MLI; "sexp", `SEXP]) `CPP & info ["output"] ~docs ~docv:"OUTPUT-FILE-TYPE" ~doc) in
  Term.(const gen_f $ ftype),
  Term.info "cppffigen" ~version ~sdocs:opts_sect ~doc ~man

let main () =
  match Term.eval ~catch:true gen_cmd with
  | `Error _ -> exit 1 | _ -> exit 0
;;

if not !Sys.interactive then
  main()
;;
