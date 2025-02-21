(**pp -syntax camlp5o -package pa_ppx.located_sexp,pa_ppx_fmtformat*)
open Cppffigen
open Cmdliner
open Pa_ppx_located_sexp


let expand_composite tmap t =
  let expand1 = function
    | ATTRIBUTE t -> expand_attribute t
    | STRUCT t -> expand_struct tmap t
    | t -> [t] in
  { stanzas = List.concat (List.map expand1 t.stanzas) }

let gen_f ic pps mode =
  let t = t_of_located_sexp (Sexp.input_sexp ic) in
  let tmap = TMAP.mk t.stanzas in
  let t = expand_composite tmap t in
  match mode with
  | `CPP -> CPP.gen tmap pps t
  | `ML -> ML.gen tmap pps t
  | `MLI -> MLI.gen tmap pps t
  | `SEXP -> Sexp.pp_hum pps (located_sexp_of_t t)

let opts_sect = "OPTIONS"

let gen_cmd =
  let doc = "C++ FFI generator" in
  let man = [] in
  let ftype =
    let doc = "output file type (cpp or ml)" in
    let docs = opts_sect in
    Arg.(value & opt (enum ["cpp",`CPP; "ml", `ML; "mli", `MLI; "sexp", `SEXP]) `CPP & info ["output"] ~docs ~docv:"OUTPUT-FILE-TYPE" ~doc) in
  Term.(const (gen_f stdin Format.std_formatter) $ ftype),
  Term.info "cppffigen" ~version ~sdocs:opts_sect ~doc ~man

let main () =
  match Term.eval ~catch:true gen_cmd with
  | `Error _ -> exit 1 | _ -> exit 0
;;

if not !Sys.interactive then
  main()
;;
