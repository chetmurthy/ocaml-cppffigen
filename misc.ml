
let car = List.hd
let cdr = List.tl

let rec sep_last = function
    [] -> failwith "sep_last"
  | hd::[] -> (hd,[])
  | hd::tl ->
      let (l,tl) = sep_last tl in (l,hd::tl)

let third3 (a,b,c)  = c
let push l x = (l := x :: !l)

let invoked_with ?flag cmdna =
  let variant_names = [cmdna; cmdna^".byte"; cmdna^".native"] in

  let argv = Array.to_list Sys.argv in
  let path = Pcre.split ~rex:(Pcre.regexp "/") (car argv) in
  let fname, _ = sep_last path in

  List.exists ((=) fname) variant_names &&
  match flag with None -> true | Some flag ->
    let flag' = "-"^flag in
    let flag'' = "--"^flag in
    List.exists ((=) flag') (cdr argv) ||
      List.exists ((=) flag'') (cdr argv)
