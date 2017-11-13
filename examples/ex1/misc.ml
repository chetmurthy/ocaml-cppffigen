let do_option f = function
    Some x -> f x
  | None -> ()

let push l x = (l := x :: !l)


let hexdump_fmt ~offset_format ~width outf buffer =
  let format_offset ofs =
    match offset_format with
    | `Oct -> Printf.sprintf "%07o " ofs
    | `Dec -> Printf.sprintf "%07d " ofs
    | `Hex -> Printf.sprintf "%06x " ofs in

  let buflen = Bytes.length buffer in
  let rec drec ofs =
    if ofs = buflen then () else begin
      let fmt_offset = format_offset ofs in
      let offset_len = (String.length fmt_offset) in
      outf fmt_offset ;
      let remaining = (width - offset_len) / 3 in
      let remaining = min remaining (buflen - ofs) in
      for i = 0 to remaining - 1 do
	let c = Bytes.get buffer (ofs + i) in
	outf (Printf.sprintf "%02x " (Char.code c))
      done ;
      outf "\n" ;
      outf (String.make offset_len ' ') ;
      let escape c =
	let s = Char.escaped c in
	if String.get s 0 = '\\'
	&& (String.get s 1 = '0' ||
	    String.get s 1 = '1' ||
	    String.get s 1 = '2') then
	  " "
	else s in
      for i = 0 to remaining - 1 do
	let c = Bytes.get buffer (ofs + i) in
	let escs = escape c in
	let nfill = max 0 (3 - (String.length escs)) in
	outf escs ; outf (String.make nfill ' ') ;
      done ;
      outf "\n" ;

      drec (ofs + remaining)
    end
  in
  drec 0

let hexdump ?(offset_format=`Oct) ?(width=0) oc buffer =
  let fd = Unix.descr_of_out_channel oc in
  let isatty = Unix.isatty fd in
  let width =
    if width = 0 then
      80
    else width in
  hexdump_fmt ~offset_format ~width (output_string oc) buffer

let hexdump_to_string ?(offset_format=`Oct) ?(width=80) buffer =
  let b = Buffer.create width in
  hexdump_fmt ~offset_format ~width (Buffer.add_string b) buffer ;
  Buffer.contents b
