open Result

let kNotFound = 1

let code_to_string = function
  | 0 ->     "Ok"
  | 1 ->     "NotFound"
  | 2 ->     "Corruption"
  | 3 ->     "NotSupported"
  | 4 ->     "InvalidArgument"
  | 5 ->     "IOError"
  | 6 ->     "MergeInProgress"
  | 7 ->     "Incomplete"
  | 8 ->     "ShutdownInProgress"
  | 9 ->     "TimedOut"
  | 10 ->     "Aborted"
  | 11 ->     "Busy"
  | 12 ->     "Expired"
  | 13 ->     "TryAgain"
  | n -> Printf.sprintf "<Unrecognized code %d>" n

let subcode_to_string = function
  | 0 ->     "None"
  | 1 ->     "MutexTimeout"
  | 2 ->     "LockTimeout"
  | 3 ->     "LockLimit"
  | 4 ->     "NoSpace"
  | 5 ->     "Deadlock"
  | n -> Printf.sprintf "<Unrecognized subcode %d>" n


type status_t = {code : int ; subcode : int ; msg : string }
let format_status st =
  Printf.sprintf "<%s, %s, %s>\n" (code_to_string st.code) (subcode_to_string st.subcode) st.msg

let status_to_result st =
  if 0 = st.code then Ok ()
  else Error (format_status st)

let status2_raise_not_found (st, rv as p) =
  if kNotFound = st.code then raise Not_found ;
  p

let status2_to_result (st, rv) =
  if 0 = st.code then Ok rv
  else Error (format_status st)

let status3_to_result (st, rv1, rv2) =
  if 0 = st.code then Ok (rv1, rv2)
  else Error (format_status st)

let error_to_failure ?(msg="") = function
  | Error s -> 
    failwith (msg^": "^s)
  | Ok s -> s

let error_to_assert_failure = function
  | Error s ->
    assert false
  | Ok s -> s

let none_to_failure ?(msg="") = function
  | None -> 
    failwith msg
  | Some s -> s
