val kNotFound : int
val code_to_string : int -> string
val subcode_to_string : int -> string
type status_t = { code : int; subcode : int; msg : string; }
val format_status : status_t -> string
val status_to_result : status_t -> (unit, string) Result.result
val status2_raise_not_found : status_t * 'a -> status_t * 'a
val status2_to_result : status_t * 'a -> ('a, string) Result.result
val status3_to_result : status_t * 'a * 'b -> ('a * 'b, string) Result.result
val error_to_failure : ?msg:string -> ('a, string) Result.result -> 'a
val error_to_assert_failure : ('a, 'b) Result.result -> 'a
val none_to_failure : ?msg:string -> 'a option -> 'a
