exception Error of string

val input_uint8 : in_channel -> int
val input_int8 : in_channel -> int
val input_uint16 : in_channel -> int
val input_int16 : in_channel -> int
val input_uint24 : in_channel -> int
val input_int24 : in_channel -> int
val input_int32 : in_channel -> int
val input_string : in_channel -> int -> string
val skip_bytes : in_channel -> int -> unit
