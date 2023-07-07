exception Parse_exception of string

type 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val skip : int -> unit t
val repeat_until_relative_index_offset_exact : int -> 'a t -> 'a list t
val repeat_until_end_exact : 'a t -> 'a list t
val string4 : string t
val int32be : int t
val int16be : int t
val byte : int t
val peek_byte : int t
val byte_msb0 : int t
val n_bytes : int -> char array t
val variable_length_quantity : int t
val run : 'a t -> char array -> 'a
