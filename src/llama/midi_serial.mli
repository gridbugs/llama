type t

val create : port:string -> baud:int -> t
val consume_all_available_bytes : t -> unit
val drain_messages : t -> Llama_midi.Message.t list
