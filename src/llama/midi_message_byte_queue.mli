type t

val create : unit -> t
val add_byte : t -> char -> unit
val drain_messages : t -> Llama_midi.Message.t List.t
