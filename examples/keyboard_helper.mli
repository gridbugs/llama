open Llama_interactive

module Voice : sig
  type t = { frequency_hz : float; gate : bool Signal.t }
end

val voices :
  keys:bool Signal.t Input.All_keyboard.t ->
  mid_note:Music.Note.t ->
  Voice.t list
