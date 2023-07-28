open Llama_interactive

module Voice : sig
  type t = { frequency_hz : float; gate : Signal.Gate.t }
end

val voices :
  keys:Signal.Gate.t Input.All_keyboard.t ->
  mid_note:Music.Note.t ->
  Voice.t list
