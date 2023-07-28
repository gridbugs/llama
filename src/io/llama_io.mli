open! Llama

module Wav : sig
  type t

  val of_file_at_path : string -> t

  val sample_player_mono : t -> Signal.Trigger.t -> float Signal.t
  (** returns signal of float values from the wav file where each channel is
      averaged together *)
end
