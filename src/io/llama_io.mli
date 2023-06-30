open! Llama

module Wav : sig
  type t

  val of_file_at_path : string -> t
  val sample_player_mono : t -> bool Signal.t -> float Signal.t
end
