module System : sig
  val env_logger_init : unit -> unit
end

module Wav : sig
  val read_wav_file_mono_exn : string -> float array
end

module Output_stream : sig
  type t

  val create_with_downsample : int -> t
  val sample_rate_hz : t -> int
  val num_channels : t -> int
  val set_buffer_padding : t -> int -> unit
  val samples_behind : t -> int
  val send_sample : t -> float -> unit
end

module Midi_input : sig
  type t

  val create : unit -> t
  val enumerate_midi_ports : t -> string list
end
