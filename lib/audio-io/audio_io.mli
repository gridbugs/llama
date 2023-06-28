module System : sig
  val env_logger_init : unit -> unit
end

module Output_stream : sig
  type t

  val create_with_downsample : int -> t
  val sample_rate : t -> int
  val num_channels : t -> int
  val set_buffer_padding : t -> int -> unit
  val samples_behind : t -> int
  val send_sample : t -> float -> unit
end
