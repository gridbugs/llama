module System = struct
  external env_logger_init : unit -> unit = "env_logger_init"
end

module Output_stream = struct
  type t

  external create_with_downsample_32 : int32 -> t
    = "create_output_stream_with_downsample"

  let create_with_downsample downsample =
    create_with_downsample_32 (Int32.of_int downsample)

  external sample_rate_32 : t -> int32 = "sample_rate"

  let sample_rate t = sample_rate_32 t |> Int32.to_int

  external num_channels_32 : t -> int32 = "num_channels"

  let num_channels t = num_channels_32 t |> Int32.to_int

  external set_buffer_padding_32 : t -> int32 -> unit = "set_buffer_padding"

  let set_buffer_padding t buffer_padding =
    set_buffer_padding_32 t (Int32.of_int buffer_padding)

  external samples_behind_32 : t -> int32 = "samples_behind"

  let samples_behind t = samples_behind_32 t |> Int32.to_int

  external send_sample : t -> float -> unit = "send_sample"
end
