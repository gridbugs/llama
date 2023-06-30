module System = struct
  external env_logger_init_raw : unit -> unit = "env_logger_init"

  let is_initialized = ref false

  let env_logger_init () =
    if not !is_initialized then (
      is_initialized := true;
      env_logger_init_raw ())
end

module Wav = struct
  external read_wav_file_mono_exn_raw : string -> float array
    = "read_wav_file_mono"

  let read_wav_file_mono_exn path =
    (* TODO: This array can't be indexed with [Array.get] but values can be
       obtained with (say) [Array.iter]. This is possibly a bug in the ocaml
       rust bindings for the [float array] special case. *)
    let broken_array = read_wav_file_mono_exn_raw path in
    Array.of_list (Array.to_list broken_array)
end

module Output_stream = struct
  type t

  external create_with_downsample_32 : int32 -> t
    = "create_output_stream_with_downsample"

  let create_with_downsample downsample =
    create_with_downsample_32 (Int32.of_int downsample)

  external sample_rate_hz_32 : t -> int32 = "sample_rate_hz"

  let sample_rate_hz t = sample_rate_hz_32 t |> Int32.to_int

  external num_channels_32 : t -> int32 = "num_channels"

  let num_channels t = num_channels_32 t |> Int32.to_int

  external set_buffer_padding_32 : t -> int32 -> unit = "set_buffer_padding"

  let set_buffer_padding t buffer_padding =
    set_buffer_padding_32 t (Int32.of_int buffer_padding)

  external samples_behind_32 : t -> int32 = "samples_behind"

  let samples_behind t = samples_behind_32 t |> Int32.to_int

  external send_sample : t -> float -> unit = "send_sample"
end
