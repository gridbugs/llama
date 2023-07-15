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

module Midi_input = struct
  type t

  external create : unit -> t = "create_midi_input"
  external midi_port_names_raw : t -> string array = "midi_port_names"
  external get_num_midi_ports_raw : t -> int32 = "get_num_midi_ports"
  external is_midi_input_available_raw : t -> bool = "is_midi_input_available"
  external midi_port_connect_raw : t -> int32 -> unit = "midi_port_connect"

  external midi_port_drain_messages_raw : t -> int32 array
    = "midi_port_drain_messages"

  let midi_port_names t = midi_port_names_raw t |> Array.to_list
  let get_num_midi_ports t = get_num_midi_ports_raw t |> Int32.to_int

  let midi_port_connect t index =
    let num_midi_ports = get_num_midi_ports t in
    if index < num_midi_ports then
      if is_midi_input_available_raw t then
        midi_port_connect_raw t (Int32.of_int index)
      else
        failwith
          "Midi input may only be used to connect to a single port (ever)"
    else
      failwith
        (Printf.sprintf
           "Midi port index %d is out of range (there are %d midi ports)" index
           num_midi_ports)

  let midi_port_drain_messages_to_char_array t =
    midi_port_drain_messages_raw t
    |> Array.map (fun i32 -> char_of_int (Int32.to_int i32))
end
