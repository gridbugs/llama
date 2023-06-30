open! Llama

module Wav = struct
  type t = { path : string }

  let of_file_at_path path = { path }

  let read_wav_file_mono_exn { path } =
    Llama_low_level.Wav.read_wav_file_mono_exn path

  let sample_player_mono t =
    let data = read_wav_file_mono_exn t in
    Dsl.sample_player_mono data
end
