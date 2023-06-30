open! Llama

module Wav = struct
  module Metadata = struct
    type t = { channels : int; sample_rate : int; length_in_samples : int }
  end

  module Data = struct
    type t = { metadata : Metadata.t }
  end

  let read_file_exn path =
    let reader = new Mm.Audio.IO.Reader.of_wav_file path in
    let channels = reader#channels in
    let sample_rate = reader#sample_rate in
    let length_in_samples = reader#length in
    let metadata = { Metadata.channels; sample_rate; length_in_samples } in
    let buffer =
      Array.init channels ~f:(fun _ ->
          Array.init length_in_samples ~f:(fun _ -> 0.0))
    in
    (* TODO: mm fails to decode any of the wav files I tried here *)
    let _samples_read = reader#read buffer 0 length_in_samples in
    let data = { Data.metadata } in
    reader#close;
    data
end
