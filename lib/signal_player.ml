module Output_stream = Audio_io.Output_stream
module Ctx = Signal.Ctx

type t = {
  output_stream : Output_stream.t;
  mutable sample_index : int;
  signal : float Signal.t ref;
  sample_rate_hz : float;
  num_channels : int;
}

let create ?(downsample = 1) () =
  let output_stream = Output_stream.create_with_downsample downsample in
  {
    output_stream;
    sample_index = 0;
    signal = ref (Signal.const 0.0);
    sample_rate_hz = Int.to_float (Output_stream.sample_rate_hz output_stream);
    num_channels = Output_stream.num_channels output_stream;
  }

let signal_ref t = t.signal

let play_stream_fragment t =
  let num_samples_to_play =
    Output_stream.samples_behind t.output_stream / t.num_channels
  in
  for _ = 0 to num_samples_to_play do
    let ctx =
      { Ctx.sample_index = t.sample_index; sample_rate_hz = t.sample_rate_hz }
    in
    t.sample_index <- t.sample_index + 1;
    let sample = Signal.sample !(t.signal) ctx in
    Output_stream.send_sample t.output_stream sample
  done

let rec loop t =
  let open Lwt.Syntax in
  play_stream_fragment t;
  let* () = Lwt_unix.sleep 0.01 in
  loop t

let run t = loop t