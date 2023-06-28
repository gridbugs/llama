let test () =
  Audio_io.System.env_logger_init ();
  let output_stream = Audio_io.Output_stream.create () in
  print_endline
    (Printf.sprintf "num_channels: %d, sample_rate: %d"
       (Audio_io.Output_stream.num_channels output_stream)
       (Audio_io.Output_stream.sample_rate output_stream));
  Audio_io.Output_stream.send_sample output_stream 0.01
