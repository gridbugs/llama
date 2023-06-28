let foo = Audio_io.a

module Output_stream = Audio_io.Output_stream

let test () =
  let player = Output_stream.new_player () in
  let i = Output_stream.example player 6.5 |> Int32.to_int in
  print_endline (Printf.sprintf "%d" i)
