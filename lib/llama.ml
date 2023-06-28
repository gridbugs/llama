let foo = Cpal_wrapper.a

let test () =
  let player = Cpal_wrapper.new_player () in
  let i = Cpal_wrapper.example player 6.5 |> Int32.to_int in
  print_endline (Printf.sprintf "%d"  i)
