type t = {
  fd : Unix.file_descr;
  midi_message_byte_queue : Midi_message_byte_queue.t;
  buf : bytes;
}

let open_serial_port ~port ~baud =
  let fd = Unix.openfile port [ Unix.O_RDONLY; Unix.O_NONBLOCK ] 0o000 in
  let current_attrs = Unix.tcgetattr fd in
  let () =
    Unix.tcsetattr fd Unix.TCSANOW
      {
        current_attrs with
        c_ibaud = baud;
        c_obaud = baud;
        c_echo = false;
        c_icanon = false;
      }
  in
  fd

let create ~port ~baud =
  let fd = open_serial_port ~port ~baud in
  let midi_message_byte_queue = Midi_message_byte_queue.create () in
  let buf = Bytes.create 1 in
  { fd; midi_message_byte_queue; buf }

let read_byte t =
  try
    let bytes_read = Unix.read t.fd t.buf 0 1 in
    if bytes_read == 1 then Some (Bytes.get t.buf 0) else None
  with Unix.Unix_error (Unix.EAGAIN, _, _) -> None

let consume_all_available_bytes t =
  let rec loop () =
    match read_byte t with
    | None -> ()
    | Some byte ->
        Midi_message_byte_queue.add_byte t.midi_message_byte_queue byte;
        loop ()
  in
  loop ()

let drain_messages t =
  Midi_message_byte_queue.drain_messages t.midi_message_byte_queue
