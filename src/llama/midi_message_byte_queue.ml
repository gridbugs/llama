type state = Empty | Ignoring_system_exclusive | Waiting_for_bytes of int

type t = {
  byte_queue : char Queue.t;
  message_queue : Llama_midi.Message.t Queue.t;
  state : state ref;
}

let create () =
  {
    byte_queue = Queue.create ();
    message_queue = Queue.create ();
    state = ref Empty;
  }

let drain_bytes_into_message_queue t =
  let message =
    Queue.to_seq t.byte_queue |> Array.of_seq
    |> Llama_midi.Message.parse_from_char_array
  in
  Queue.add message t.message_queue;
  Queue.clear t.byte_queue

let add_byte t byte =
  let int = int_of_char byte in
  match !(t.state) with
  | Ignoring_system_exclusive ->
      if int == Llama_midi.Low_level.Message.system_exclusive_end then
        t.state := Empty
  | Empty ->
      assert (Queue.is_empty t.byte_queue);
      if int == Llama_midi.Low_level.Message.system_exclusive_start then
        t.state := Ignoring_system_exclusive
      else if Llama_midi.Low_level.Message.is_status_byte int then (
        Queue.add byte t.byte_queue;
        match
          Llama_midi.Low_level.Message.payload_length_of_status_byte int
        with
        | Variable -> failwith "unreachable"
        | Fixed 0 -> drain_bytes_into_message_queue t
        | Fixed n -> t.state := Waiting_for_bytes n)
      else
        (* assume that we connected mid-stream or there's an error on the line *)
        ()
  | Waiting_for_bytes n ->
      assert (n > 0);
      if Llama_midi.Low_level.Message.is_status_byte int then (
        (* unexpected end of message, go back to empty state *)
        Queue.clear t.byte_queue;
        t.state := Empty)
      else (
        Queue.add byte t.byte_queue;
        if n == 1 then (
          (* this was the final byte of the message *)
          drain_bytes_into_message_queue t;
          t.state := Empty)
        else t.state := Waiting_for_bytes (n - 1))

let drain_messages t =
  let messages = Queue.to_seq t.message_queue |> List.of_seq in
  Queue.clear t.message_queue;
  messages
