module Signal = Llama_core.Signal

module Sync_channel = struct
  type channel = {
    mutable send_state : int64;
    mutable reciever_state : int64;
    mutex : Mutex.t;
    send_and_recv_states_are_different : Condition.t;
  }

  module Send = struct
    type nonrec t = { channel : channel }

    let send { channel } =
      Mutex.lock channel.mutex;
      channel.send_state <- Int64.add channel.send_state Int64.one;
      Condition.signal channel.send_and_recv_states_are_different;
      Mutex.unlock channel.mutex
  end

  module Recv = struct
    type nonrec t = { channel : channel }

    let recv { channel } =
      Mutex.lock channel.mutex;
      while Int64.equal channel.send_state channel.reciever_state do
        Condition.wait channel.send_and_recv_states_are_different channel.mutex
      done;
      channel.reciever_state <- channel.send_state;
      Mutex.unlock channel.mutex
  end

  (* A communication channel between a pair of threads: the "sender" and
     "reciever". The reciever can call [recv] to wait until the sender calls
     [send]. The [recv] function will block until [send] is called, unless
     [send] has been called since the last time [recv] returned. *)
  type t = { send : Send.t; recv : Recv.t }

  (* Create a linked receiver/sender pair. *)
  let create () =
    let channel =
      {
        send_state = Int64.zero;
        reciever_state = Int64.zero;
        mutex = Mutex.create ();
        send_and_recv_states_are_different = Condition.create ();
      }
    in
    { send = { Send.channel }; recv = { Recv.channel } }
end

module Viz_queue = struct
  module Writer = struct
    type 'a t = { queue : 'a Queue.t }

    let push t x = Queue.push x t.queue
  end

  type 'a t = { queue : 'a Queue.t; mutex : Mutex.t }

  let create () = { queue = Queue.create (); mutex = Mutex.create () }

  let drain t ~f =
    Mutex.protect t.mutex (fun () ->
        let rec loop () =
          match Queue.take_opt t.queue with
          | Some x ->
              f x;
              loop ()
          | None -> ()
        in
        loop ())

  let with_writer t ~f =
    Mutex.protect t.mutex (fun () -> f { Writer.queue = t.queue })
end

let bits = 16
let rate = 44100
let channels = 2

type t = { device : Ao.t; byte_format : Ao.byte_format_t }

module Double_buffer = struct
  type t = { mutable generate_into : Bytes.t; mutable play_from : Bytes.t }

  let create size =
    { generate_into = Bytes.create size; play_from = Bytes.create size }

  let swap t =
    let tmp = t.generate_into in
    t.generate_into <- t.play_from;
    t.play_from <- tmp
end

let create () =
  let driver = Ao.get_default_driver () in
  let byte_format = Ao.driver_preferred_byte_format driver in
  let device = Ao.open_live ~bits ~rate ~channels ~driver ~byte_format () in
  { device; byte_format }

let close t = Ao.close t.device

let sample_to_byte_pair_lo_hi sample =
  (* Clamp the sample between -1 and 1. *)
  let sample_clamped = Float.min sample 1.0 |> Float.max (-1.0) in
  (* Convert to a signed int between -32767 and 32767.*)
  let sample_int = int_of_float (sample_clamped *. 32767.0) in
  let byte_lo = sample_int land 0xFF |> char_of_int in
  let byte_hi = (sample_int lsr 8) land 0xFF |> char_of_int in
  (byte_lo, byte_hi)

let write_bytes_to_buffer_le buffer offset ~left_lo ~left_hi ~right_lo ~right_hi
    =
  Bytes.set buffer offset left_lo;
  Bytes.set buffer (offset + 1) left_hi;
  Bytes.set buffer (offset + 2) right_lo;
  Bytes.set buffer (offset + 3) right_hi

let write_bytes_to_buffer_be buffer offset ~left_lo ~left_hi ~right_lo ~right_hi
    =
  Bytes.set buffer offset left_hi;
  Bytes.set buffer (offset + 1) left_lo;
  Bytes.set buffer (offset + 2) right_hi;
  Bytes.set buffer (offset + 3) right_lo

module Viz = struct
  type t = Active
end

module Playing = struct
  type 'a t = {
    viz_queue : 'a Viz_queue.t;
    generator : unit Domain.t;
    player : unit Domain.t;
  }

  let wait { player; _ } = Domain.join player
  let viz_queue t = t.viz_queue
end

let play_mono t ?(viz_queue = `Create) ?(buffer_size_in_samples = 256)
    ?(viz = None) signal =
  assert (bits mod 8 == 0);
  assert (channels == 2);
  let ready_to_swap = Sync_channel.create () in
  let ready_to_gen = Sync_channel.create () in
  let viz_queue =
    match viz_queue with
    | `Create -> Viz_queue.create ()
    | `Use viz_queue -> viz_queue
  in
  let buffer_size_in_bytes = bits / 8 * channels * buffer_size_in_samples in
  let buffer = Double_buffer.create buffer_size_in_bytes in
  let write_bytes_to_buffer =
    match t.byte_format with
    | `LITTLE_ENDIAN | `UNKNOWN | `NATIVE ->
        (* assume little-endianness unless otherwise specified as it's by far the most common *)
        write_bytes_to_buffer_le
    | `BIG_ENDIAN -> write_bytes_to_buffer_be
  in
  let sample_index = ref 0 in
  let generator =
    Domain.spawn (fun _ ->
        while true do
          Viz_queue.with_writer viz_queue ~f:(fun writer ->
              for i = 0 to buffer_size_in_samples - 1 do
                let ctx =
                  {
                    Signal.Ctx.sample_rate_hz = float_of_int rate;
                    sample_index = !sample_index;
                  }
                in
                sample_index := !sample_index + 1;
                let sample = Signal.sample signal ctx in
                if Option.is_some viz then Viz_queue.Writer.push writer sample;
                let sample_lo, sample_hi = sample_to_byte_pair_lo_hi sample in
                write_bytes_to_buffer buffer.generate_into (i * 4)
                  ~left_lo:sample_lo ~left_hi:sample_hi ~right_lo:sample_lo
                  ~right_hi:sample_hi
              done);
          Sync_channel.Send.send ready_to_swap.send;
          Sync_channel.Recv.recv ready_to_gen.recv
        done)
  in
  let player =
    Domain.spawn (fun _ ->
        while true do
          Ao.play t.device (Bytes.to_string buffer.play_from);
          Sync_channel.Recv.recv ready_to_swap.recv;
          Double_buffer.swap buffer;
          Sync_channel.Send.send ready_to_gen.send
        done)
  in
  { Playing.viz_queue; generator; player }
