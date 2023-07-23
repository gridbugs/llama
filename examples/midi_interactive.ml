open Llama_interactive
open Dsl

let live_midi_signal midi_input =
  Signal.of_raw (fun _ ->
      let raw_data =
        Llama_low_level.Midi_input.midi_port_drain_messages_to_char_array
          midi_input
      in
      Llama_midi.Event.parse_multi_from_char_array raw_data)

let pitch_wheel_to_pitch_multiplier =
  let pitch_wheel_max = 8192.0 in
  let max_ratio = Music.semitone_ratio *. Music.semitone_ratio in
  fun pitch_wheel ->
    let pitch_wheel_1 = Int.to_float pitch_wheel /. pitch_wheel_max in
    Float.pow max_ratio pitch_wheel_1

module Controller_table = struct
  type t = float Signal.t array

  let num_controllers = 128

  let create () =
    let refs = Array.init num_controllers ~f:(fun _ -> ref 0.0) in
    let t = Array.map refs ~f:Signal.of_ref in
    (t, refs)

  let get t i = Array.get t i
end

module Sequencer = struct
  open Llama_midi

  type output_per_voice = {
    frequency_hz : float Signal.t;
    gate : bool Signal.t;
    velocity : int Signal.t;
  }

  type output = {
    per_voice : output_per_voice list;
    pitch_wheel_multiplier : float Signal.t;
    controller_table : Controller_table.t;
  }

  type voice_state = { note : int; gate : bool; velocity : int }

  let signal num_voices (track_signal : Event.t list Signal.t) =
    let voices =
      Array.init num_voices
        ~f:(Fun.const { note = 0; gate = false; velocity = 0 })
    in
    let find_free_voice_index () =
      let rec loop i =
        if i >= Array.length voices then None
        else if not (Array.get voices i).gate then Some i
        else loop (i + 1)
      in
      loop 0
    in
    let currently_playing_voice_index_by_note =
      Array.init 128 ~f:(Fun.const None)
    in
    let controller_table, controller_refs = Controller_table.create () in
    let pitch_wheel_multiplier = ref 1.0 in
    let signal_to_update_state =
      Signal.of_raw (fun ctx ->
          let voice_messages =
            Signal.sample track_signal ctx
            |> List.filter_map ~f:(fun (event : Event.t) ->
                   match event.message with
                   | Message.Channel_voice_message voice_message ->
                       Some voice_message.message
                   | _ -> None)
          in
          List.iter voice_messages ~f:(fun message ->
              match message with
              | Llama_midi.Channel_voice_message.Note_off { note; velocity }
                -> (
                  match
                    Array.get currently_playing_voice_index_by_note note
                  with
                  | None -> ()
                  | Some voice_index ->
                      Array.set currently_playing_voice_index_by_note note None;
                      let voice = Array.get voices voice_index in
                      Array.set voices voice_index
                        { voice with gate = false; velocity })
              | Note_on { note; velocity } -> (
                  match
                    Array.get currently_playing_voice_index_by_note note
                  with
                  | Some voice_index_already_assigned_to_note ->
                      (* Update the velocity *)
                      Array.set voices voice_index_already_assigned_to_note
                        { note; gate = true; velocity }
                  | None -> (
                      match find_free_voice_index () with
                      | None ->
                          (* There are no free voices for the new note *) ()
                      | Some voice_index ->
                          (* Store the mapping from note -> voice so that when the note
                             is released we turn off the right voice. *)
                          Array.set currently_playing_voice_index_by_note note
                            (Some voice_index);
                          let current_voice = Array.get voices voice_index in
                          if current_voice.gate then
                            (* Another note is still using that voice. Clear its
                               mapping from note -> voice so when the note is
                               released we don't turn off the voice. *)
                            Array.set currently_playing_voice_index_by_note
                              current_voice.note None;
                          (* Store the mapping from voice to note so if another note takes this
                             voice it can update the fact the the current note no longer holds
                             it. *)
                          Array.set voices voice_index
                            { note; gate = true; velocity }))
              | Pitch_wheel_change { signed_value } ->
                  pitch_wheel_multiplier :=
                    pitch_wheel_to_pitch_multiplier signed_value
              | Control_change { controller; value } ->
                  let ref = Array.get controller_refs controller in
                  ref := Int.to_float value /. 127.0
              | _ -> ()))
    in
    let per_voice =
      List.init ~len:num_voices ~f:(fun i ->
          let frequency_hz =
            Signal.map signal_to_update_state ~f:(fun () ->
                let { note; _ } = Array.get voices i in
                Music.frequency_hz_of_midi_index note)
          in
          let gate =
            Signal.map signal_to_update_state ~f:(fun () ->
                let { gate; _ } = Array.get voices i in
                gate)
          in
          let velocity =
            Signal.map signal_to_update_state ~f:(fun () ->
                let { velocity; _ } = Array.get voices i in
                velocity)
          in
          { frequency_hz; gate; velocity })
    in
    let pitch_wheel_multiplier = Signal.of_ref pitch_wheel_multiplier in
    { per_voice; pitch_wheel_multiplier; controller_table }
end

let make_voice _effect_clock pitch_wheel_multiplier waveform
    { Sequencer.frequency_hz; gate; velocity } =
  let velocity_01 =
    map velocity ~f:(fun v -> Float.of_int v /. 127.0 |> Float.clamp_01)
  in
  let oscillator_frequency_hz = frequency_hz *.. pitch_wheel_multiplier in
  let osc =
    mean
      [
        oscillator ~square_wave_pulse_width_01:(const 0.1) waveform
          oscillator_frequency_hz;
      ]
  in
  let attack_s = const 0.01 +.. (const 0.1 *.. (const 1.0 -.. velocity_01)) in
  let release_s = const 0.01 +.. (const 0.3 *.. (const 1.0 -.. velocity_01)) in
  let filter_env =
    velocity_01
    *.. adsr_linear ~gate ~attack_s ~decay_s:(const 1.0) ~sustain_01:(const 0.5)
          ~release_s
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 10.0)
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 1.0)
      ~cutoff_hz:(sum [ const 1000.0; filter_env |> scale 2000.0 ])
  in
  let amp_env =
    ar_linear ~gate ~attack_s ~release_s
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 10.0)
  in
  lazy_amplifier filtered_osc ~volume:amp_env

(* Removes any sharp changes from the mouse position which could cause bad
   sounds to come out of the filter controlled by the mouse *)
let mouse_filter =
  butterworth_low_pass_filter ~half_power_frequency_hz:(const 10.0)

let signal (input : (bool Signal.t, float Signal.t) Input.t) midi_input =
  let event_signal = live_midi_signal midi_input in
  let { Sequencer.per_voice; pitch_wheel_multiplier; controller_table } =
    Sequencer.signal 12 event_signal
  in
  let preset =
    Controller_table.get controller_table 1
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 20.0)
  in
  let effect_clock = clock (const 8.0) in
  let hold = Controller_table.get controller_table 64 in
  let voices =
    List.map per_voice
      ~f:(make_voice effect_clock pitch_wheel_multiplier (const Saw))
    |> sum
  in
  let mouse_x = mouse_filter input.mouse.mouse_x in
  let mouse_y = mouse_filter input.mouse.mouse_y in
  voices
  |> chebyshev_low_pass_filter
       ~epsilon:(mouse_y |> exp_01 4.0 |> scale 10.0)
       ~cutoff_hz:
         (const 1.0 -.. preset |> exp_01 4.0 |> scale 8000.0 |> offset 100.0)
  |> both (both mouse_x hold)
  |> map ~f:(fun ((mouse_x, hold), x) ->
         if hold > 0.5 then
           (1.0 +. (10.0 *. mouse_x)) *. x |> Float.clamp_sym ~mag:1.0
         else x)
(*
  |> echo ~f:echo_effect ~delay_s:(const 0.3)
  |> echo ~f:echo_effect ~delay_s:(const 0.5) *)

module Args = struct
  type t = { list_midi_ports : bool; midi_port : int }

  let parse () =
    let list_midi_ports = ref false in
    let midi_port = ref 0 in
    Arg.parse
      [
        ( "--list-midi-ports",
          Arg.Set list_midi_ports,
          "List midi ports by index and exit" );
        ( "--midi-port",
          Arg.Set_int midi_port,
          "Use the midi port with this index" );
      ]
      (fun anon_arg ->
        failwith (Printf.sprintf "Unexpected position argument: %s" anon_arg))
      "Play music with a midi keyboard";
    { list_midi_ports = !list_midi_ports; midi_port = !midi_port }
end

let () =
  let { Args.list_midi_ports; midi_port } = Args.parse () in
  let midi_input = Llama_low_level.Midi_input.create () in
  if list_midi_ports then
    let midi_port_names =
      Llama_low_level.Midi_input.midi_port_names midi_input
    in
    List.iteri
      ~f:(fun i name -> Printf.printf "%d: %s\n" i name)
      midi_port_names
  else (
    Llama_low_level.Midi_input.midi_port_connect midi_input midi_port;
    with_window ~background_rgba_01:(0.0, 0.0, 0.2, 1.0) (fun window ->
        let signal = signal (Window.input_signals window) midi_input in
        let viz'd_signal =
          visualize ~stable:true ~stride:4 ~pixel_scale:6 ~sample_scale:0.4
            ~sample_to_rgba_01:(Fun.const (0.6, 0.5, 0.2, 1.0))
            window signal
        in
        play_signal ~scale_output_volume:0.5 viz'd_signal))
