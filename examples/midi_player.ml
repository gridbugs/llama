open Llama_interactive
open Llama_midi
open Dsl

let track_signal (track : Track.t) clock =
  let event_array = Array.of_list track in
  let current_index = ref 0 in
  let next_time = ref 0 in
  Signal.of_raw (fun ctx ->
      if !current_index >= Array.length event_array then []
      else if Signal.sample clock ctx then (
        let current_time = !next_time in
        next_time := current_time + 1;
        let next_event = Array.get event_array !current_index in
        if current_time == next_event.delta_time then (
          current_index := !current_index + 1;
          let rec loop acc =
            if !current_index >= Array.length event_array then acc
            else
              let next_event = Array.get event_array !current_index in
              if next_event.delta_time == 0 then (
                current_index := !current_index + 1;
                loop (next_event :: acc))
              else (
                next_time := 0;
                acc)
          in
          List.rev (loop [ next_event ]))
        else [])
      else [])

module Sequencer = struct
  type output_per_voice = {
    frequency_hz : float Signal.t;
    gate : bool Signal.t;
  }

  type voice_state = { note : int; gate : bool }

  let signal num_voices (track_signal : Event.t list Signal.t) =
    let voices =
      Array.init num_voices ~f:(Fun.const { note = 0; gate = false })
    in
    let currently_playing_voice_index_by_note =
      Array.init 128 ~f:(Fun.const None)
    in
    let next_voice_index = ref 0 in
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
          List.iter voice_messages ~f:(function
            | Llama_midi.Channel_voice_message.Note_off { note; _ } -> (
                match Array.get currently_playing_voice_index_by_note note with
                | None -> ()
                | Some voice_index ->
                    Array.set currently_playing_voice_index_by_note note None;
                    let voice = Array.get voices voice_index in
                    Array.set voices voice_index { voice with gate = false })
            | Llama_midi.Channel_voice_message.Note_on { note; _ } -> (
                match Array.get currently_playing_voice_index_by_note note with
                | Some _voice_already_assigned_to_note -> ()
                | None ->
                    let voice_index = !next_voice_index in
                    next_voice_index := (voice_index + 1) mod num_voices;
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
                    Array.set voices voice_index { note; gate = true })
            | _ -> ()))
    in
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
        { frequency_hz; gate })
end

let midi_signal (data : Llama_midi.Data.t) =
  let clock = clock (const 1000.0) in
  let track = List.hd data.tracks in
  let midi_event_signal = track_signal track clock in
  let sequencer_output = Sequencer.signal 40 midi_event_signal in
  let voices =
    List.map sequencer_output ~f:(fun { Sequencer.frequency_hz; gate } ->
        let oscillator_frequency_hz = frequency_hz |> scale 0.5 in
        let osc =
          mean
            [
              oscillator (const Saw) oscillator_frequency_hz;
              oscillator ~square_wave_pulse_width_01:(const 0.2) (const Saw)
                (oscillator_frequency_hz |> scale 2.0)
              |> scale 0.5;
              oscillator ~square_wave_pulse_width_01:(const 0.2) (const Saw)
                (oscillator_frequency_hz |> scale 4.0)
              |> scale 0.25;
            ]
        in
        let release_s = const 0.1 in
        let filter_env =
          adsr_linear ~gate ~attack_s:(const 0.1) ~decay_s:(const 1.0)
            ~sustain_01:(const 0.2) ~release_s
          |> exp_01 1.0
        in
        let filtered_osc =
          chebyshev_low_pass_filter osc ~epsilon:(const 4.0)
            ~cutoff_hz:(sum [ const 100.0; filter_env |> scale 12000.0 ])
        in
        filtered_osc
        |> lazy_amplifier
             ~volume:(asr_linear ~gate ~attack_s:(const 0.001) ~release_s))
    |> sum
  in
  voices

let mouse_filter =
  butterworth_low_pass_filter ~half_power_frequency_hz:(const 10.0)

let midi_signal_with_effects (input : (bool Signal.t, float Signal.t) Input.t)
    data =
  let mouse_x = mouse_filter input.mouse.mouse_x in
  let mouse_y = mouse_filter input.mouse.mouse_y in
  midi_signal data
  |> chebyshev_low_pass_filter ~epsilon:(const 2.0)
       ~cutoff_hz:(mouse_x |> exp_01 4.0 |> scale 10000.0 |> offset 100.0)
  |> chebyshev_high_pass_filter ~epsilon:(const 4.0)
       ~cutoff_hz:(mouse_y |> exp_01 4.0 |> scale 2000.0 |> offset 100.0)

module Args = struct
  type t = { midi_file_path : string }

  let usage () =
    let arg0 =
      if Array.length Sys.argv < 1 then "midi.exe" else Array.get Sys.argv 0
    in
    Printf.sprintf "USAGE:\n%s FILE\n\nPlay a midi file.\n" arg0

  let parse () =
    let anon_args = ref [] in
    let spec = [] in
    Arg.parse spec
      (fun anon_arg -> anon_args := anon_arg :: !anon_args)
      (usage ());
    match !anon_args with
    | [ midi_file_path ] -> { midi_file_path }
    | [] ->
        Printf.eprintf "Missing midi file path!\n\n%s"
          (Arg.usage_string spec (usage ()));
        exit 1
    | _ ->
        Printf.eprintf "Too many anonymous arguments!\n\n%s"
          (Arg.usage_string spec (usage ()));
        exit 1
end

let () =
  with_window ~background_rgba_01:(0.0, 0.0, 0.2, 1.0) (fun window ->
      let { Args.midi_file_path } = Args.parse () in
      let reader = Llama_midi.File_reader.of_path midi_file_path in
      let data = Llama_midi.File_reader.read reader in
      let signal =
        midi_signal_with_effects (Window.input_signals window) data
      in
      let viz'd_signal =
        visualize ~stable:false ~stride:4 ~pixel_scale:6 ~sample_scale:0.2
          ~sample_to_rgba_01:(Fun.const (0.6, 0.5, 0.2, 1.0))
          window signal
      in
      play_signal ~scale_output_volume:0.01 viz'd_signal)
