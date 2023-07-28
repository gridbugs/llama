open StdLabels
open Llama_interactive
open Dsl

let mk_voice frequency gate ~preset =
  let saw_freq = frequency |> scale (Music.semitone_ratio (-19.0)) in
  let square_freq = frequency |> scale 2.0 in
  let osc =
    mean
      [ oscillator (const Saw) saw_freq; oscillator (const Pulse) square_freq ]
    |> chebyshev_high_pass_filter ~resonance:(const 2.0)
         ~cutoff_hz:(sum [ const 200.0; preset |> scale 2000.0 ])
  in
  let amp_env =
    ar_linear ~gate ~attack_s:(const 0.001) ~release_s:(const 0.1)
  in
  let filter_env =
    ar_linear ~gate ~attack_s:(const 0.01) ~release_s:(const 0.01)
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~resonance:(const 0.1)
      ~cutoff_hz:(sum [ const 1000.0; filter_env |> scale 4000.0 ])
  in
  lazy_amplifier ~volume:amp_env filtered_osc

let signal_midi =
  let port = 1 in
  match
    Midi.Midi_input.create ()
    |> Midi.live_midi_sequencer ~port ~channel:0 ~polyphony:12
  with
  | Ok { Midi.Midi_sequencer.voices; pitch_wheel_multiplier; controller_table }
    ->
      let preset = Midi.Controller_table.get_raw controller_table 1 in
      List.map voices
        ~f:(fun { Midi.Midi_sequencer.frequency_hz; gate; velocity = _ } ->
          mk_voice (frequency_hz *.. pitch_wheel_multiplier) gate ~preset)
      |> sum
  | Error `No_such_port ->
      Printf.eprintf "No such midi port: %d\n" port;
      silence

let signal_sdl (input : (_, _) Input.t) =
  let mouse_x =
    input.mouse.mouse_x
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 10.0)
  in
  let mouse_y =
    input.mouse.mouse_y
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 10.0)
  in
  let preset = mouse_x in
  Keyboard_helper.voices ~keys:input.keyboard ~mid_note:(`C, 3)
  |> List.map ~f:(fun { Keyboard_helper.Voice.frequency_hz; gate } ->
         mk_voice (const frequency_hz) gate ~preset)
  |> sum
  |> chebyshev_low_pass_filter ~resonance:(const 2.0)
       ~cutoff_hz:(sum [ const 500.0; mouse_y |> scale 8000.0 ])
(*|> map ~f:(fun x -> x *. 2.0 |> Float.clamp_sym ~mag:1.0) *)

let signal input = signal_midi +.. signal_sdl input

let () =
  with_window (fun window ->
      let viz'd_signal =
        visualize ~stride:2 ~stable:true ~pixel_scale:2 window
          (signal (Window.input_signals window))
      in
      play_signal ~scale_output_volume:0.1 viz'd_signal)
