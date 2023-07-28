open StdLabels
open Llama_interactive
open Dsl

module Controls = struct
  type nonrec t = {
    lfo_frequency : float t;
    lfo_scale : float t;
    low_pass_filter_cutoff : float t;
    low_pass_filter_resonance : float t;
    high_pass_filter_cutoff : float t;
    high_pass_filter_resonance : float t;
    detune : float t;
    saturate_boost : float t;
    saturate_threshold : float t;
  }

  let protect c = butterworth_low_pass_filter c ~cutoff_hz:(const 10.0)

  let of_controller_table ct =
    let get = Midi.Controller_table.get_raw ct in
    let inv = map ~f:(fun x -> 1.0 -. x) in
    {
      lfo_frequency = Midi.Controller_table.modulation ct |> inv |> protect;
      low_pass_filter_cutoff = get 21 |> inv |> exp_01 4.0 |> protect;
      low_pass_filter_resonance = get 22 |> protect;
      high_pass_filter_cutoff = get 23 |> exp_01 4.0 |> protect;
      high_pass_filter_resonance = get 24 |> protect;
      detune = get 25 |> protect;
      lfo_scale = get 26 |> protect;
      saturate_boost = get 27 |> protect;
      saturate_threshold = get 28 |> inv |> protect;
    }
end

let make_voice _effect_clock pitch_wheel_multiplier waveform
    (controls : Controls.t) { Midi.Midi_sequencer.frequency_hz; gate; velocity }
    =
  let velocity_01 =
    map velocity ~f:(fun v -> Float.of_int v /. 127.0 |> Float.clamp_01)
  in
  let lfo =
    low_frequency_oscillator_01 (const Sine)
      (const 16.0 /.. (controls.lfo_frequency |> scale 64.0 |> offset 1.0))
      (Gate.to_trigger gate)
  in
  let detune_factor = controls.detune |> scale 0.02 |> offset 1.0 in
  let oscillator_frequency_hz = frequency_hz *.. pitch_wheel_multiplier in
  let osc =
    mean
      [
        oscillator waveform oscillator_frequency_hz;
        oscillator waveform (oscillator_frequency_hz *.. detune_factor);
        oscillator waveform (oscillator_frequency_hz /.. detune_factor);
      ]
  in
  let attack_s = const 0.01 +.. (const 0.01 *.. (const 1.0 -.. velocity_01)) in
  let release_s = const 0.01 +.. (const 0.03 *.. (const 1.0 -.. velocity_01)) in
  let filter_env =
    adsr_linear ~gate ~attack_s ~decay_s:(const 0.5) ~sustain_01:(const 0.1)
      ~release_s
    |> exp_01 1.0
  in
  let filtered_osc =
    osc
    |> chebyshev_high_pass_filter
         ~resonance:(controls.high_pass_filter_resonance |> scale 10.0)
         ~cutoff_hz:
           (sum
              [ const 20.0; controls.high_pass_filter_cutoff |> scale 8000.0 ])
    |> chebyshev_low_pass_filter
         ~resonance:(controls.low_pass_filter_resonance |> scale 10.0)
         ~cutoff_hz:
           (sum
              [
                const 100.0;
                filter_env |> scale 2000.0;
                controls.low_pass_filter_cutoff |> scale 8000.0;
                lfo *.. controls.lfo_scale |> scale 8000.0;
              ])
  in
  let amp_env = ar_linear ~gate ~attack_s ~release_s in
  lazy_amplifier filtered_osc ~volume:amp_env
  |> saturate
       ~boost:(controls.saturate_boost |> scale 4.0 |> offset 1.0)
       ~threshold:(controls.saturate_threshold |> scale 4.0)

(* Removes any sharp changes from the mouse position which could cause bad
   sounds to come out of the filter controlled by the mouse *)
let mouse_filter = butterworth_low_pass_filter ~cutoff_hz:(const 10.0)

let signal (input : (_, _) Input.t) ~main_sequencer ~pad_gates ~wav_players
    ~echo_effect =
  let { Midi.Midi_sequencer.voices; pitch_wheel_multiplier; controller_table } =
    main_sequencer
  in
  let effect_clock = clock_of_frequency_hz (const 8.0) in
  let controls = Controls.of_controller_table controller_table in
  let voices =
    List.map voices
      ~f:(make_voice effect_clock pitch_wheel_multiplier (const Saw) controls)
    |> sum
  in
  let drums =
    List.mapi wav_players ~f:(fun i wav_player ->
        let trigger =
          Midi.Gate_table.get pad_gates (i + 36) |> Gate.to_trigger
        in
        wav_player trigger)
    |> sum
  in
  let _mouse_x = mouse_filter input.mouse.mouse_x in
  let _mouse_y = mouse_filter input.mouse.mouse_y in
  let volume = Midi.Controller_table.volume controller_table in
  let output = voices +.. drums |> fun signal -> signal *.. volume in
  if echo_effect then output |> echo ~f:(scale 0.8) ~delay_s:(const 0.2)
  else output

module Args = struct
  type t = {
    list_midi_ports : bool;
    echo_effect : bool;
    print_events : bool;
    midi_port : int;
    sample_paths : string list;
  }

  let parse () =
    let list_midi_ports = ref false in
    let echo_effect = ref false in
    let print_events = ref false in
    let midi_port = ref 0 in
    let sample_paths = ref [] in
    Arg.parse
      [
        ( "--list-midi-ports",
          Arg.Set list_midi_ports,
          "List midi ports by index and exit" );
        ("--echo-effect", Arg.Set echo_effect, "Use echo effect");
        ( "--print-events",
          Arg.Set print_events,
          "Print each midi event to stdout" );
        ( "--midi-port",
          Arg.Set_int midi_port,
          "Use the midi port with this index" );
      ]
      (fun anon_arg -> sample_paths := anon_arg :: !sample_paths)
      "Play music with a midi keyboard";
    {
      list_midi_ports = !list_midi_ports;
      echo_effect = !echo_effect;
      print_events = !print_events;
      midi_port = !midi_port;
      sample_paths = List.rev !sample_paths;
    }
end

let () =
  let {
    Args.list_midi_ports;
    echo_effect;
    print_events;
    midi_port;
    sample_paths;
  } =
    Args.parse ()
  in
  let wav_players =
    List.map sample_paths ~f:(fun sample_path ->
        Llama_io.Wav.(of_file_at_path sample_path |> sample_player_mono))
  in
  let midi_input = Llama.Midi.Midi_input.create () in
  if list_midi_ports then
    let midi_port_names = Llama.Midi.Midi_input.port_names midi_input in
    List.iteri
      ~f:(fun i name -> Printf.printf "%d: %s\n" i name)
      midi_port_names
  else
    let midi_events =
      Llama.Midi.live_midi_signal midi_input midi_port |> Result.get_ok
    in
    let midi_events =
      if print_events then
        Signal.debug midi_events
          ~f:
            (List.iter ~f:(fun event ->
                 print_endline (Midi.Event.to_string event)))
      else midi_events
    in
    let main_sequencer =
      Llama.Midi.Midi_sequencer.signal midi_events ~channel:0 ~polyphony:12
    in
    let pad_gates =
      Llama.Midi.Midi_sequencer.key_gates ~channel:9 midi_events
    in
    with_window ~background_rgba_01:(0.0, 0.05, 0.0, 1.0) (fun window ->
        let signal =
          signal
            (Window.input_signals window)
            ~main_sequencer ~pad_gates ~wav_players ~echo_effect
        in
        let viz'd_signal =
          visualize ~stable:true ~stride:4 ~pixel_scale:6 ~sample_scale:0.4
            ~sample_to_rgba_01:(Fun.const (0.0, 0.5, 0.0, 1.0))
            window signal
        in
        play_signal ~scale_output_volume:0.5 viz'd_signal)
