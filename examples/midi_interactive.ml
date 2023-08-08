open StdLabels
open Llama_interactive
open Dsl

module Controls = struct
  type global = {
    volume : float t;
    saturate : float t;
    low_pass_filter_cutoff : float t;
    low_pass_filter_resonance : float t;
    high_pass_filter_cutoff : float t;
    high_pass_filter_resonance : float t;
    echo_delay : float t;
    echo_scale : float t;
    noise_sah_frequency : float t;
  }

  type note = {
    attack : float t;
    decay : float t;
    sustain : float t;
    release : float t;
    filter_envelope_scale : float t;
    filter_resonance : float t;
    detune : float t;
    lfo_frequency : float t;
    lfo_scale : float t;
    noise_sah_scale : float t;
  }

  type nonrec t = { global : global; note : note }

  let protect c = butterworth_low_pass_filter c ~cutoff_hz:(const 10.0)

  let of_controller_table ~main_controller_table ~secondary_controller_table =
    let inv = map ~f:(fun x -> 1.0 -. x) in
    let freq = exp_01 1.0 in
    let get_main =
      let base = 21 in
      fun i -> Midi.Controller_table.get_raw main_controller_table (base + i)
    in
    let get_secondary =
      let base = 31 in
      fun i ->
        Midi.Controller_table.get_raw secondary_controller_table (base + i)
    in
    {
      global =
        {
          volume = Midi.Controller_table.volume main_controller_table |> protect;
          saturate =
            Midi.Controller_table.modulation main_controller_table |> protect;
          low_pass_filter_cutoff = get_main 4 |> inv |> exp_01 4.0 |> protect;
          low_pass_filter_resonance = get_main 5 |> protect;
          high_pass_filter_cutoff = get_main 6 |> exp_01 4.0 |> protect;
          high_pass_filter_resonance = get_main 7 |> protect;
          echo_delay = const 0.2;
          echo_scale = get_secondary 6 |> protect;
          noise_sah_frequency = get_secondary 1 |> freq;
        };
      note =
        {
          attack = get_main 0 |> protect;
          decay = get_main 1 |> protect;
          sustain = get_main 2 |> inv |> protect;
          release = get_main 3 |> protect;
          filter_envelope_scale = get_secondary 3 |> inv |> protect;
          filter_resonance = get_secondary 2 |> protect;
          detune = get_secondary 7 |> protect;
          lfo_frequency = get_secondary 0 |> freq |> protect;
          lfo_scale = get_secondary 4 |> protect;
          noise_sah_scale = get_secondary 5 |> protect;
        };
    }
end

let make_voice effect_clock pitch_wheel_multiplier waveform
    (controls : Controls.note)
    { Midi.Midi_sequencer.frequency_hz; gate; velocity = _ } =
  let lfo =
    low_frequency_oscillator_01 (const Sine)
      (controls.lfo_frequency |> scale 40.0)
      (Gate.to_trigger gate)
  in
  let noise = noise_1 () in
  let sah =
    sample_and_hold noise effect_clock
    |> butterworth_low_pass_filter ~cutoff_hz:(const 100.0)
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
  let env_adj s = s |> scale 2.0 |> offset 0.1 in
  let filter_env =
    adsr_linear ~gate
      ~attack_s:(controls.attack |> env_adj)
      ~decay_s:(controls.decay |> env_adj)
      ~sustain_01:controls.sustain
      ~release_s:(controls.release |> env_adj)
    |> exp_01 2.0
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc
      ~resonance:(controls.filter_resonance |> scale 10.0)
      ~cutoff_hz:
        (sum
           [
             filter_env *.. controls.filter_envelope_scale |> scale 8000.0;
             lfo *.. controls.lfo_scale |> scale 8000.0;
             sah *.. controls.noise_sah_scale |> scale 4000.0;
           ]
        |> map ~f:(fun x -> Float.max x 100.0))
  in
  let amp_env =
    ar_linear ~gate ~attack_s:(const 0.01)
      ~release_s:(controls.release |> env_adj)
  in
  lazy_amplifier filtered_osc ~volume:amp_env

(* Removes any sharp changes from the mouse position which could cause bad
   sounds to come out of the filter controlled by the mouse *)
let mouse_filter = butterworth_low_pass_filter ~cutoff_hz:(const 10.0)

let signal (input : (_, _) Input.t) ~main_sequencer ~secondary_sequencer
    ~pad_gates ~wav_players ~echo_effect =
  let {
    Midi.Midi_sequencer.voices;
    pitch_wheel_multiplier;
    controller_table = main_controller_table;
  } =
    main_sequencer
  in
  let { Midi.Midi_sequencer.controller_table = secondary_controller_table; _ } =
    secondary_sequencer
  in
  let { Controls.global = global_controls; note = note_controls } =
    Controls.of_controller_table ~main_controller_table
      ~secondary_controller_table
  in
  let effect_clock =
    clock_of_frequency_hz
      (global_controls.noise_sah_frequency |> scale 10.0 |> offset 1.0)
  in
  let voices =
    List.map voices
      ~f:
        (make_voice effect_clock pitch_wheel_multiplier (const Saw)
           note_controls)
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
  let volume = Midi.Controller_table.volume main_controller_table in
  let output = voices +.. drums |> fun signal -> signal *.. volume in
  if echo_effect then output |> echo ~f:(scale 0.8) ~delay_s:(const 0.2)
  else output

module Args = struct
  type t = {
    list_midi_ports : bool;
    echo_effect : bool;
    print_messages : bool;
    midi_port : int;
    sample_paths : string list;
    serial_port : string;
    baud : int;
  }

  let parse () =
    let list_midi_ports = ref false in
    let echo_effect = ref false in
    let print_messages = ref false in
    let midi_port = ref 0 in
    let sample_paths = ref [] in
    let serial_port = ref "" in
    let baud = ref 115200 in
    Arg.parse
      [
        ( "--list-midi-ports",
          Arg.Set list_midi_ports,
          "List midi ports by index and exit" );
        ("--echo-effect", Arg.Set echo_effect, "Use echo effect");
        ( "--print-messages",
          Arg.Set print_messages,
          "Print each midi event to stdout" );
        ( "--midi-port",
          Arg.Set_int midi_port,
          "Use the midi port with this index" );
        ("--serial-port", Arg.Set_string serial_port, "Use this serial port");
        ("--baud", Arg.Set_int baud, "baud rate");
      ]
      (fun anon_arg -> sample_paths := anon_arg :: !sample_paths)
      "Play music with a midi keyboard";
    {
      list_midi_ports = !list_midi_ports;
      echo_effect = !echo_effect;
      print_messages = !print_messages;
      midi_port = !midi_port;
      sample_paths = List.rev !sample_paths;
      serial_port = !serial_port;
      baud = !baud;
    }
end

let () =
  let {
    Args.list_midi_ports;
    echo_effect;
    print_messages;
    midi_port;
    sample_paths;
    serial_port;
    baud;
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
    let midi_messages =
      Llama.Midi.live_midi_signal_messages midi_input midi_port |> Result.get_ok
    in
    let midi_messages =
      if print_messages then
        Signal.debug midi_messages
          ~f:
            (List.iter ~f:(fun message ->
                 print_endline
                   (Printf.sprintf "MIDI: %s" (Midi.Message.to_string message))))
      else midi_messages
    in
    let midi_messages_serial =
      Llama.Midi.live_midi_messages_serial ~port:serial_port ~baud
    in
    let midi_messages_serial =
      if print_messages then
        Signal.debug midi_messages_serial
          ~f:
            (List.iter ~f:(fun message ->
                 print_endline
                   (Printf.sprintf "MIDI Serial: %s"
                      (Midi.Message.to_string message))))
      else midi_messages_serial
    in
    let main_sequencer =
      Llama.Midi.Midi_sequencer.signal midi_messages ~channel:0 ~polyphony:12
    in
    let secondary_sequencer =
      Llama.Midi.Midi_sequencer.signal midi_messages_serial ~channel:0
        ~polyphony:1
    in
    let pad_gates =
      Llama.Midi.Midi_sequencer.key_gates ~channel:9 midi_messages
    in
    with_window ~background_rgba_01:(0.0, 0.05, 0.0, 1.0) (fun window ->
        let signal =
          signal
            (Window.input_signals window)
            ~main_sequencer ~secondary_sequencer ~pad_gates ~wav_players
            ~echo_effect
        in
        let viz'd_signal =
          visualize ~stable:true ~stride:1 ~pixel_scale:1 ~sample_scale:0.4
            ~sample_to_rgba_01:(Fun.const (0.0, 0.5, 0.0, 1.0))
            window signal
        in
        play_signal ~scale_output_volume:0.5 viz'd_signal)
