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

  let of_controller_table ~main_controller_table ~secondary_controller_table =
    let get = Midi.Controller_table.get_raw secondary_controller_table in
    let inv = map ~f:(fun x -> 1.0 -. x) in
    {
      lfo_frequency = get 28 |> inv |> protect;
      low_pass_filter_cutoff = get 21 |> inv |> exp_01 4.0 |> protect;
      low_pass_filter_resonance = get 22 |> protect;
      high_pass_filter_cutoff = get 23 |> exp_01 4.0 |> protect;
      high_pass_filter_resonance = get 24 |> protect;
      detune = get 25 |> protect;
      lfo_scale = get 26 |> protect;
      saturate_boost = get 27 |> protect;
      saturate_threshold =
        Midi.Controller_table.modulation main_controller_table |> inv |> protect;
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

let signal ~main_sequencer ~secondary_sequencer =
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
  let effect_clock = clock_of_frequency_hz (const 8.0) in
  let controls =
    Controls.of_controller_table ~main_controller_table
      ~secondary_controller_table
  in
  let voices =
    List.map voices
      ~f:(make_voice effect_clock pitch_wheel_multiplier (const Saw) controls)
    |> sum
  in
  let volume = const 0.2 in
  let output = voices |> fun signal -> signal *.. volume in
  output

module Args = struct
  type t = {
    list_midi_ports : bool;
    print_messages : bool;
    midi_port : int;
    serial_port : string;
    baud : int;
  }

  let parse () =
    let list_midi_ports = ref false in
    let print_messages = ref false in
    let midi_port = ref 0 in
    let serial_port = ref "" in
    let baud = ref 115200 in
    Arg.parse
      [
        ( "--list-midi-ports",
          Arg.Set list_midi_ports,
          "List midi ports by index and exit" );
        ( "--print-messages",
          Arg.Set print_messages,
          "Print each midi event to stdout" );
        ( "--midi-port",
          Arg.Set_int midi_port,
          "Use the midi port with this index" );
        ("--serial-port", Arg.Set_string serial_port, "Use this serial port");
        ("--baud", Arg.Set_int baud, "baud rate");
      ]
      (fun _ -> failwith "unexpected anonymous argument")
      "Play music with a midi keyboard";
    {
      list_midi_ports = !list_midi_ports;
      print_messages = !print_messages;
      midi_port = !midi_port;
      serial_port = !serial_port;
      baud = !baud;
    }
end

let () =
  let { Args.list_midi_ports; print_messages; midi_port; serial_port; baud } =
    Args.parse ()
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
    let midi_messages_control =
      Llama.Midi.live_midi_messages_serial ~port:serial_port ~baud
    in
    let midi_messages_control =
      if print_messages then
        Signal.debug midi_messages_control
          ~f:
            (List.iter ~f:(fun message ->
                 print_endline (Midi.Message.to_string message)))
      else midi_messages_control
    in
    let main_sequencer =
      Llama.Midi.Midi_sequencer.signal midi_messages ~channel:0 ~polyphony:12
    in
    let secondary_sequencer =
      Llama.Midi.Midi_sequencer.signal midi_messages_control ~channel:0
        ~polyphony:1
    in
    with_window ~background_rgba_01:(0.0, 0.05, 0.0, 1.0) (fun window ->
        let signal = signal ~main_sequencer ~secondary_sequencer in
        let viz'd_signal =
          visualize ~stable:true ~stride:1 ~pixel_scale:1 ~sample_scale:0.4
            ~sample_to_rgba_01:(Fun.const (0.0, 0.5, 0.0, 1.0))
            window signal
        in
        play_signal ~scale_output_volume:0.5 viz'd_signal)
