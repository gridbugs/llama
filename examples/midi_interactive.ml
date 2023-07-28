open StdLabels
open Llama_interactive
open Dsl

let make_voice _effect_clock pitch_wheel_multiplier waveform
    { Midi.Midi_sequencer.frequency_hz; gate; velocity } =
  let velocity_01 =
    map velocity ~f:(fun v -> Float.of_int v /. 127.0 |> Float.clamp_01)
  in
  let oscillator_frequency_hz = frequency_hz *.. pitch_wheel_multiplier in
  let osc =
    mean
      [
        oscillator ~pulse_width_01:(const 0.1) waveform oscillator_frequency_hz;
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
    chebyshev_low_pass_filter osc ~resonance:(const 1.0)
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

let signal (input : (bool Signal.t, float Signal.t) Input.t) midi_sequencer =
  let { Midi.Midi_sequencer.voices; pitch_wheel_multiplier; controller_table } =
    midi_sequencer
  in
  let preset =
    Midi.Controller_table.get_raw controller_table 1
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 20.0)
  in
  let effect_clock = clock (const 8.0) in
  let hold = Midi.Controller_table.get_raw controller_table 64 in
  let voices =
    List.map voices
      ~f:(make_voice effect_clock pitch_wheel_multiplier (const Saw))
    |> sum
  in
  let mouse_x = mouse_filter input.mouse.mouse_x in
  let mouse_y = mouse_filter input.mouse.mouse_y in
  voices
  |> chebyshev_low_pass_filter
       ~resonance:(mouse_y |> exp_01 4.0 |> scale 10.0)
       ~cutoff_hz:
         (const 1.0 -.. preset |> exp_01 4.0 |> scale 8000.0 |> offset 100.0)
  |> both (both mouse_x hold)
  |> map ~f:(fun ((mouse_x, hold), x) ->
         if hold > 0.5 then
           (1.0 +. (10.0 *. mouse_x)) *. x |> Float.clamp_sym ~mag:1.0
         else x)

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
  let midi_input = Llama.Midi.Midi_input.create () in
  if list_midi_ports then
    let midi_port_names = Llama.Midi.Midi_input.port_names midi_input in
    List.iteri
      ~f:(fun i name -> Printf.printf "%d: %s\n" i name)
      midi_port_names
  else
    let midi_sequencer =
      Llama.Midi.live_midi_sequencer midi_input ~port:midi_port ~channel:0
        ~polyphony:12
      |> Result.get_ok
    in
    with_window ~background_rgba_01:(0.0, 0.0, 0.2, 1.0) (fun window ->
        let signal = signal (Window.input_signals window) midi_sequencer in
        let viz'd_signal =
          visualize ~stable:true ~stride:4 ~pixel_scale:6 ~sample_scale:0.4
            ~sample_to_rgba_01:(Fun.const (0.6, 0.5, 0.2, 1.0))
            window signal
        in
        play_signal ~scale_output_volume:0.5 viz'd_signal)
