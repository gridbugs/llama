open Llama_interactive
open Dsl

let pretend_key = pulse ~frequency_hz:(const 2.0) ~duty_01:(const 0.1)

let mk_voice frequency gate =
  let envelope =
    ar_linear ~gate ~attack_s:(const 0.05) ~release_s:(const 0.2) |> exp_01 1.0
  in
  envelope *.. oscillator (const Saw) frequency

let signal =
  let { Midi.Midi_sequencer.voices; pitch_wheel_multiplier; controller_table } =
    Midi.Midi_input.create ()
    |> Midi.live_midi_sequencer ~port:1 ~channel:0 ~polyphony:12
  in
  let preset = Midi.Controller_table.get controller_table 1 in
  List.map voices
    ~f:(fun { Midi.Midi_sequencer.frequency_hz; gate; velocity = _ } ->
      mk_voice (frequency_hz *.. pitch_wheel_multiplier) gate)
  |> sum
  |> chebyshev_low_pass_filter ~epsilon:(const 5.0)
       ~cutoff_hz:((const 1.0 -.. preset) *.. const 5000.0)
  |> map ~f:(fun x -> x *. 2.0 |> Float.clamp_sym ~mag:1.0)
  |> echo ~f:(scale 0.5) ~delay_s:(const 0.4)

let () =
  with_window (fun window ->
      let viz'd_signal = visualize ~stride:2 ~stable:true window signal in
      play_signal ~scale_output_volume:0.1 viz'd_signal)
