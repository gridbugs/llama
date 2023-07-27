open Llama_interactive
open Dsl

let _pretend_key = pulse ~frequency_hz:(const 2.0) ~duty_01:(const 0.1)

let mk_voice frequency gate ~preset =
  let saw_freq = frequency |> scale (Music.semitone_ratio (-19.0)) in
  let square_freq = frequency |> scale 2.0 in
  let osc =
    mean
      [ oscillator (const Saw) saw_freq; oscillator (const Square) square_freq ]
    |> chebyshev_high_pass_filter ~epsilon:(const 2.0)
         ~cutoff_hz:(sum [ const 200.0; preset |> scale 2000.0 ])
  in
  let amp_env =
    ar_linear ~gate ~attack_s:(const 0.001) ~release_s:(const 0.1)
  in
  let filter_env =
    ar_linear ~gate ~attack_s:(const 0.01) ~release_s:(const 0.01)
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 0.1)
      ~cutoff_hz:(sum [ const 1000.0; filter_env |> scale 8000.0 ])
  in
  amp_env *.. filtered_osc

let signal =
  let { Midi.Midi_sequencer.voices; pitch_wheel_multiplier; controller_table } =
    Midi.Midi_input.create ()
    |> Midi.live_midi_sequencer ~port:1 ~channel:0 ~polyphony:12
  in
  let preset = Midi.Controller_table.get controller_table 1 in
  List.map voices
    ~f:(fun { Midi.Midi_sequencer.frequency_hz; gate; velocity = _ } ->
      mk_voice (frequency_hz *.. pitch_wheel_multiplier) gate ~preset)
  |> sum

let () =
  with_window (fun window ->
      let viz'd_signal = visualize ~stride:2 ~stable:true window signal in
      play_signal ~scale_output_volume:0.1 viz'd_signal)
