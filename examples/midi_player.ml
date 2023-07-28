open StdLabels
open Llama_interactive
open Llama.Midi
open Dsl

let midi_signal (data : Llama_midi.Data.t) =
  let clock = clock (const 1000.0) in
  let track = List.hd data.tracks in
  let midi_event_signal = track_signal track clock in
  let sequencer_output =
    Midi_sequencer.signal ~channel:0 ~polyphony:64 midi_event_signal
  in
  let voices =
    List.map sequencer_output.voices
      ~f:(fun { Midi_sequencer.frequency_hz; gate; velocity = _ } ->
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
             ~volume:(ar_linear ~gate ~attack_s:(const 0.001) ~release_s))
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
  let { Args.midi_file_path } = Args.parse () in
  with_window ~background_rgba_01:(0.0, 0.0, 0.2, 1.0) (fun window ->
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
