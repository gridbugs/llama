open StdLabels
open Llama_interactive
open Llama.Midi
open Dsl

let midi_signal (data : Llama_midi.Data.t) ~mouse_x ~mouse_y =
  let clock = clock_of_frequency_hz (const 1000.0) in
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
              oscillator ~pulse_width_01:(const 0.2) (const Saw)
                (oscillator_frequency_hz |> scale 2.0)
              |> scale 0.5;
              oscillator ~pulse_width_01:(const 0.2) (const Saw)
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
          chebyshev_low_pass_filter osc ~resonance:mouse_y
            ~cutoff_hz:
              (sum [ const 100.0; filter_env *.. mouse_x |> scale 12000.0 ])
        in
        filtered_osc
        |> lazy_amplifier
             ~volume:(ar_linear ~gate ~attack_s:(const 0.001) ~release_s))
    |> sum
  in
  voices

let mouse_filter = butterworth_low_pass_filter ~cutoff_hz:(const 10.0)

let midi_signal_with_effects (input : (_, _) Input.t) data =
  let mouse_x = mouse_filter input.mouse.mouse_x in
  let mouse_y = mouse_filter input.mouse.mouse_y in
  midi_signal data ~mouse_x ~mouse_y

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
  with_visualization_window ~background_rgba_01:(0.0, 0.0, 0.2, 1.0)
    ~stable:false ~stride:1 ~pixel_scale:1 ~sample_scale:1.
    ~sample_to_rgba_01:(Fun.const (0.6, 0.5, 0.2, 1.0))
    (fun window ->
      let reader = Llama_midi.File_reader.of_path midi_file_path in
      let data = Llama_midi.File_reader.read reader in
      midi_signal_with_effects (Window.input_signals window) data |> scale 0.1)
