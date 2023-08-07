open StdLabels
open Llama_interactive
open Dsl

let make_voice frequency_hz gate =
  let detune_factor = const 1.005 in
  let oscillator_frequency_hz = frequency_hz in
  let waveform = const Saw in
  let osc =
    mean
      [
        oscillator waveform oscillator_frequency_hz;
        oscillator waveform (oscillator_frequency_hz *.. detune_factor);
        oscillator waveform (oscillator_frequency_hz /.. detune_factor);
      ]
  in
  let attack_s = const 0.05 in
  let release_s = const 0.01 in
  let amp_env = ar_linear ~gate ~attack_s ~release_s in
  lazy_amplifier osc ~volume:amp_env

let signal ~main_sequencer ~secondary_sequencer =
  let {
    Midi.Midi_sequencer.voices;
    pitch_wheel_multiplier;
    controller_table = _;
  } =
    main_sequencer
  in
  let { Midi.Midi_sequencer.controller_table; _ } = secondary_sequencer in
  let voice =
    List.map voices
      ~f:(fun { Midi.Midi_sequencer.frequency_hz; gate; velocity = _ } ->
        make_voice (frequency_hz *.. pitch_wheel_multiplier) gate)
    |> sum
  in
  let filter_cutoff =
    sum
      [
        const 100.0;
        Midi.Controller_table.get_raw controller_table 21 |> scale 2000.0;
      ]
  in
  chebyshev_low_pass_filter voice ~cutoff_hz:filter_cutoff
    ~resonance:(const 1.0)

module Args = struct
  type t = { list_midi_ports : bool; print_messages : bool; midi_port : int }

  let parse () =
    let list_midi_ports = ref false in
    let print_messages = ref false in
    let midi_port = ref 0 in
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
      ]
      (fun _ -> failwith "unexpected anonymous argument")
      "Play music with a midi keyboard";
    {
      list_midi_ports = !list_midi_ports;
      print_messages = !print_messages;
      midi_port = !midi_port;
    }
end

let () =
  let { Args.list_midi_ports; print_messages; midi_port } = Args.parse () in
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
      Llama.Midi.live_midi_messages_serial ~port:"/dev/ttyUSB0" ~baud:115200
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
