open Llama
open Dsl

let c3 = 130.81
let e3 = 164.81

let hsc_sequence =
  [
    Some (c3, 1.5);
    None;
    Some (c3, 3.0);
    None;
    None;
    Some (c3, 0.5);
    Some (e3, 0.5);
    Some (c3, 0.5);
    Some (e3, 0.5);
    Some (c3, 1.5);
    None;
    Some (e3, 1.5);
    Some (c3, 0.5);
    None;
    None;
    None;
  ]

let make_signal () =
  let sequencer_freq = 8.0 in
  let sequencer_clock = clock (const sequencer_freq) in
  let { value = sequencer_freq; gate } =
    step_sequencer
      (List.map hsc_sequence
         ~f:
           (Option.map (fun (freq, period) ->
                {
                  value = const freq;
                  period_s = const (period /. sequencer_freq);
                })))
      sequencer_clock
  in
  let osc = oscillator (const Saw) sequencer_freq in
  let env = asr_linear ~gate ~attack_s:(const 0.1) ~release_s:(const 1.0) in
  amplifier osc ~volume:env

let () =
  Audio_io.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player
  := make_signal () |> amplifier ~volume:(const 0.1);
  Lwt_main.run (Signal_player.run signal_player)
