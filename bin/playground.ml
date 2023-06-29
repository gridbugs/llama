open Llama
open Dsl

let c = 130.81 /. 2.0
let e = 164.81 /. 2.0

let hsc_sequence =
  [
    Some (c, 1.5);
    None;
    Some (c, 3.0);
    None;
    None;
    Some (c, 0.5);
    Some (e, 0.5);
    Some (c, 0.5);
    Some (e, 0.5);
    Some (c, 1.5);
    None;
    Some (e, 0.5);
    Some (c, 1.5);
    None;
    None;
    None;
  ]

let short_sequence = [ Some (c *. 2.0, 1.0); None ]

let make_signal () =
  let sequencer_freq = 8.0 in
  let sequencer_clock = clock (const sequencer_freq) in
  let { value = sequencer_freq; gate } =
    step_sequencer
      (List.map short_sequence
         ~f:
           (Option.map (fun (freq, period) ->
                {
                  value = const freq;
                  period_s = const (period /. sequencer_freq);
                })))
      sequencer_clock
  in
  let osc = oscillator (const Saw) sequencer_freq in
  let release_s = const 0.1 in
  let filter_env =
    adsr_linear ~gate ~attack_s:(const 0.3) ~decay_s:(const 0.2)
      ~sustain_01:(const 1.0) ~release_s
    |> exp01 1.0
    (*|> butterworth_low_pass_filter ~half_power_frequency_hz:(const 200.0) *)
    (*    |> debug ~f:(fun x -> print_endline (Printf.sprintf "%f" x)) *)
  in
  let filtered_osc =
    butterworth_low_pass_filter osc
      ~half_power_frequency_hz:(filter_env |> scale 5000.0 |> offset 100.0)
    |> butterworth_high_pass_filter ~half_power_frequency_hz:(const 200.0)
  in
  amplifier filtered_osc
    ~volume:(asr_linear ~gate ~attack_s:(const 0.01) ~release_s)

let () =
  Audio_io.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player
  := make_signal () |> amplifier ~volume:(const 0.1);
  Lwt_main.run (Signal_player.run signal_player)
