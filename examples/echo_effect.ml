(* Example of applying the [echo] effect *)

open StdLabels
open Llama
open Dsl

let random_pentatonic_sequencer octave period clock =
  let pentatonic o = [ (`C, o); (`D, o); (`F, o); (`G, o); (`A, o) ] in
  let notes = List.append (pentatonic (octave + 1)) (pentatonic octave) in
  let random_sequencer_values =
    List.map notes ~f:(fun note -> const (Music.Note.frequency_hz note))
  in
  random_sequencer random_sequencer_values (const period) clock

let () =
  Random.self_init ();
  let clock_period = noise ~min:(const 0.5) ~max:(const 3.0) in
  let sequencer_clock = clock_of_period_s clock_period in
  let { value = sequencer_freq; gate } =
    random_pentatonic_sequencer 3 0.1 sequencer_clock
  in
  let osc =
    sum
      [
        oscillator ~pulse_width_01:(const 0.1) (const Pulse) sequencer_freq;
        oscillator ~pulse_width_01:(const 0.1) (const Pulse)
          (sequencer_freq |> scale 2.0);
      ]
  in
  let envelope =
    ar_linear ~gate ~attack_s:(const 0.01) ~release_s:(const 0.1)
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 1.0)
      ~cutoff_hz:(envelope |> scale 500.0 |> offset 200.0)
  in
  let env'd_osc = filtered_osc *.. envelope in
  let echo_effect signal =
    signal |> scale 0.5
    |> chebyshev_low_pass_filter ~epsilon:(const 0.8) ~cutoff_hz:(const 2000.0)
  in
  let track =
    env'd_osc
    |> echo ~f:echo_effect ~delay_s:(const 0.3)
    |> echo ~f:echo_effect ~delay_s:(const 0.7)
    |> echo ~f:echo_effect ~delay_s:(const 1.1)
    |> echo ~f:(scale 0.9) ~delay_s:(const 10.0)
    |> echo ~f:(scale 0.9) ~delay_s:(const 20.0)
    |> echo ~f:(scale 0.9) ~delay_s:(const 30.0)
  in
  let mk_track_envelope ~fade_out_start ~fade_out_duration =
    Raw.with_state' ~init:0.0 ~f:(fun time_s (ctx : Ctx.t) ->
        let time_s = time_s +. (1.0 /. ctx.sample_rate_hz) in
        let since_fade = time_s -. fade_out_start in
        let value =
          if since_fade > 0.0 then
            Float.max (fade_out_duration -. since_fade) 0.0 /. fade_out_duration
          else 1.0
        in
        (time_s, value))
    |> Signal.of_raw |> exp_01 2.0
  in
  let track_envelope =
    mk_track_envelope ~fade_out_start:60.0 ~fade_out_duration:20.0
  in
  let output =
    butterworth_low_pass_filter track
      ~half_power_frequency_hz:(track_envelope |> scale 3000.0)
    |> butterworth_high_pass_filter ~half_power_frequency_hz:(const 600.0)
  in
  let output =
    output
    +.. (scale 0.5 env'd_osc *.. (const 1.0 -.. track_envelope)
        |> echo ~f:(scale 0.5) ~delay_s:(const 0.3))
  in
  play_signal (output |> scale 0.1)
