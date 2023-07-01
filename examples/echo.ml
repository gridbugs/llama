(* Example of applying the [echo] effect *)

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
        oscillator ~square_wave_pulse_width_01:(const 0.1) (const Square)
          sequencer_freq;
        oscillator ~square_wave_pulse_width_01:(const 0.1) (const Square)
          (sequencer_freq |> scale 2.0);
      ]
  in
  let envelope =
    asr_linear ~gate ~attack_s:(const 0.01) ~release_s:(const 0.1)
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 1.0)
      ~cutoff_hz:(envelope |> scale 500.0 |> offset 200.0)
  in
  let env'd_osc = filtered_osc *.. envelope in
  let echo_effect signal =
    signal |> scale 0.5
    |> chebyshev_low_pass_filter ~epsilon:(const 0.2) ~cutoff_hz:(const 3000.0)
  in
  let output =
    echo ~f:echo_effect ~delay_s:(const 0.7) env'd_osc
    |> echo ~f:echo_effect ~delay_s:(const 1.1)
    |> echo ~f:echo_effect ~delay_s:(const 0.3)
  in
  play_signal (output |> scale 0.3)
