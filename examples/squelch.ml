(* Example of using filter resonance to make the iconic "squelch" effect *)

open Llama
open Dsl

let osc : float Signal.t = oscillator (const Saw) (const 80.0)

let note_clock : bool Signal.t =
  pulse ~frequency_hz:(const 4.0) ~duty_01:(const 0.5)

let envelope : float Signal.t =
  asr_linear ~gate:note_clock ~attack_s:(const 0.2) ~release_s:(const 0.2)

(* Apply some chebyshev filters to the oscillator. The [epsilon] argument
   controls how much resonance is present in the output. *)
let filtered =
  chebyshev_low_pass_filter osc ~epsilon:(const 8.0)
    ~cutoff_hz:(envelope |> scale 8000.0 |> offset 100.0)
  |> chebyshev_high_pass_filter ~epsilon:(const 1.0) ~cutoff_hz:(const 100.0)

let output : float Signal.t = filtered *.. envelope

(* Play the sound! *)
let () = play_signal (output |> scale 0.1)
