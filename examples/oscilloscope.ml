open Llama_interactive
open Dsl

let hsv_to_rgb h s v =
  if Float.equal s 0.0 then (v, v, v)
  else
    let h = h /. 60.0 in
    let i = Float.to_int h in
    let f = h -. Int.to_float i in
    let p = v *. (1.0 -. s) in
    let q = v *. (1.0 -. (s *. f)) in
    let t = v *. (1.0 -. (s *. (1.0 -. f))) in
    match i with
    | 0 -> (v, t, p)
    | 1 -> (q, v, p)
    | 2 -> (p, v, t)
    | 3 -> (p, q, v)
    | 4 -> (t, p, v)
    | _ -> (v, p, q)

let waveform_sequencer =
  value_sequencer
    [ const Sine; const Triangle; const Saw; const Pulse; const Noise ]

let () =
  let clock = clock_of_period_s (const 1.0) in
  let waveform = waveform_sequencer clock in
  let lfo =
    low_frequency_oscillator_01 (const Triangle) (const 0.1)
      Signal.Trigger.never
    |> scale 2000.0 |> offset 200.0
  in
  let signal = oscillator waveform lfo in
  play_signal_visualized ~background_rgba_01:(0.1, 0.1, 0.1, 1.0)
    ~sample_to_rgba_01:(fun x ->
      let hue = 240. +. (x *. 60.0) in
      let r, g, b = hsv_to_rgb hue 0.5 0.9 in
      (r, g, b, 1.0))
    ~pixel_scale:16 ~scale_output_volume:0.1 ~stable:true signal
