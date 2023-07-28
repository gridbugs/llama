open StdLabels
open Llama_interactive
open Dsl

let mk_voice gate freq_hz effect_clock =
  let sah_noise = sample_and_hold (noise_01 ()) effect_clock in
  let lfo =
    low_frequency_oscillator (const Sine) (const 0.2) (Gate.to_trigger gate)
  in
  let freq_signal = const freq_hz in
  let osc =
    mean
      [
        oscillator (const Saw) freq_signal;
        oscillator (const Saw) (freq_signal |> scale 2.0);
      ]
  in
  let release_s = const 0.1 in
  let filter_env =
    adsr_linear ~gate ~attack_s:(const 0.2) ~decay_s:(const 0.1)
      ~sustain_01:(const 1.0) ~release_s
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~resonance:(const 5.0)
      ~cutoff_hz:
        (sum
           [
             const 100.0;
             filter_env |> scale 1000.0;
             sah_noise |> scale 500.0;
             lfo |> scale 500.0;
           ])
  in
  let amp_env =
    ar_linear ~gate ~attack_s:(const 0.01) ~release_s |> exp_01 1.0
  in
  lazy_amplifier filtered_osc ~volume:amp_env
  |> map ~f:(fun x -> Float.clamp_sym ~mag:2.0 (x *. 10.0))

let mk_voices (keys : _ Input.All_keyboard.t) =
  let effect_clock = clock_of_frequency_hz (const 8.0) in
  Keyboard_helper.voices ~keys ~mid_note:(`C, 4)
  |> List.map ~f:(fun { Keyboard_helper.Voice.frequency_hz; gate } ->
         mk_voice gate frequency_hz effect_clock)
  |> sum

(* Removes any sharp changes from the mouse position which could cause bad
   sounds to come out of the filter controlled by the mouse *)
let mouse_filter = butterworth_low_pass_filter ~cutoff_hz:(const 10.0)

let mk_synth (input : (_, _) Input.t) =
  let mouse_x = mouse_filter input.mouse.mouse_x in
  let mouse_y = mouse_filter input.mouse.mouse_y in
  let echo_effect signal = signal |> scale 0.6 in
  mk_voices input.keyboard
  |> chebyshev_low_pass_filter
       ~resonance:(mouse_y |> exp_01 1.0 |> scale 10.0)
       ~cutoff_hz:(mouse_x |> exp_01 4.0 |> scale 8000.0 |> offset 100.0)
  |> echo ~f:echo_effect ~delay_s:(const 0.3)
  |> echo ~f:echo_effect ~delay_s:(const 0.5)

let () =
  with_window (fun window ->
      let signal = mk_synth (Window.input_signals window) in
      let viz'd_signal =
        visualize ~stable:true ~stride:4 ~sample_scale:0.05
          ~sample_to_rgba_01:(fun x ->
            let g = 0.0 in
            let rb = 0.2 +. Float.abs x in
            (rb, g, rb, 1.0))
          window signal
      in
      play_signal ~scale_output_volume:0.1 viz'd_signal)
