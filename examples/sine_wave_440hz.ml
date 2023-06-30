open Llama
open Dsl

let () = play_signal (oscillator (const Sine) (const 440.0) |> scale 0.1)
