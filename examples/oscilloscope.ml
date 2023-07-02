open Llama
open Llama_graphical
open Dsl

let () =
  with_window (fun window ->
      let signal = oscillator (const Sine) (const 440.0) in
      let viz'd_signal = visualize window signal in
      play_signal_lwt (viz'd_signal |> scale 0.1))
