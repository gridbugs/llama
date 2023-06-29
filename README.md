# Language for Live Audio Module Arrangement

*The llama is a domesticated South American camelid.*

## Getting Started

You will need to [install rust](https://rustup.rs/) to build the low level
library that talks to your sound card.

Then you should be able to build and run the example program with:
```
dune exec ./bin/playground.exe
```

## Example Session

Start a utop session with the `Llama` module available by running `$ dune
utop`, then enter this into the utop repl.

```ocaml
open Llama.Live;;

(* Define a sequence of frequencies and durations. *)
let steps = [ Some (110.0, 0.1); Some (123.47, 0.1); Some (98.0, 0.2); None ]
|> List.map (Option.map (fun (freq, period) -> { value = const freq; period_s = const period }));;

(* Create a sequencer to play the notes. *)
let { value = freq; gate } = step_sequencer steps (clock (const 4.0));;

(* Create an oscillator to buzz at the frequency selected by the sequencer. *)
let osc = oscillator (const Saw) freq;;

(* Create an envelope generator to shape the volume according to the gate. *)
let env = asr_linear ~gate ~attack_s:(const 0.01) ~release_s:(const 0.2);;

(* Use the envelope to control the volume of the oscillator. The second call to
   `amplifier` reduces the overall volume because the output can be quite load
   otherwise. *)
let amp = amplifier osc ~volume:env |> amplifier ~volume:(const 0.5);;

(* Create a player returning a `float t ref` which can be set to the signal we want to play. *)
let out = go ();;

(* Play! *)
out := amp;;

(* To silence playback you can change the output to [silence]. *)
out := silence;;
```
