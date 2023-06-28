module Ctx = Signal.Ctx

module Oscillator = struct
  type waveform = Sine | Saw
  type t = { waveform : waveform Signal.t; frequency_hz : float Signal.t }

  let raw t =
    let state = ref 0.0 in
    fun (ctx : Ctx.t) ->
      let state_delta =
        Signal.sample t.frequency_hz ctx /. ctx.sample_rate_hz
      in
      let new_state = Float.rem (!state +. state_delta) 1.0 in
      state := new_state;
      match Signal.sample t.waveform ctx with
      | Sine -> Float.sin (new_state *. Float.pi *. 2.0)
      | Saw -> (new_state *. 2.0) -. 1.0

  let signal t = Signal.of_raw (raw t)
end
