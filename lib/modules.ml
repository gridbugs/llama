module Ctx = Signal.Ctx
module Raw = Signal.Raw

let sweep01 frequency_hz =
  Raw.with_state ~init:0.0 ~f:(fun state ctx ->
      let state_delta = Signal.sample frequency_hz ctx /. ctx.sample_rate_hz in
      Float.rem (state +. state_delta) 1.0)

module Oscillator = struct
  type waveform = Sine | Saw
  type t = { waveform : waveform Signal.t; frequency_hz : float Signal.t }

  let signal t =
    sweep01 t.frequency_hz
    |> Raw.bind ~f:(fun state ctx ->
           match Signal.sample t.waveform ctx with
           | Sine -> Float.sin (state *. Float.pi *. 2.0)
           | Saw -> (state *. 2.0) -. 1.0)
    |> Signal.of_raw
end

module Clock = struct
  type t = { frequency_hz : float Signal.t }

  let signal t =
    sweep01 t.frequency_hz
    |> Raw.map ~f:(fun state -> state < 0.5)
    |> Signal.of_raw |> Signal.trigger
end

module Amplifier = struct
  type t = { signal : float Signal.t; volume : float Signal.t }

  (* As an optimization, if the volume is less than this value, the signal is
     not evaluated. *)
  let threshold = 1.0 /. 64.0

  let raw t ctx =
    let volume = Signal.sample t.volume ctx in
    if Float.abs volume > threshold then Signal.sample t.signal ctx *. volume
    else 0.0

  let signal t = Signal.of_raw (raw t)
end

module Asr_linear = struct
  type t = {
    gate : bool Signal.t;
    attack_s : float Signal.t;
    release_s : float Signal.t;
  }

  let signal t =
    Raw.with_state ~init:0.0 ~f:(fun state ctx ->
        let delta =
          if Signal.sample t.gate ctx then
            1.0 /. (Signal.sample t.attack_s ctx *. ctx.sample_rate_hz)
          else -1.0 /. (Signal.sample t.release_s ctx *. ctx.sample_rate_hz)
        in
        Float.clamp01 (state +. delta))
    |> Signal.of_raw
end

module Step_sequencer = struct
  type step = { value : float Signal.t; period_s : float Signal.t }
  type t = { sequence : step option list; clock : bool Signal.t }
  type state = { step_index : int; gate_remain_s : float }
  type output = { value : float Signal.t; gate : bool Signal.t }

  let init_state t =
    { step_index = List.length t.sequence; gate_remain_s = 0.0 }

  let signal t =
    let sequence_array = Array.of_list t.sequence in
    let sequence_length = List.length t.sequence in
    let combined =
      Raw.with_state ~init:(init_state t) ~f:(fun state ctx ->
          let gate_remain_s_delta = 1.0 /. ctx.sample_rate_hz in
          if Signal.sample t.clock ctx then
            let step_index = (state.step_index + 1) mod sequence_length in
            let gate_remain_s =
              match Array.get sequence_array step_index with
              | Some current_step ->
                  Signal.sample current_step.period_s ctx -. gate_remain_s_delta
              | None -> 0.0
            in
            { step_index; gate_remain_s }
          else
            {
              state with
              gate_remain_s = state.gate_remain_s -. gate_remain_s_delta;
            })
      |> Raw.bind ~f:(fun state ctx ->
             match Array.get sequence_array state.step_index with
             | Some current_step ->
                 let gate = state.gate_remain_s > 0.0 in
                 let value = Signal.sample current_step.value ctx in
                 (value, gate)
             | None -> (0.0, false))
      |> Signal.of_raw
    in
    { value = Signal.map combined ~f:fst; gate = Signal.map combined ~f:snd }
end
