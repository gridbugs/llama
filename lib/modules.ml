module Ctx = Signal.Ctx
module Raw = Signal.Raw

let sweep_01 frequency_hz =
  Raw.with_state ~init:0.0 ~f:(fun state ctx ->
      let state_delta = Signal.sample frequency_hz ctx /. ctx.sample_rate_hz in
      Float.rem (state +. state_delta) 1.0)

module Oscillator = struct
  type waveform = Sine | Saw
  type t = { waveform : waveform Signal.t; frequency_hz : float Signal.t }

  let signal t =
    sweep_01 t.frequency_hz
    |> Raw.bind ~f:(fun state ctx ->
           match Signal.sample t.waveform ctx with
           | Sine -> Float.sin (state *. Float.pi *. 2.0)
           | Saw -> (state *. 2.0) -. 1.0)
    |> Signal.of_raw
end

module Clock = struct
  type t = { frequency_hz : float Signal.t }

  let signal t =
    sweep_01 t.frequency_hz
    |> Raw.map ~f:(fun state -> state < 0.5)
    |> Signal.of_raw |> Signal.trigger
end

module Amplifier = struct
  type t = { signal : float Signal.t; volume : float Signal.t }

  (* As an optimization, if the volume is less than this value, the signal is
     not evaluated. *)
  let threshold = 1.0 /. 256.0

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
        Float.clamp_01 (state +. delta))
    |> Signal.of_raw
end

module Adsr_linear = struct
  type t = {
    gate : bool Signal.t;
    attack_s : float Signal.t;
    decay_s : float Signal.t;
    sustain_01 : float Signal.t;
    release_s : float Signal.t;
  }

  type state = { current_value : float; crossed_threshold : bool }

  let signal t =
    Raw.with_state ~init:{ current_value = 0.0; crossed_threshold = false }
      ~f:(fun state ctx ->
        if Signal.sample t.gate ctx then
          if state.crossed_threshold then
            (* decay and sustain *)
            let decay_s = Signal.sample t.decay_s ctx in
            let sustain_01 = Signal.sample t.sustain_01 ctx in
            let delta = 1.0 /. (decay_s *. ctx.sample_rate_hz) in
            let current_value =
              state.current_value -. delta |> Float.max sustain_01
            in
            { state with current_value }
          else
            (* attack *)
            let attack_s = Signal.sample t.attack_s ctx in
            let delta = 1.0 /. (attack_s *. ctx.sample_rate_hz) in
            let current_value = state.current_value +. delta |> Float.min 1.0 in
            { current_value; crossed_threshold = Float.equal current_value 1.0 }
        else
          (* release *)
          let release_s = Signal.sample t.release_s ctx in
          let delta = 1.0 /. (release_s *. ctx.sample_rate_hz) in
          let current_value = state.current_value -. delta |> Float.max 0.0 in
          { current_value; crossed_threshold = false })
    |> Raw.map ~f:(fun { current_value; _ } -> current_value)
    |> Signal.of_raw
end

module Step_sequencer = struct
  type step = { value : float Signal.t; period_s : float Signal.t }
  type t = { sequence : step option list; clock : bool Signal.t }

  type state = {
    step_index : int;
    gate_remain_s : float;
    current_value : float;
  }

  type output = { value : float Signal.t; gate : bool Signal.t }

  let init_state t =
    {
      step_index = List.length t.sequence - 1;
      gate_remain_s = 0.0;
      current_value = 0.0;
    }

  let signal t =
    let sequence_array = Array.of_list t.sequence in
    let sequence_length = List.length t.sequence in
    let combined =
      Raw.with_state ~init:(init_state t) ~f:(fun state ctx ->
          let gate_remain_s_delta = 1.0 /. ctx.sample_rate_hz in
          if Signal.sample t.clock ctx then
            let step_index = (state.step_index + 1) mod sequence_length in
            let gate_remain_s, current_value =
              match Array.get sequence_array step_index with
              | Some current_step ->
                  let gate_remain_s =
                    Signal.sample current_step.period_s ctx
                    -. gate_remain_s_delta
                  in
                  let current_value = Signal.sample current_step.value ctx in
                  (gate_remain_s, current_value)
              | None -> (0.0, state.current_value)
            in
            { step_index; gate_remain_s; current_value }
          else
            {
              state with
              gate_remain_s = state.gate_remain_s -. gate_remain_s_delta;
            })
      |> Raw.map ~f:(fun { current_value; gate_remain_s; _ } ->
             (current_value, gate_remain_s > 0.0))
      |> Signal.of_raw
    in
    { value = Signal.map combined ~f:fst; gate = Signal.map combined ~f:snd }
end

module Biquad_filter = struct
  (* This is based on the filter designs at:
     https://exstrom.com/journal/sigproc/dsigproc.html *)

  type buffer_entry = {
    mutable a : float;
    mutable d1 : float;
    mutable d2 : float;
    mutable w0 : float;
    mutable w1 : float;
    mutable w2 : float;
  }

  let buffer_entry_zeroes () =
    { a = 0.0; d1 = 0.0; d2 = 0.0; w0 = 0.0; w1 = 0.0; w2 = 0.0 }

  module Buffer = struct
    type t = buffer_entry array

    let create filter_order_half =
      Array.init filter_order_half ~f:(fun _ -> buffer_entry_zeroes ())

    let apply_low_pass t sample =
      Array.fold_left t ~init:sample ~f:(fun sample entry ->
          entry.w0 <- (entry.d1 *. entry.w1) +. (entry.d2 *. entry.w2) +. sample;
          let sample = entry.a *. (entry.w0 +. (2.0 *. entry.w1) +. entry.w2) in
          entry.w2 <- entry.w1;
          entry.w1 <- entry.w0;
          sample)

    let apply_high_pass t sample =
      Array.fold_left t ~init:sample ~f:(fun sample entry ->
          entry.w0 <- (entry.d1 *. entry.w1) +. (entry.d2 *. entry.w2) +. sample;
          let sample = entry.a *. (entry.w0 -. (2.0 *. entry.w1) +. entry.w2) in
          entry.w2 <- entry.w1;
          entry.w1 <- entry.w0;
          sample)
  end

  module Butterworth = struct
    type t = {
      signal : float Signal.t;
      half_power_frequency_hz : float Signal.t;
    }

    let update_buffer_low_pass buffer ~half_power_frequency_sample_rate_ratio =
      let a = Float.tan (Float.pi *. half_power_frequency_sample_rate_ratio) in
      let a2 = a *. a in
      let n = Int.to_float (Array.length buffer) in
      Array.iteri buffer ~f:(fun i entry ->
          let r =
            Float.sin
              (Float.pi *. ((2.0 *. Int.to_float i) +. 1.0) /. (4.0 *. n))
          in
          let s = a2 +. (2.0 *. a *. r) +. 1.0 in
          entry.a <- a2 /. s;
          entry.d1 <- 2.0 *. (1.0 -. a2) /. s;
          entry.d2 <- -.(a2 -. (2.0 *. a *. r) +. 1.0) /. s)

    let update_buffer_high_pass buffer ~half_power_frequency_sample_rate_ratio =
      let a = Float.tan (Float.pi *. half_power_frequency_sample_rate_ratio) in
      let a2 = a *. a in
      let n = Int.to_float (Array.length buffer) in
      Array.iteri buffer ~f:(fun i entry ->
          let r =
            Float.sin
              (Float.pi *. ((2.0 *. Int.to_float i) +. 1.0) /. (4.0 *. n))
          in
          let s = a2 +. (2.0 *. a *. r) +. 1.0 in
          entry.a <- 1.0 /. s;
          entry.d1 <- 2.0 *. (1.0 -. a2) /. s;
          entry.d2 <- -.(a2 -. (2.0 *. a *. r) +. 1.0) /. s)

    let raw t ~update_buffer ~apply_buffer ~filter_order_half =
      if filter_order_half <= 0 then
        (* Handle the degenerate case by applying no filtering at all *)
        Signal.sample t.signal
      else
        let buffer = Buffer.create filter_order_half in
        fun ctx ->
          let sample = Signal.sample t.signal ctx in
          let half_power_frequency_hz =
            Signal.sample t.half_power_frequency_hz ctx
          in
          let half_power_frequency_sample_rate_ratio =
            half_power_frequency_hz /. ctx.sample_rate_hz
          in
          update_buffer buffer ~half_power_frequency_sample_rate_ratio;
          apply_buffer buffer sample

    let signal_low_pass t ~filter_order_half =
      Signal.of_raw
        (raw t ~update_buffer:update_buffer_low_pass
           ~apply_buffer:Buffer.apply_low_pass ~filter_order_half)

    let signal_high_pass t ~filter_order_half =
      Signal.of_raw
        (raw t ~update_buffer:update_buffer_high_pass
           ~apply_buffer:Buffer.apply_high_pass ~filter_order_half)
  end
end

module Butterworth_filter = struct
  type t = Biquad_filter.Butterworth.t = {
    signal : float Signal.t;
    half_power_frequency_hz : float Signal.t;
  }

  let signal_low_pass = Biquad_filter.Butterworth.signal_low_pass
  let signal_high_pass = Biquad_filter.Butterworth.signal_high_pass
end
