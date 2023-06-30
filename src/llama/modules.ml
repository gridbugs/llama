module Ctx = Signal.Ctx
module Raw = Signal.Raw

let sweep_01 frequency_hz =
  Raw.with_state ~init:0.0 ~f:(fun state ctx ->
      let state_delta = Signal.sample frequency_hz ctx /. ctx.sample_rate_hz in
      Float.rem (state +. state_delta) 1.0)

module Oscillator = struct
  type waveform = Sine | Saw | Triangle | Square | Noise

  type t = {
    waveform : waveform Signal.t;
    frequency_hz : float Signal.t;
    square_wave_pulse_width_01 : float Signal.t;
    reset_trigger : bool Signal.t;
    reset_offset_01 : float Signal.t;
  }

  let signal t =
    Raw.with_state' ~init:None ~f:(fun state ctx ->
        let state =
          match state with
          | None -> Signal.sample t.reset_offset_01 ctx
          | Some state ->
              if Signal.sample t.reset_trigger ctx then
                Signal.sample t.reset_offset_01 ctx
              else state
        in
        let state_delta =
          Signal.sample t.frequency_hz ctx /. ctx.sample_rate_hz
        in
        let state = Float.rem (state +. state_delta) 1.0 in
        let x =
          match Signal.sample t.waveform ctx with
          | Sine -> Float.sin (state *. Float.pi *. 2.0)
          | Saw -> (state *. 2.0) -. 1.0
          | Triangle -> (Float.abs ((state *. 2.0) -. 1.0) *. 2.0) -. 1.0
          | Square ->
              if state < Signal.sample t.square_wave_pulse_width_01 ctx then
                -1.0
              else 1.0
          | Noise -> Random.float 2.0 -. 1.0
        in
        (Some state, x))
    |> Signal.of_raw
end

module Clock = struct
  type t = { frequency_hz : float Signal.t }

  let signal t =
    sweep_01 t.frequency_hz
    |> Raw.map ~f:(fun state -> state < 0.5)
    |> Signal.of_raw |> Signal.trigger
end

module Clock_divider = struct
  type t = { clock : bool Signal.t; denominator : int }

  let signal t =
    Raw.with_state' ~init:0 ~f:(fun state ctx ->
        if Signal.sample t.clock ctx then
          if state > 0 then (state - 1, false) else (t.denominator - 1, true)
        else (state, false))
    |> Signal.of_raw
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
    Raw.with_state' ~init:{ current_value = 0.0; crossed_threshold = false }
      ~f:(fun state ctx ->
        let state =
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
              let current_value =
                state.current_value +. delta |> Float.min 1.0
              in
              {
                current_value;
                crossed_threshold = Float.equal current_value 1.0;
              }
          else
            (* release *)
            let release_s = Signal.sample t.release_s ctx in
            let delta = 1.0 /. (release_s *. ctx.sample_rate_hz) in
            let current_value = state.current_value -. delta |> Float.max 0.0 in
            { current_value; crossed_threshold = false }
        in
        (state, state.current_value))
    |> Signal.of_raw
end

module Sequencer = struct
  type output = { value : float Signal.t; gate : bool Signal.t }
end

module Step_sequencer = struct
  type step = { value : float Signal.t; period_s : float Signal.t }
  type t = { sequence : step option list; clock : bool Signal.t }

  type state = {
    step_index : int;
    gate_remain_s : float;
    current_value : float;
  }

  let init_state t =
    {
      step_index = List.length t.sequence - 1;
      gate_remain_s = 0.0;
      current_value = 0.0;
    }

  let signal t =
    let sequence_array = Array.of_list t.sequence in
    let combined =
      Raw.with_state' ~init:(init_state t) ~f:(fun state ctx ->
          let gate_remain_s_delta = 1.0 /. ctx.sample_rate_hz in
          let state =
            if Signal.sample t.clock ctx then
              let step_index =
                (state.step_index + 1) mod Array.length sequence_array
              in
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
              }
          in
          (state, (state.current_value, state.gate_remain_s > 0.0)))
      |> Signal.of_raw
    in
    {
      Sequencer.value = Signal.map combined ~f:fst;
      gate = Signal.map combined ~f:snd;
    }
end

module Random_sequencer = struct
  type t = {
    values : float Signal.t list;
    period : float Signal.t;
    clock : bool Signal.t;
  }

  type state = { gate_remain_s : float; current_value : float }

  let signal t =
    let value_array = Array.of_list t.values in
    let combined =
      Raw.with_state' ~init:{ gate_remain_s = 0.0; current_value = 0.0 }
        ~f:(fun state ctx ->
          let gate_remain_s_delta = 1.0 /. ctx.sample_rate_hz in
          let state =
            if Signal.sample t.clock ctx then
              let index = Random.int (Array.length value_array) in
              let current_value =
                Signal.sample (Array.get value_array index) ctx
              in
              let gate_remain_s =
                Signal.sample t.period ctx -. gate_remain_s_delta
              in
              { current_value; gate_remain_s }
            else
              {
                state with
                gate_remain_s = state.gate_remain_s -. gate_remain_s_delta;
              }
          in
          (state, (state.current_value, state.gate_remain_s > 0.0)))
      |> Signal.of_raw
    in
    {
      Sequencer.value = Signal.map combined ~f:fst;
      gate = Signal.map combined ~f:snd;
    }
end

module Butterworth_filter = struct
  type t = Biquad_filter.Butterworth.t = {
    signal : float Signal.t;
    half_power_frequency_hz : float Signal.t;
  }

  let signal_low_pass = Biquad_filter.Butterworth.signal_low_pass
  let signal_high_pass = Biquad_filter.Butterworth.signal_high_pass
end

module Chebyshev_filter = struct
  type t = Biquad_filter.Chebyshev.t = {
    signal : float Signal.t;
    cutoff_hz : float Signal.t;
    epsilon : float Signal.t;
  }

  let signal_low_pass = Biquad_filter.Chebyshev.signal_low_pass
  let signal_high_pass = Biquad_filter.Chebyshev.signal_high_pass
end

module Sample_and_hold = struct
  type t = { signal : float Signal.t; trigger : bool Signal.t }

  let signal t =
    Raw.with_state ~init:0.0 ~f:(fun state ctx ->
        if Signal.sample t.trigger ctx then Signal.sample t.signal ctx
        else state)
    |> Signal.of_raw
end

module Sample_player_mono = struct
  type t = { data : float array; trigger : bool Signal.t }

  let signal t =
    Raw.with_state' ~init:(Array.length t.data) ~f:(fun index ctx ->
        if Signal.sample t.trigger ctx then (0, 0.0)
        else
          (* deliberately allow index to be 1 out of bounds to indicate that the sample is finished *)
          let sample =
            if index < Array.length t.data then Array.get t.data index else 0.0
          in
          let index = Int.min (index + 1) (Array.length t.data) in
          (index, sample))
    |> Signal.of_raw
end

module Bitwise_trigger_sequencer = struct
  type t = {
    num_channels : int;
    sequence : int Signal.t list;
    clock : bool Signal.t;
  }

  let signals t =
    let sequence_array = Array.of_list t.sequence in
    let bitfield_signal =
      Raw.with_state'
        ~init:(Array.length sequence_array - 1)
        ~f:(fun index ctx ->
          if Signal.sample t.clock ctx then
            let index = (index + 1) mod Array.length sequence_array in
            let signal = Array.get sequence_array index in
            let output_bits = Signal.sample signal ctx in
            (index, output_bits)
          else (index, 0))
      |> Signal.of_raw
    in
    List.init ~len:t.num_channels ~f:(fun i ->
        Signal.map bitfield_signal ~f:(fun bits ->
            not (Int.equal (bits land (1 lsl i)) 0)))
end
