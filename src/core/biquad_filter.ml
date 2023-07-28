open StdLabels

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
  type t = { signal : float Signal.t; half_power_frequency_hz : float Signal.t }

  let update_buffer_low_pass buffer ~half_power_frequency_sample_rate_ratio =
    let a = Float.tan (Float.pi *. half_power_frequency_sample_rate_ratio) in
    let a2 = a *. a in
    let n = Int.to_float (Array.length buffer) in
    Array.iteri buffer ~f:(fun i entry ->
        let r =
          Float.sin (Float.pi *. ((2.0 *. Int.to_float i) +. 1.0) /. (4.0 *. n))
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
          Float.sin (Float.pi *. ((2.0 *. Int.to_float i) +. 1.0) /. (4.0 *. n))
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
          half_power_frequency_hz /. ctx.sample_rate_hz |> Float.max 0.0
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

module Chebyshev = struct
  type t = {
    signal : float Signal.t;
    cutoff_hz : float Signal.t;
    epsilon : float Signal.t;
  }

  let update_buffer_low_pass buffer ~cutoff_sample_rate_ratio ~epsilon =
    let a = Float.tan (Float.pi *. cutoff_sample_rate_ratio) in
    let a2 = a *. a in
    let u =
      Float.log ((1.0 +. Float.sqrt (1.0 +. (epsilon *. epsilon))) /. epsilon)
    in
    let n = Int.to_float (Array.length buffer * 2) in
    let su = Float.sinh (u /. n) in
    let cu = Float.cosh (u /. n) in
    Array.iteri buffer ~f:(fun i entry ->
        let theta =
          Float.pi *. ((2.0 *. Int.to_float i) +. 1.0) /. (2.0 *. n)
        in
        let b = Float.sin theta *. su in
        let c = Float.cos theta *. cu in
        let c = (b *. b) +. (c *. c) in
        let s = (a2 *. c) +. (2.0 *. a *. b) +. 1.0 in
        entry.a <- a2 /. (4.0 *. s);
        entry.d1 <- 2.0 *. (1.0 -. (a2 *. c)) /. s;
        entry.d2 <- -.((a2 *. c) -. (2.0 *. a *. b) +. 1.0) /. s)

  let update_buffer_high_pass buffer ~cutoff_sample_rate_ratio ~epsilon =
    let a = Float.tan (Float.pi *. cutoff_sample_rate_ratio) in
    let a2 = a *. a in
    let u =
      Float.log ((1.0 +. Float.sqrt (1.0 +. (epsilon *. epsilon))) /. epsilon)
    in
    let n = Int.to_float (Array.length buffer * 2) in
    let su = Float.sinh (u /. n) in
    let cu = Float.cosh (u /. n) in
    Array.iteri buffer ~f:(fun i entry ->
        let theta =
          Float.pi *. ((2.0 *. Int.to_float i) +. 1.0) /. (2.0 *. n)
        in
        let b = Float.sin theta *. su in
        let c = Float.cos theta *. cu in
        let c = (b *. b) +. (c *. c) in
        let s = a2 +. (2.0 *. a *. b) +. c in
        entry.a <- 1.0 /. (4.0 *. s);
        entry.d1 <- 2.0 *. (c -. a2) /. s;
        entry.d2 <- -.(a2 -. (2.0 *. a *. b) +. c) /. s)

  (* Arbitrary small constant below which you don't hear much difference. We
     have this to protect against the case where epsilon is set to 0 which
     would otherwise result in silence. *)
  let epsilon_min = 0.000000001

  let raw t ~update_buffer ~apply_buffer ~filter_order_half =
    if filter_order_half <= 0 then
      (* Handle the degenerate case by applying no filtering at all *)
      Signal.sample t.signal
    else
      let buffer = Buffer.create filter_order_half in
      fun ctx ->
        let sample = Signal.sample t.signal ctx in
        let cutoff_hz = Signal.sample t.cutoff_hz ctx in
        let cutoff_sample_rate_ratio =
          cutoff_hz /. ctx.sample_rate_hz |> Float.max 0.0
        in
        let epsilon = Signal.sample t.epsilon ctx |> Float.max epsilon_min in
        update_buffer buffer ~cutoff_sample_rate_ratio ~epsilon;
        let output_scaled = apply_buffer buffer sample in
        let scale_factor = (1.0 -. Float.exp (-.epsilon)) /. 2.0 in
        output_scaled /. scale_factor

  let signal_low_pass t ~filter_order_half =
    Signal.of_raw
      (raw t ~update_buffer:update_buffer_low_pass
         ~apply_buffer:Buffer.apply_low_pass ~filter_order_half)

  let signal_high_pass t ~filter_order_half =
    Signal.of_raw
      (raw t ~update_buffer:update_buffer_high_pass
         ~apply_buffer:Buffer.apply_high_pass ~filter_order_half)
end
