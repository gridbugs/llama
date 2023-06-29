module Ctx = struct
  type t = { sample_index : int; sample_rate_hz : float }
end

module Raw = struct
  type 'a t = Ctx.t -> 'a

  let with_state ~init ~f =
    let state = ref init in
    fun ctx ->
      let new_state = f !state ctx in
      state := new_state;
      new_state

  let map t ~f ctx = f (t ctx)
  let bind t ~f ctx = f (t ctx) ctx
end

type 'a t = {
  raw : 'a Raw.t;
  mutable buffered_sample : 'a option;
  mutable next_sample_index : int;
}

let of_raw raw = { raw; buffered_sample = None; next_sample_index = 0 }

let sample_and_update t ctx =
  let x = t.raw ctx in
  t.buffered_sample <- Some x;
  x

let sample t (ctx : Ctx.t) =
  if ctx.sample_index < t.next_sample_index then
    match t.buffered_sample with
    | Some sample -> sample
    | None -> sample_and_update t ctx
  else (
    t.next_sample_index <- t.next_sample_index + 1;
    sample_and_update t ctx)

let map t ~f =
  of_raw (fun ctx ->
      let x = sample t ctx in
      f x)

let both a b =
  of_raw (fun ctx ->
      let xa = sample a ctx in
      let xb = sample b ctx in
      (xa, xb))

let const x = of_raw (Fun.const x)
let of_ref ref = of_raw (fun _ -> !ref)

let var x =
  let ref = ref x in
  (of_ref ref, ref)

module Trigger = struct
  type nonrec t = { signal : bool t }

  let of_signal signal = { signal }

  let raw t =
    let previous = ref false in
    fun ctx ->
      let sample = sample t.signal ctx in
      let trigger_sample = sample && not !previous in
      previous := sample;
      trigger_sample

  let signal t = of_raw (raw t)
end

let trigger t = Trigger.(of_signal t |> signal)
