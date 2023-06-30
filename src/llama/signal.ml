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

  let with_state' ~init ~f =
    let state = ref init in
    fun ctx ->
      let new_state, x = f !state ctx in
      state := new_state;
      x

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

let silence = const 0.0
let never = const false

let trigger t =
  of_raw
    (let previous = ref false in
     fun ctx ->
       let sample = sample t ctx in
       let trigger_sample = sample && not !previous in
       previous := sample;
       trigger_sample)

let scale s = map ~f:(fun x -> x *. s)
let offset s = map ~f:(fun x -> x +. s)

let exp01 k =
  if Float.equal k 0.0 then Fun.id
  else
    let b = 1.0 /. (Float.exp k -. 1.0) in
    let a = -.(Float.log b /. k) in
    map ~f:(fun x -> Float.exp (k *. (x -. a)) -. b)

let debug t ~f =
  map t ~f:(fun x ->
      f x;
      x)

let sum ts =
  of_raw (fun ctx ->
      List.fold_left ts ~init:0.0 ~f:(fun acc signal ->
          acc +. sample signal ctx))

let mean ts =
  let length = Int.to_float (List.length ts) in
  sum ts |> map ~f:(fun sum -> sum /. length)

let to_01 = map ~f:(fun x -> (x +. 1.0) /. 2.0)
let add a b = both a b |> map ~f:(fun (a, b) -> a +. b)
let ( +.. ) a b = add a b
let mul a b = both a b |> map ~f:(fun (a, b) -> a *. b)
let ( *.. ) a b = mul a b
