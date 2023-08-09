open StdLabels

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
    t.next_sample_index <- ctx.sample_index + 1;
    sample_and_update t ctx)

let map_ctx t ~f =
  of_raw (fun ctx ->
      let x = sample t ctx in
      f x ctx)

let map t ~f =
  of_raw (fun ctx ->
      let x = sample t ctx in
      f x)

let both a b =
  of_raw (fun ctx ->
      let xa = sample a ctx in
      let xb = sample b ctx in
      (xa, xb))

let map2 t1 t2 ~f = both t1 t2 |> map ~f:(fun (x1, x2) -> f x1 x2)

let map3 t1 t2 t3 ~f =
  both t1 (both t2 t3) |> map ~f:(fun (x1, (x2, x3)) -> f x1 x2 x3)

let force t ~to_force = map2 t to_force ~f:(fun x _ -> x)
let const x = of_raw (Fun.const x)
let of_ref ref = of_raw (fun _ -> !ref)

let var x =
  let ref = ref x in
  (of_ref ref, ref)

let silence = const 0.0
let never = const false
let scale s = map ~f:(fun x -> x *. s)
let scale_div s = map ~f:(fun x -> x /. s)
let offset s = map ~f:(fun x -> x +. s)

let exp_01 k =
  if Float.equal k 0.0 then Fun.id
  else
    let b = 1.0 /. (Float.exp k -. 1.0) in
    let a = -.(Float.log b /. k) in
    map ~f:(fun x -> Float.exp (k *. (x -. a)) -. b)

let debug t ~f =
  map t ~f:(fun x ->
      f x;
      x)

let debug_print_float_endline =
  debug ~f:(fun x -> print_endline (Printf.sprintf "%f" x))

let debug_print_sample_index_on_true =
  map_ctx ~f:(fun x ctx ->
      if x then print_endline (Printf.sprintf "%d" ctx.sample_index);
      x)

let sum ts =
  of_raw (fun ctx ->
      List.fold_left ts ~init:0.0 ~f:(fun acc signal ->
          acc +. sample signal ctx))

let mean ts =
  let length = Int.to_float (List.length ts) in
  sum ts |> map ~f:(fun sum -> sum /. length)

let recip = map ~f:(fun x -> 1.0 /. x)
let to_01 = map ~f:(fun x -> (x +. 1.0) /. 2.0)
let add a b = both a b |> map ~f:(fun (a, b) -> a +. b)
let ( +.. ) a b = add a b
let mul a b = both a b |> map ~f:(fun (a, b) -> a *. b)
let ( *.. ) a b = mul a b
let sub a b = both a b |> map ~f:(fun (a, b) -> a -. b)
let ( -.. ) a b = sub a b
let div a b = both a b |> map ~f:(fun (a, b) -> a /. b)
let ( /.. ) a b = div a b

module Trigger = struct
  type nonrec t = bool t

  let rising_edge ?(init = false) t =
    of_raw
      (let previous = ref init in
       fun ctx ->
         let sample = sample t ctx in
         let trigger_sample = sample && not !previous in
         previous := sample;
         trigger_sample)

  let of_signal_unsafe t = t
  let to_signal t = t
  let sample = sample
  let never = never
  let debug_print_sample_index_on_true = debug_print_sample_index_on_true
end

module Gate = struct
  type nonrec t = bool t

  let of_signal t = t
  let to_signal t = t
  let to_trigger t = Trigger.rising_edge t
  let sample = sample
  let debug_print_sample_index_on_true = debug_print_sample_index_on_true
end

let gate = Gate.of_signal
