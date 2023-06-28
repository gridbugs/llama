module Ctx = struct
  type t = { sample_index : int; sample_rate_hz : float }
end

module Raw = struct
  type 'a t = Ctx.t -> 'a
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

module type Ops = sig
  val of_raw : 'a Raw.t -> 'a t
  val of_ref : 'a ref -> 'a t
  val sample : 'a t -> Ctx.t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val const : 'a -> 'a t
  val var : 'a -> 'a t * 'a ref
end
