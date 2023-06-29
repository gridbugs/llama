open Llama

type t = (Music.Note.t * float) option list

let to_steps t ~time_scale =
  let open Dsl in
  List.map t
    ~f:
      (Option.map (fun (note, period) ->
           let freq = Music.Note.frequency_hz note in
           { value = const freq; period_s = const (period *. time_scale) }))

let middle_c_loop = [ Some ((`C, 4), 1.0); None ]

let hsc =
  [
    Some ((`C, 4), 1.5);
    None;
    Some ((`C, 4), 3.0);
    None;
    None;
    Some ((`C, 4), 0.5);
    Some ((`E, 4), 0.5);
    Some ((`C, 4), 0.5);
    Some ((`E, 4), 0.5);
    Some ((`C, 4), 1.5);
    None;
    Some ((`E, 4), 0.5);
    Some ((`C, 4), 1.5);
    None;
    None;
    None;
  ]
