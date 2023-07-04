module Chunk_header = struct
  type t = { type_ : string; length : int }

  let to_string { type_; length } =
    Printf.sprintf "((type_ %s) (length %d))" type_ length
end

module Format = struct
  type t =
    | Single_track
    | Simultaneous_tracks of int
    | Sequential_tracks of int

  let to_string = function
    | Single_track -> "Single_track"
    | Simultaneous_tracks n -> Printf.sprintf "(Simultaneous_tracks %d)" n
    | Sequential_tracks n -> Printf.sprintf "(Sequential_tracks %d)" n
end

module Division = struct
  type time_code = { smpte_format : int; ticks_per_frame : int }
  type t = Ticks_per_quarter_note of int | Time_code of time_code

  let to_string = function
    | Ticks_per_quarter_note n -> Printf.sprintf "(Ticks_per_quarter_note %d)" n
    | Time_code { smpte_format; ticks_per_frame } ->
        Printf.sprintf "(Time_code ((smpte_format %d) (ticks_per_frame %d)))"
          smpte_format ticks_per_frame

  let of_raw_int16 i =
    let payload = i land lnot (1 lsl 15) in
    if i land (1 lsl 15) == 0 then Ticks_per_quarter_note payload
    else
      let negative_smpte_format = payload lsr 8 in
      let ticks_per_frame = payload land 255 in
      let smpte_format = -negative_smpte_format + 1 in
      Time_code { smpte_format; ticks_per_frame }
end

exception Bad_header of string

module Midi_header = struct
  module Raw = struct
    type t = { format_ : int; ntrks : int; division : int }

    let to_string { format_; ntrks; division } =
      Printf.sprintf "((format_ %d) (ntrks %d) (division %d))" format_ ntrks
        division
  end

  type t = { format_ : Format.t; division : Division.t }

  let to_string { format_; division } =
    Printf.sprintf "((format_ %s) (division %s))" (Format.to_string format_)
      (Division.to_string division)

  let of_raw { Raw.format_; ntrks; division } =
    let format_ =
      match format_ with
      | 0 ->
          if ntrks != 1 then
            raise
              (Bad_header
                 (Printf.sprintf
                    "expected 1 track for Single_track format but got %d" ntrks));
          Format.Single_track
      | 1 -> Format.Simultaneous_tracks ntrks
      | 2 -> Format.Sequential_tracks ntrks
      | _ ->
          raise
            (Bad_header
               (Printf.sprintf "unexpected format field in header: %d" format_))
    in
    let division = Division.of_raw_int16 division in
    { format_; division }
end

module Midi_byte_array = struct
  type t = { byte_array : char array }

  let seq4_at t index = Seq.init 4 (fun i -> Array.get t.byte_array (index + i))
  let seq2_at t index = Seq.init 2 (fun i -> Array.get t.byte_array (index + i))
  let string4_at t index = seq4_at t index |> String.of_seq

  let int32be_at t index =
    seq4_at t index
    |> Seq.fold_lefti
         (fun acc i ch -> acc + (int_of_char ch lsl ((3 - i) * 8)))
         0

  let int16be_at t index =
    seq2_at t index
    |> Seq.fold_lefti
         (fun acc i ch -> acc + (int_of_char ch lsl ((1 - i) * 8)))
         0

  let read_string4 t i = (string4_at t i, i + 4)
  let read_int32be t i = (int32be_at t i, i + 4)
  let read_int16be t i = (int16be_at t i, i + 2)

  let read_chunk_header t i =
    let type_, i = read_string4 t i in
    let length, i = read_int32be t i in
    ({ Chunk_header.type_; length }, i)

  let read_midi_header t i =
    let format_, i = read_int16be t i in
    let ntrks, i = read_int16be t i in
    let division, i = read_int16be t i in
    let raw = { Midi_header.Raw.format_; ntrks; division } in
    (Midi_header.of_raw raw, i)
end

module Midi_file_reader = struct
  type t = { path : string }

  let of_path path = { path }

  let read_byte_array t =
    let channel = open_in_bin t.path in
    let byte_array =
      Seq.of_dispenser (fun () ->
          match input_char channel with
          | byte -> Some byte
          | exception End_of_file ->
              close_in channel;
              None)
      |> Array.of_seq
    in
    { Midi_byte_array.byte_array }

  let read t =
    let open Midi_byte_array in
    let byte_array = read_byte_array t in
    let _header_chunk_header, i = read_chunk_header byte_array 0 in
    print_endline
      (Midi_byte_array.read_midi_header byte_array i
      |> fst |> Midi_header.to_string);
    ()
end

module Args = struct
  type t = { midi_file_path : string }

  let usage () =
    let arg0 =
      if Array.length Sys.argv < 1 then "midi.exe" else Array.get Sys.argv 0
    in
    Printf.sprintf "USAGE:\n%s FILE\n\nPlay a midi file.\n" arg0

  let parse () =
    let anon_args = ref [] in
    let spec = [] in
    Arg.parse spec
      (fun anon_arg -> anon_args := anon_arg :: !anon_args)
      (usage ());
    match !anon_args with
    | [ midi_file_path ] -> { midi_file_path }
    | [] ->
        Printf.eprintf "Missing midi file path!\n\n%s"
          (Arg.usage_string spec (usage ()));
        exit 1
    | _ ->
        Printf.eprintf "Too many anonymous arguments!\n\n%s"
          (Arg.usage_string spec (usage ()));
        exit 1
end

let () =
  let { Args.midi_file_path } = Args.parse () in
  let reader = Midi_file_reader.of_path midi_file_path in
  Midi_file_reader.read reader
