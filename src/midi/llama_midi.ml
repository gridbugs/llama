exception Parse_exception = Byte_array_parser.Parse_exception

let sprintf = Printf.sprintf

module Unknown = struct
  type t = Unknown of string
end

module Unimplemented = struct
  type t = Unimplemented of string
end

open Unknown
open Unimplemented

module Chunk_type = struct
  type t = Header | Track

  let of_string_opt = function
    | "MThd" -> Some Header
    | "MTrk" -> Some Track
    | _ -> None

  let parse_result : (t, Unknown.t) result Byte_array_parser.t =
    let open Byte_array_parser in
    let+ name = string4 in
    match of_string_opt name with
    | Some t -> Ok t
    | None -> Error (Unknown name)
end

module Format = struct
  type t =
    | Single_track
    | Simultaneous_tracks of int
    | Sequential_tracks of int

  let to_string = function
    | Single_track -> "Single_track"
    | Simultaneous_tracks n -> sprintf "(Simultaneous_tracks %d)" n
    | Sequential_tracks n -> sprintf "(Sequential_tracks %d)" n

  let num_tracks = function
    | Single_track -> 1
    | Simultaneous_tracks n | Sequential_tracks n -> n
end

module Division = struct
  type time_code = { smpte_format : int; ticks_per_frame : int }
  type t = Ticks_per_quarter_note of int | Time_code of time_code

  let to_string = function
    | Ticks_per_quarter_note n -> sprintf "(Ticks_per_quarter_note %d)" n
    | Time_code { smpte_format; ticks_per_frame } ->
        sprintf "(Time_code ((smpte_format %d) (ticks_per_frame %d)))"
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

module Header = struct
  module Raw = struct
    type t = { format_ : int; ntrks : int; division : int }

    let parse =
      let open Byte_array_parser in
      let+ format_ = int16be and+ ntrks = int16be and+ division = int16be in
      { format_; ntrks; division }
  end

  type t = { format_ : Format.t; division : Division.t }

  let to_string { format_; division } =
    sprintf "((format_ %s) (division %s))" (Format.to_string format_)
      (Division.to_string division)

  let of_raw { Raw.format_; ntrks; division } =
    let format_ =
      match format_ with
      | 0 ->
          if ntrks != 1 then
            raise
              (Parse_exception
                 (sprintf "expected 1 track for Single_track format but got %d"
                    ntrks));
          Format.Single_track
      | 1 -> Format.Simultaneous_tracks ntrks
      | 2 -> Format.Sequential_tracks ntrks
      | _ ->
          raise
            (Parse_exception
               (sprintf "unexpected format field in header: %d" format_))
    in
    let division = Division.of_raw_int16 division in
    { format_; division }

  let parse = Byte_array_parser.(Raw.parse >>| of_raw)
end

module Channel_voice_message = struct
  type note_event = { note : int; velocity : int }
  type message = Note_off of note_event | Note_on of note_event
  type t = { channel : int; message : message }

  let note_event_to_string { note; velocity } =
    sprintf "((note %d) (velocity %d))" note velocity

  let message_to_string = function
    | Note_off note_event ->
        sprintf "(Note_off %s)" (note_event_to_string note_event)
    | Note_on note_event ->
        sprintf "(Note_on %s)" (note_event_to_string note_event)

  let to_string { channel; message } =
    sprintf "((channel %d) (message %s))" channel (message_to_string message)

  let parse_result status =
    if status < 0 || status > 255 then raise (Parse_exception "Expected byte");
    if status < 128 then
      raise (Parse_exception "Expected most significant bit to be 1");
    let open Byte_array_parser in
    (* bits 4,5,6 *)
    let message_type_identifier = (status lsr 4) land 0x7 in
    let channel = status land 0xF in
    let+ message =
      match message_type_identifier with
      | 0 ->
          let+ note = byte_msb0 ~message:"note"
          and+ velocity = byte_msb0 ~message:"velocity" in
          Ok (Note_off { note; velocity })
      | 1 ->
          let+ note = byte_msb0 ~message:"note"
          and+ velocity = byte_msb0 ~message:"velocity" in
          Ok (Note_on { note; velocity })
      | 2 ->
          let+ _note = byte_msb0 ~message:"note"
          and+ _pressure = byte_msb0 ~message:"pressure" in
          Error (Unimplemented "Polyphonic_key_pressure")
      | 3 ->
          let+ _controller = byte_msb0 ~message:"controller"
          and+ _value = byte_msb0 ~message:"value" in
          Error (Unimplemented "Control_change")
      | 4 ->
          let+ _program = byte_msb0 ~message:"program" in
          Error (Unimplemented "Program_change")
      | 5 ->
          let+ _pressure = byte_msb0 ~message:"pressure" in
          Error (Unimplemented "Channel_pressure")
      | 6 ->
          let+ _low_bits = byte_msb0 ~message:"low_bits"
          and+ _high_bits = byte_msb0 ~message:"high_bits" in
          Error (Unimplemented "Pitch_wheel_change")
      | other ->
          raise
            (Parse_exception
               (sprintf "Unexpected message type identifier: %d" other))
    in
    Result.map (fun message -> { channel; message }) message
end

module Meta_event = struct
  type other = { type_index : int; contents : char array }
  type t = End_of_track | Other of other

  let to_string = function
    | End_of_track -> "End_of_track"
    | Other { type_index; contents = _ } ->
        sprintf "(Other ((type_index %d) (contents ...)))" type_index

  let parse =
    let open Byte_array_parser in
    let* type_index, length = both byte byte in
    match type_index with
    | 0x2F -> return End_of_track
    | _ ->
        let+ contents = n_bytes length in
        Other { type_index; contents }
end

module Message = struct
  type t =
    | Channel_voice_message of Channel_voice_message.t
    | Meta_event of Meta_event.t

  let to_string = function
    | Channel_voice_message channel_voice_message ->
        sprintf "(Channel_voice_message %s)"
          (Channel_voice_message.to_string channel_voice_message)
    | Meta_event meta_event ->
        sprintf "(Meta_event %s)" (Meta_event.to_string meta_event)

  let parse_result status =
    let open Byte_array_parser in
    if status < 0 || status > 255 then raise (Parse_exception "Expected byte");
    if status < 128 then
      raise (Parse_exception "Expected most significant bit to be 1");
    if status == 255 then
      Meta_event.parse >>| fun meta_event -> Ok (Meta_event meta_event)
    else
      let message_type_identifier = (status lsr 4) land 0x7 in
      if message_type_identifier == 7 then
        return (Error (Unimplemented "System_message"))
      else
        Channel_voice_message.parse_result status
        >>| Result.map (fun channel_voice_message ->
                Channel_voice_message channel_voice_message)
end

module Event = struct
  type t = { delta_time : int; message : (Message.t, Unimplemented.t) result }

  let to_string { delta_time; message } =
    let message_string =
      match message with
      | Ok message -> Message.to_string message
      | Error (Unimplemented unimplemented) ->
          sprintf "(Unimplemented %s)" unimplemented
    in
    sprintf "((delta_time %d) (message %s))" delta_time message_string

  let parse_result running_status =
    let open Byte_array_parser in
    let* delta_time = variable_length_quantity and+ next_byte = peek_byte in
    let* status =
      if next_byte >= 128 then
        let+ () = skip 1 in
        next_byte
      else
        match running_status with
        | Some running_status -> return running_status
        | None -> raise (Parse_exception "First event in track lacks status")
    in
    let+ message = Message.parse_result status in
    ({ delta_time; message }, `Status status)
end

module Track = struct
  type t = Event.t list

  let to_string t =
    sprintf "(%s)" (String.concat "\n" (List.map Event.to_string t))

  let parse length =
    let open Byte_array_parser in
    let rec loop acc rem_length running_status =
      if rem_length == 0 then return acc
      else if rem_length < 0 then
        raise
          (Parse_exception
             "Last event in track extends beyond the track boundary")
      else
        let* event, `Status running_status =
          Event.parse_result running_status
        in
        match event.message with
        | Ok (Meta_event End_of_track) -> return (event :: acc)
        | _ -> loop (event :: acc) (rem_length - 1) (Some running_status)
    in
    loop [] length None >>| List.rev
end

module Chunk = struct
  type t = Header of Header.t | Track of Track.t

  let parse_result =
    let open Byte_array_parser in
    let* type_result, length = both Chunk_type.parse_result int32be in
    match type_result with
    | Ok Header ->
        let+ header = Header.parse in
        Ok (Header header)
    | Ok Track ->
        let+ track = Track.parse length in
        Ok (Track track)
    | Error unknown ->
        let+ () = skip length in
        Error unknown
end

module Data = struct
  type t = { header : Header.t; tracks : Track.t list }

  let to_string { header; tracks } =
    sprintf "((header %s)\n(tracks (%s)))" (Header.to_string header)
      (String.concat "\n" (List.map Track.to_string tracks))

  let parse =
    let open Byte_array_parser in
    let+ all_chunk_results = repeat_until_end_exact Chunk.parse_result in
    let t =
      match all_chunk_results with
      | first :: rest ->
          let header =
            match first with
            | Ok (Header header) -> header
            | _ -> raise (Parse_exception "First chunk was not header")
          in
          let tracks =
            List.filter_map
              (function
                | Ok (Chunk.Header _) ->
                    Printf.eprintf "Second header found after first chunk\n";
                    None
                | Ok (Track track) -> Some track
                | Error (Unknown unknown_chunk_type) ->
                    Printf.eprintf "Unknown chunk type: %s\n" unknown_chunk_type;
                    None)
              rest
          in
          { header; tracks }
      | _ -> raise (Parse_exception "No chunks found")
    in
    let num_tracks_according_to_header = Format.num_tracks t.header.format_ in
    let num_tracks_found = List.length t.tracks in
    if num_tracks_according_to_header <> num_tracks_found then
      Printf.eprintf
        "Header implies there should be %d tracks but found %d tracks instead\n"
        num_tracks_according_to_header num_tracks_found;
    t
end

module File_reader = struct
  type t = { path : string }

  let of_path path = { path }

  let read_byte_array t =
    let channel = open_in_bin t.path in
    let rec loop acc =
      match input_char channel with
      | byte -> loop (byte :: acc)
      | exception End_of_file -> List.rev acc
    in
    let byte_list = loop [] in
    close_in channel;
    Array.of_list byte_list

  let read t =
    let byte_array = read_byte_array t in
    Byte_array_parser.run Data.parse byte_array
end
