open Llama_midi

let track_signal (track : Track.t) clock =
  let event_array = Array.of_list track in
  let current_index = ref 0 in
  let next_time = ref 0 in
  Signal.of_raw (fun ctx ->
      if !current_index >= Array.length event_array then []
      else if Signal.sample clock ctx then (
        let current_time = !next_time in
        next_time := current_time + 1;
        let next_event = Array.get event_array !current_index in
        if current_time == next_event.delta_time then (
          current_index := !current_index + 1;
          let rec loop acc =
            if !current_index >= Array.length event_array then acc
            else
              let next_event = Array.get event_array !current_index in
              if next_event.delta_time == 0 then (
                current_index := !current_index + 1;
                loop (next_event :: acc))
              else (
                next_time := 0;
                acc)
          in
          List.rev (loop [ next_event ]))
        else [])
      else [])

module Sequencer = struct
  type output_per_voice = {
    frequency_hz : float Signal.t;
    gate : bool Signal.t;
  }

  type voice_state = { note : int; gate : bool }

  let signal num_voices (track_signal : Event.t list Signal.t) =
    let voices =
      Array.init num_voices ~f:(Fun.const { note = 0; gate = false })
    in
    let currently_playing_voice_index_by_note =
      Array.init 128 ~f:(Fun.const None)
    in
    let next_voice_index = ref 0 in
    let signal_to_update_state =
      Signal.of_raw (fun ctx ->
          let voice_messages =
            Signal.sample track_signal ctx
            |> List.filter_map ~f:(fun (event : Event.t) ->
                   match event.message with
                   | Ok (Message.Channel_voice_message voice_message) ->
                       Some voice_message.message
                   | _ -> None)
          in
          List.iter voice_messages ~f:(function
            | Llama_midi.Channel_voice_message.Note_off { note; _ } -> (
                match Array.get currently_playing_voice_index_by_note note with
                | None -> ()
                | Some voice_index ->
                    Array.set currently_playing_voice_index_by_note note None;
                    let voice = Array.get voices voice_index in
                    Array.set voices voice_index { voice with gate = false })
            | Llama_midi.Channel_voice_message.Note_on { note; _ } -> (
                match Array.get currently_playing_voice_index_by_note note with
                | Some _voice_already_assigned_to_note -> ()
                | None ->
                    let voice_index = !next_voice_index in
                    next_voice_index := (voice_index + 1) mod num_voices;
                    (* Store the mapping from note -> voice so that when the note
                       is released we turn off the right voice. *)
                    Array.set currently_playing_voice_index_by_note note
                      (Some voice_index);
                    let current_voice = Array.get voices voice_index in
                    if current_voice.gate then
                      (* Another note is still using that voice. Clear its
                         mapping from note -> voice so when the note is
                         released we don't turn off the voice. *)
                      Array.set currently_playing_voice_index_by_note
                        current_voice.note None;
                    (* Store the mapping from voice to note so if another note takes this
                       voice it can update the fact the the current note no longer holds
                       it. *)
                    Array.set voices voice_index { note; gate = true })))
    in
    List.init ~len:num_voices ~f:(fun i ->
        let frequency_hz =
          Signal.map signal_to_update_state ~f:(fun () ->
              let { note; _ } = Array.get voices i in
              Music.frequency_hz_of_midi_index note)
        in
        let gate =
          Signal.map signal_to_update_state ~f:(fun () ->
              let { gate; _ } = Array.get voices i in
              gate)
        in
        { frequency_hz; gate })
end
