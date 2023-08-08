open StdLabels
include Llama_midi

module Controller_table = struct
  type t = float Signal.t array

  let num_controllers = 128
  let create_refs () = Array.init num_controllers ~f:(fun _ -> ref 0.0)
  let get_raw = Array.get
  let modulation t = get_raw t 1
  let volume t = get_raw t 7
end

let pitch_wheel_to_pitch_multiplier =
  let pitch_wheel_max = 8192.0 in
  let max_ratio = Music.semitone_ratio 2.0 in
  fun pitch_wheel ->
    let pitch_wheel_1 = Int.to_float pitch_wheel /. pitch_wheel_max in
    Float.pow max_ratio pitch_wheel_1

module Gate_table = struct
  type t = Signal.Gate.t array

  let get = Array.get
end

module Midi_sequencer = struct
  let num_notes = 128

  type voice = {
    frequency_hz : float Signal.t;
    gate : Signal.Gate.t;
    velocity : int Signal.t;
  }

  type output = {
    voices : voice list;
    pitch_wheel_multiplier : float Signal.t;
    controller_table : Controller_table.t;
  }

  type voice_state = { note : int; gate : bool; velocity : int }

  let key_gates ~channel (messages : Message.t list Signal.t) =
    let ref_array = Array.init num_notes ~f:(fun _ -> ref false) in
    let update_signal =
      Signal.of_raw (fun ctx ->
          let voice_messages =
            Signal.sample messages ctx
            |> List.filter_map ~f:(fun (message : Message.t) ->
                   match message with
                   | Message.Channel_voice_message voice_message ->
                       if voice_message.channel == channel then
                         Some voice_message.message
                       else None
                   | _ -> None)
          in
          List.iter voice_messages ~f:(fun message ->
              match message with
              | Llama_midi.Channel_voice_message.Note_off { note; _ } ->
                  Array.get ref_array note := false
              | Note_on { note; _ } -> Array.get ref_array note := true
              | _ -> ()))
    in
    Array.init num_notes ~f:(fun i ->
        Signal.map update_signal ~f:(fun _ -> !(Array.get ref_array i))
        |> Signal.gate)

  let signal ~channel ~polyphony (messages : Message.t list Signal.t) =
    let voices =
      Array.init polyphony
        ~f:(Fun.const { note = 0; gate = false; velocity = 0 })
    in
    let find_free_voice_index () =
      let rec loop i =
        if i >= Array.length voices then None
        else if not (Array.get voices i).gate then Some i
        else loop (i + 1)
      in
      loop 0
    in
    let currently_playing_voice_index_by_note =
      Array.init num_notes ~f:(Fun.const None)
    in
    let controller_refs = Controller_table.create_refs () in
    let pitch_wheel_multiplier = ref 1.0 in
    let next_voice_index = ref 0 in
    let signal_to_update_state =
      Signal.of_raw (fun ctx ->
          let voice_messages =
            Signal.sample messages ctx
            |> List.filter_map ~f:(fun (message : Message.t) ->
                   match message with
                   | Message.Channel_voice_message voice_message ->
                       if voice_message.channel == channel then
                         Some voice_message.message
                       else None
                   | _ -> None)
          in
          List.iter voice_messages ~f:(fun message ->
              match message with
              | Llama_midi.Channel_voice_message.Note_off { note; velocity }
                -> (
                  match
                    Array.get currently_playing_voice_index_by_note note
                  with
                  | None -> ()
                  | Some voice_index ->
                      Array.set currently_playing_voice_index_by_note note None;
                      let voice = Array.get voices voice_index in
                      Array.set voices voice_index
                        { voice with gate = false; velocity })
              | Note_on { note; velocity } -> (
                  match
                    Array.get currently_playing_voice_index_by_note note
                  with
                  | Some voice_index_already_assigned_to_note ->
                      (* Update the velocity *)
                      Array.set voices voice_index_already_assigned_to_note
                        { note; gate = true; velocity }
                  | None ->
                      let voice_index = !next_voice_index in
                      next_voice_index := (voice_index + 1) mod polyphony;
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
                      Array.set voices voice_index
                        { note; gate = true; velocity })
              | Pitch_wheel_change { signed_value } ->
                  pitch_wheel_multiplier :=
                    pitch_wheel_to_pitch_multiplier signed_value
              | Control_change { controller; value } ->
                  let ref = Array.get controller_refs controller in
                  ref := Int.to_float value /. 127.0
              | _ -> ()))
    in
    let voices =
      List.init ~len:polyphony ~f:(fun i ->
          let frequency_hz =
            Signal.map signal_to_update_state ~f:(fun () ->
                let { note; _ } = Array.get voices i in
                Music.frequency_hz_of_midi_index note)
          in
          let gate =
            Signal.map signal_to_update_state ~f:(fun () ->
                let { gate; _ } = Array.get voices i in
                gate)
            |> Signal.gate
          in
          let velocity =
            Signal.map signal_to_update_state ~f:(fun () ->
                let { velocity; _ } = Array.get voices i in
                velocity)
          in
          { frequency_hz; gate; velocity })
    in
    let pitch_wheel_multiplier = Signal.of_ref pitch_wheel_multiplier in
    let controller_table =
      Array.map controller_refs ~f:(fun controller_ref ->
          Signal.map signal_to_update_state ~f:(fun () -> !controller_ref))
    in
    { voices; pitch_wheel_multiplier; controller_table }
end

let track_signal (track : Track.t) clock =
  let event_array = Array.of_list track in
  let current_index = ref 0 in
  let next_time = ref 0 in
  Signal.of_raw (fun ctx ->
      if !current_index >= Array.length event_array then []
      else if Signal.Trigger.sample clock ctx then (
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
  |> Signal.map ~f:(List.map ~f:(fun (event : Event.t) -> event.message))
