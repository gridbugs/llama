open StdLabels
open Llama_interactive

module Voice = struct
  type t = { frequency_hz : float; gate : bool Signal.t }
end

let voices_for_key_row ~key_gates_in_order ~note_for_second_key =
  let base_index = Music.Note.to_midi_index note_for_second_key - 1 in
  List.mapi key_gates_in_order ~f:(fun i gate ->
      let midi_index = base_index + i in
      let frequency_hz = Music.frequency_hz_of_midi_index midi_index in
      { Voice.frequency_hz; gate })

let voices ~(keys : bool Signal.t Input.All_keyboard.t)
    ~mid_note:(note_name, octave_index) =
  let top_row =
    [
      keys.key_q;
      keys.key_w;
      keys.key_3;
      keys.key_e;
      keys.key_4;
      keys.key_r;
      keys.key_t;
      keys.key_6;
      keys.key_y;
      keys.key_7;
      keys.key_u;
      keys.key_8;
      keys.key_i;
      keys.key_o;
      keys.key_0;
      keys.key_p;
    ]
  in
  let bottom_row =
    [
      keys.key_z;
      keys.key_x;
      keys.key_d;
      keys.key_c;
      keys.key_f;
      keys.key_v;
      keys.key_b;
      keys.key_h;
      keys.key_n;
      keys.key_j;
      keys.key_m;
      keys.key_k;
      keys.key_comma;
      keys.key_period;
      keys.key_semicolon;
    ]
  in
  voices_for_key_row ~key_gates_in_order:top_row
    ~note_for_second_key:(note_name, octave_index)
  @ voices_for_key_row ~key_gates_in_order:bottom_row
      ~note_for_second_key:(note_name, octave_index - 1)
