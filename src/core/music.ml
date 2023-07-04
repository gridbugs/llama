module Note_name = struct
  type t =
    [ `C
    | `C_sharp
    | `D
    | `D_sharp
    | `E
    | `F
    | `F_sharp
    | `G
    | `G_sharp
    | `A
    | `A_sharp
    | `B ]

  let to_string = function
    | `C -> "C"
    | `C_sharp -> "C_sharp"
    | `D -> "D"
    | `D_sharp -> "D_sharp"
    | `E -> "E"
    | `F -> "F"
    | `F_sharp -> "F_sharp"
    | `G -> "G"
    | `G_sharp -> "G_sharp"
    | `A -> "A"
    | `A_sharp -> "A_sharp"
    | `B -> "B"

  let all_index_order =
    [
      `C;
      `C_sharp;
      `D;
      `D_sharp;
      `E;
      `F;
      `F_sharp;
      `G;
      `G_sharp;
      `A;
      `A_sharp;
      `B;
    ]

  let to_index = function
    | `C -> 0
    | `C_sharp -> 1
    | `D -> 2
    | `D_sharp -> 3
    | `E -> 4
    | `F -> 5
    | `F_sharp -> 6
    | `G -> 7
    | `G_sharp -> 8
    | `A -> 9
    | `A_sharp -> 10
    | `B -> 11
end

let a4_frequency = 440.0
let a4_index = 57
let notes_per_octave = 12

let frequency_hz_of_midi_index index =
  a4_frequency
  *. Float.pow 2.0
       (Int.to_float (index - a4_index) /. Int.to_float notes_per_octave)

module Note = struct
  type t = Note_name.t * int

  let to_midi_index (note_name, octave) =
    (octave * notes_per_octave) + Note_name.to_index note_name

  let frequency_hz t = frequency_hz_of_midi_index (to_midi_index t)

  let of_midi_index i =
    let note_name =
      List.nth Note_name.all_index_order (i mod notes_per_octave)
    in
    let octave = i / notes_per_octave in
    (note_name, octave)
end
