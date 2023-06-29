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

  let index = function
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

module Note = struct
  type t = Note_name.t * int

  let a4_frequency = 440.0
  let a4_index = 57
  let notes_per_octave = 12

  let frequency_hz_of_note_index index =
    a4_frequency
    *. Float.pow 2.0
         (Int.to_float (index - a4_index) /. Int.to_float notes_per_octave)

  let frequency_hz (note_name, octave) =
    let note_index = (octave * notes_per_octave) + Note_name.index note_name in
    frequency_hz_of_note_index note_index
end
