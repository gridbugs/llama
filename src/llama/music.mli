module Note_name : sig
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

  val to_string : t -> string
end

module Note : sig
  type t = Note_name.t * int

  val frequency_hz : t -> float
  val to_midi_index : t -> int
  val of_midi_index : int -> t
end

val frequency_hz_of_midi_index : int -> float
