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
end

module Note : sig
  type t = Note_name.t * int

  val frequency_hz : t -> float
end

val frequency_hz_of_midi_index : int -> float
