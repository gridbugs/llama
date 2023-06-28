let a = "b"

type t

external hello : unit -> unit = "hello"

external new_player : unit -> t = "new_player"

external example : t -> float -> int32 = "player_example"
