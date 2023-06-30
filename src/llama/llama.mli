module Live : sig
  val go : unit -> float Signal.t ref

  include module type of struct
    include Dsl
  end
end

val play_signal : ?downsample:int -> float Signal.t -> unit

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
module Float = Float
module Music = Music
module List = List
module Array = Array
