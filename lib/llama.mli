module Live : sig
  val go : unit -> float Signal.t ref

  include module type of struct
    include Dsl
  end
end

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
module Float = Float
