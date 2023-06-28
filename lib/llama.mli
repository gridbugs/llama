module Interactive : sig
  val go : unit -> float Signal.t ref

  include Dsl.S
  include Signal.Ops
end

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
