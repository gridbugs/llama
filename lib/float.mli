include module type of struct
  include Stdlib.Float
end

val clamp : t -> min:t -> max:t -> t
val clamp01 : t -> t
val clamp1 : t -> t
