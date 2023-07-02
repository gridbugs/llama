include module type of struct
  include Stdlib.Float
end

val clamp : t -> min:t -> max:t -> t
val clamp_sym : t -> mag:t -> t
val clamp_01 : t -> t
val clamp_1 : t -> t
