module Butterworth : sig
  type t = { signal : float Signal.t; half_power_frequency_hz : float Signal.t }

  val signal_low_pass : t -> filter_order_half:int -> float Signal.t
  val signal_high_pass : t -> filter_order_half:int -> float Signal.t
end

module Chebyshev : sig
  type t = {
    signal : float Signal.t;
    cutoff_hz : float Signal.t;
    epsilon : float Signal.t;
  }

  val signal_low_pass : t -> filter_order_half:int -> float Signal.t
  val signal_high_pass : t -> filter_order_half:int -> float Signal.t
end
