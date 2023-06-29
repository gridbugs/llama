include Stdlib.Float

let clamp t ~min:min_ ~max:max_ = max (min t max_) min_
let clamp01 t = clamp t ~min:0.0 ~max:1.0
let clamp1 t = clamp t ~min:(-1.0) ~max:1.0
