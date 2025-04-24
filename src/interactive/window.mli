type t

val input_signals : t -> (Llama.Signal.Gate.t, float Llama.Signal.t) Input.t

val create :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?pixel_scale:int ->
  ?sample_scale:float ->
  ?sample_to_rgba_01:(float -> Types.rgba_01) ->
  ?stride:int ->
  ?stable:bool ->
  float Llama.Player.Viz_queue.t ->
  t

val loop : t -> on_close:(unit -> unit) -> 'a
