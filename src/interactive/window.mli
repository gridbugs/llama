type t

val with_lwt :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?f_delay_s:float ->
  (t -> 'a Lwt.t) ->
  'a Lwt.t

val with_ :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?f_delay_s:float ->
  (t -> 'a Lwt.t) ->
  'a

val visualize :
  t ->
  ?pixel_scale:int ->
  ?sample_scale:float ->
  ?sample_to_rgba_01:(float -> float * float * float * float) ->
  ?stride:int ->
  ?stable:bool ->
  float Llama.Signal.t ->
  float Llama.Signal.t

val input_signals : t -> (Llama.Signal.Gate.t, float Llama.Signal.t) Input.t
