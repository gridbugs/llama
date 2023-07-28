include module type of struct
  include Llama
end

module Window = Window
module Input = Input

val with_window_lwt :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?f_delay_s:float ->
  (Window.t -> 'a Lwt.t) ->
  'a Lwt.t

val with_window :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?f_delay_s:float ->
  (Window.t -> 'a Lwt.t) ->
  'a

val visualize :
  Window.t ->
  ?pixel_scale:int ->
  ?sample_scale:float ->
  ?sample_to_rgba_01:(float -> float * float * float * float) ->
  ?stride:int ->
  ?stable:bool ->
  float Llama.Signal.t ->
  float Llama.Signal.t

val play_signal_visualized_lwt :
  ?downsample:int ->
  ?scale_output_volume:float ->
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?f_delay_s:float ->
  ?pixel_scale:int ->
  ?sample_scale:float ->
  ?sample_to_rgba_01:(float -> Types.rgba_01) ->
  ?stride:int ->
  ?stable:bool ->
  float Signal.t ->
  'a Lwt.t

val play_signal_visualized :
  ?downsample:int ->
  ?scale_output_volume:float ->
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  ?background_rgba_01:Types.rgba_01 ->
  ?f_delay_s:float ->
  ?pixel_scale:int ->
  ?sample_scale:float ->
  ?sample_to_rgba_01:(float -> Types.rgba_01) ->
  ?stride:int ->
  ?stable:bool ->
  float Signal.t ->
  'a
