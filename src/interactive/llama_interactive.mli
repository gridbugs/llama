include module type of struct
  include Llama
end

module Window = Window
module Input = Input

val play_signal_visualized :
  ?buffer_size_in_samples:int ->
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
  float Signal.t ->
  unit
(** Take a signal and play it, rendering a visualization in a window. *)

val with_visualization_window :
  ?buffer_size_in_samples:int ->
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
  (Window.t -> float Signal.t) ->
  unit
(** Open a window and generate a signal using a supplied function, rendering a
    visualization in the window. The window is passed to the function, allowing
    interactive signals from the window (keystrokes, mouse movements) to be use
    to generate the signal. *)
