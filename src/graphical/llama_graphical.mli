module Window = Window

val with_window_lwt :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  (Window.t -> 'a Lwt.t) ->
  'a Lwt.t

val with_window :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  (Window.t -> 'a Lwt.t) ->
  'a

val visualize : Window.t -> float Llama.Signal.t -> float Llama.Signal.t
