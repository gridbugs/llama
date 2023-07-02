type t

val with_lwt :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  (t -> 'a Lwt.t) ->
  'a Lwt.t

val with_ :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?fps:float ->
  (t -> 'a Lwt.t) ->
  'a

val visualize : t -> float Llama.Signal.t -> float Llama.Signal.t
