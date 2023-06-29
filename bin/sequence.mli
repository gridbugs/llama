open Llama

type t = (Music.Note.t * float) option list

val to_steps : t -> time_scale:float -> Dsl.step_sequencer_step option list
val middle_c_loop : t
val hsc : t
