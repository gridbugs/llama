module Channel_voice_message : sig
  type note_event = { note : int; velocity : int }
  type polyphonic_key_pressure = { note : int; pressure : int }
  type control_change = { controller : int; value : int }
  type program_change = { program : int }
  type channel_pressure = { pressure : int }
  type pitch_wheel_change = { signed_value : int }

  type message =
    | Note_off of note_event
    | Note_on of note_event
    | Polyphonic_key_pressure of polyphonic_key_pressure
    | Control_change of control_change
    | Program_change of program_change
    | Channel_pressure of channel_pressure
    | Pitch_wheel_change of pitch_wheel_change

  type t = { channel : int; message : message }

  val message_to_string : message -> string
  val to_string : t -> string
end

module System_message : sig
  type system_exclusive = { manufacturer_id : int; payload : int list }

  type t =
    | System_exclusive of system_exclusive
    | Song_position_pointer of int
    | Song_select of int
    | Tune_request
    | Timing_clock
    | Start
    | Continue
    | Stop
    | Active_sensing
    | Reset
    | Undefined of int

  val to_string : t -> string
end

module Meta_event : sig
  type other = { type_index : int; contents : char array }
  type t = End_of_track | Other of other

  val to_string : t -> string
end

module Message : sig
  type t =
    | Channel_voice_message of Channel_voice_message.t
    | System_message of System_message.t
    | Meta_event of Meta_event.t

  val to_string : t -> string
end

module Event : sig
  type t = { delta_time : int; message : Message.t }

  val to_string : t -> string
  val parse_multi_from_char_array : char array -> t list
end

module Track : sig
  type t = Event.t list

  val to_string : t -> string
end

module Format : sig
  type t =
    | Single_track
    | Simultaneous_tracks of int
    | Sequential_tracks of int

  val to_string : t -> string
end

module Division : sig
  type time_code = { smpte_format : int; ticks_per_frame : int }
  type t = Ticks_per_quarter_note of int | Time_code of time_code

  val to_string : t -> string
end

module Header : sig
  type t = { format_ : Format.t; division : Division.t }
end

module Data : sig
  type t = { header : Header.t; tracks : Track.t list }

  val to_string : t -> string
end

module File_reader : sig
  type t

  val of_path : string -> t
  val read : t -> Data.t
end

module File_writer : sig
  type t

  val of_path : string -> t
  val write : t -> Data.t -> unit
end
