open Tsdl
open StdLabels
module Signal = Llama.Signal
module Ctx = Signal.Ctx

module Global = struct
  let initialized = ref false

  let init () =
    if not !initialized then
      match Sdl.init Sdl.Init.video with
      | Error (`Msg msg) ->
          Sdl.log "Error initializing sdl: %s" msg;
          exit 1
      | Ok () -> initialized := true
end

let f_01_to_byte f = Llama.Float.clamp_01 f *. 255.0 |> Float.to_int

let rgba_01_to_bytes (r, g, b, a) =
  (f_01_to_byte r, f_01_to_byte g, f_01_to_byte b, f_01_to_byte a)

module Rect_rgba = struct
  type t = { sdl_rect : Sdl.rect; rgb : int * int * int; a : int }
end

module Visualization_style = struct
  type t = {
    pixel_scale : int;
    sample_scale : float;
    sample_to_rgba_01 : float -> Types.rgba_01;
  }
end

module Sample_buffer = struct
  type t = { samples : floatarray; mutable next_i : int }

  let create size =
    let samples = Float.Array.init size (Fun.const 0.0) in
    { samples; next_i = 0 }

  let length t = t.next_i
  let is_full t = t.next_i == Float.Array.length t.samples

  let append_unless_full t sample =
    if not (is_full t) then (
      Float.Array.set t.samples t.next_i sample;
      t.next_i <- t.next_i + 1)

  let clear t = t.next_i <- 0

  let first_positive_gradient_zero_cross_index t =
    let rec loop i =
      if i >= t.next_i then None
      else
        let prev = Float.Array.get t.samples (i - 1) in
        let current = Float.Array.get t.samples i in
        if prev <= 0.0 && current >= 0.0 then Some i else loop (i + 1)
    in
    loop 1

  let iteri t ~offset ~stride ~max_iterations ~f =
    let rec loop i count =
      if i >= t.next_i || count >= max_iterations then ()
      else (
        f count (Float.Array.get t.samples i);
        loop (i + stride) (count + 1))
    in
    loop offset 0
end

module Visualization = struct
  type t = {
    style : Visualization_style.t;
    sample_buffer : Sample_buffer.t;
    sample_count_within_current_frame : int option ref;
    stable : bool;
    stride : int;
  }

  let sample_buffer_size = 2048

  let create ~style ~stable ~stride =
    {
      style;
      sample_buffer = Sample_buffer.create sample_buffer_size;
      sample_count_within_current_frame = ref None;
      stable;
      stride;
    }

  let iteri_samples t ~window_width ~f =
    let needed_num_samples = (window_width / t.stride) + 1 in
    let offset =
      if t.stable then
        match
          Sample_buffer.first_positive_gradient_zero_cross_index t.sample_buffer
        with
        | Some i ->
            let remaining_samples_if_we_start_at_i =
              (Sample_buffer.length t.sample_buffer - i) / t.stride
            in
            if remaining_samples_if_we_start_at_i >= needed_num_samples then i
            else 0
        | None -> 0
      else 0
    in
    Sample_buffer.iteri t.sample_buffer ~offset ~stride:t.stride
      ~max_iterations:needed_num_samples ~f

  let scaled_pixel_y_of_sample sample ~window_height ~pixel_scale ~sample_scale
      =
    let window_y_mid = Int.to_float (window_height / 2) in
    Float.to_int (window_y_mid -. (sample *. sample_scale *. window_y_mid))
    / pixel_scale

  let rect_rgba_drain_iter t ~window_size:(window_width, window_height) ~f =
    let scaled_pixel_y_of_sample =
      scaled_pixel_y_of_sample ~window_height ~pixel_scale:t.style.pixel_scale
        ~sample_scale:t.style.sample_scale
    in
    let window_y_mid = Int.to_float (window_height / 2) in
    let scaled_pixel_y_to_sample scaled_pixel_y =
      (window_y_mid
      -. (Int.to_float scaled_pixel_y *. Int.to_float t.style.pixel_scale))
      /. (t.style.sample_scale *. window_y_mid)
    in
    let mk_rect_rgba ~scaled_pixel_y ~i =
      let x = i * t.style.pixel_scale in
      let interpolated_sample = scaled_pixel_y_to_sample scaled_pixel_y in
      let r, g, b, a =
        rgba_01_to_bytes
          (t.style.sample_to_rgba_01
             (t.style.sample_scale *. interpolated_sample))
      in
      let sdl_rect =
        Sdl.Rect.create ~x
          ~y:(scaled_pixel_y * t.style.pixel_scale)
          ~w:t.style.pixel_scale ~h:t.style.pixel_scale
      in
      { Rect_rgba.sdl_rect; rgb = (r, g, b); a }
    in
    let prev_sample = ref None in
    iteri_samples t ~window_width ~f:(fun i sample ->
        let scaled_pixel_y = scaled_pixel_y_of_sample sample in
        f (mk_rect_rgba ~scaled_pixel_y ~i);
        (match !prev_sample with
        | None -> ()
        | Some prev_sample ->
            (* fill in the vertical space between the previous and current sample *)
            let scaled_pixel_y0 = scaled_pixel_y_of_sample prev_sample in
            let scaled_pixel_y1 = scaled_pixel_y_of_sample sample in
            let scaled_pixel_y_values =
              if scaled_pixel_y0 < scaled_pixel_y1 then
                List.init ~len:(scaled_pixel_y1 - scaled_pixel_y0) ~f:(fun i ->
                    scaled_pixel_y0 + i)
              else if scaled_pixel_y0 > scaled_pixel_y1 then
                List.init
                  ~len:(scaled_pixel_y0 - scaled_pixel_y1 + 1)
                  ~f:(fun i -> scaled_pixel_y1 + i)
              else []
            in
            List.iter scaled_pixel_y_values ~f:(fun scaled_pixel_y ->
                f (mk_rect_rgba ~scaled_pixel_y ~i)));
        prev_sample := Some sample);
    Sample_buffer.clear t.sample_buffer;
    t.sample_count_within_current_frame := None
end

let create_inputs () =
  let open Input in
  let keyboard = All_keyboard.init ~f:(fun () -> Signal.var false) in
  let keyboard_signals =
    All_keyboard.map keyboard ~f:(fun (key, _) -> Signal.Gate.of_signal key)
  in
  let keyboard_refs = All_keyboard.map keyboard ~f:snd in
  let mouse = Mouse_pos.init ~f:(fun () -> Signal.var 0.0) in
  let mouse_signals = Mouse_pos.map mouse ~f:fst in
  let mouse_refs = Mouse_pos.map mouse ~f:snd in
  let signals = { keyboard = keyboard_signals; mouse = mouse_signals } in
  let refs = { keyboard = keyboard_refs; mouse = mouse_refs } in
  (signals, refs)

let key_of_scancode (all_keyboard : 'a Input.All_keyboard.t)
    (scancode : Sdl.scancode) =
  match Sdl.Scancode.enum scancode with
  | `A -> Some all_keyboard.key_a
  | `B -> Some all_keyboard.key_b
  | `C -> Some all_keyboard.key_c
  | `D -> Some all_keyboard.key_d
  | `E -> Some all_keyboard.key_e
  | `F -> Some all_keyboard.key_f
  | `G -> Some all_keyboard.key_g
  | `H -> Some all_keyboard.key_h
  | `I -> Some all_keyboard.key_i
  | `J -> Some all_keyboard.key_j
  | `K -> Some all_keyboard.key_k
  | `L -> Some all_keyboard.key_l
  | `M -> Some all_keyboard.key_m
  | `N -> Some all_keyboard.key_n
  | `O -> Some all_keyboard.key_o
  | `P -> Some all_keyboard.key_p
  | `Q -> Some all_keyboard.key_q
  | `R -> Some all_keyboard.key_r
  | `S -> Some all_keyboard.key_s
  | `T -> Some all_keyboard.key_t
  | `U -> Some all_keyboard.key_u
  | `V -> Some all_keyboard.key_v
  | `W -> Some all_keyboard.key_w
  | `X -> Some all_keyboard.key_x
  | `Y -> Some all_keyboard.key_y
  | `Z -> Some all_keyboard.key_z
  | `Semicolon -> Some all_keyboard.key_semicolon
  | `Apostrophe -> Some all_keyboard.key_apostrophe
  | `Comma -> Some all_keyboard.key_comma
  | `Period -> Some all_keyboard.key_period
  | `Space -> Some all_keyboard.key_space
  | `K1 -> Some all_keyboard.key_1
  | `K2 -> Some all_keyboard.key_2
  | `K3 -> Some all_keyboard.key_3
  | `K4 -> Some all_keyboard.key_4
  | `K5 -> Some all_keyboard.key_5
  | `K6 -> Some all_keyboard.key_6
  | `K7 -> Some all_keyboard.key_7
  | `K8 -> Some all_keyboard.key_8
  | `K9 -> Some all_keyboard.key_9
  | `K0 -> Some all_keyboard.key_0
  | _ -> None

type t = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  fps : float;
  background_rgba_01 : Types.rgba_01;
  visualization : Visualization.t option ref;
  viz_queue : float Llama.Player.Viz_queue.t option;
  input_signals : (Signal.Gate.t, float Signal.t) Input.t;
  input_refs : (bool ref, float ref) Input.t;
}

let proc_event t event ~on_close =
  let typ = Sdl.Event.get event Sdl.Event.typ in
  if typ == Sdl.Event.quit then (
    Sdl.quit ();
    on_close ();
    exit 0)
  else if typ == Sdl.Event.key_down then
    let scancode = Sdl.Event.get event Sdl.Event.keyboard_scancode in
    match key_of_scancode t.input_refs.keyboard scancode with
    | Some key_ref -> key_ref := true
    | None -> ()
  else if typ == Sdl.Event.key_up then
    let scancode = Sdl.Event.get event Sdl.Event.keyboard_scancode in
    match key_of_scancode t.input_refs.keyboard scancode with
    | Some key_ref -> key_ref := false
    | None -> ()
  else if typ == Sdl.Event.mouse_motion then (
    let mm_x = Sdl.Event.get event Sdl.Event.mouse_motion_x in
    let mm_y = Sdl.Event.get event Sdl.Event.mouse_motion_y in
    let window_width, window_height = Sdl.get_window_size t.window in
    let mouse_x_01 = Float.of_int mm_x /. Float.of_int window_width in
    let mouse_y_01 = Float.of_int mm_y /. Float.of_int window_height in
    t.input_refs.mouse.mouse_x := mouse_x_01;
    t.input_refs.mouse.mouse_y := mouse_y_01)

let rec drain_events t ~on_close =
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then (
    proc_event t event ~on_close;
    drain_events t ~on_close)
  else ()

let log_error = function Ok () -> () | Error (`Msg msg) -> Sdl.log "%s" msg

let render t =
  let r, g, b, a = rgba_01_to_bytes t.background_rgba_01 in
  Sdl.set_render_draw_color t.renderer r g b a |> log_error;
  Sdl.render_clear t.renderer |> log_error;
  (match !(t.visualization) with
  | None -> ()
  | Some visualization ->
      Visualization.rect_rgba_drain_iter visualization
        ~window_size:(Sdl.get_window_size t.window)
        ~f:(fun { Rect_rgba.sdl_rect; rgb = r, g, b; a } ->
          Sdl.set_render_draw_color t.renderer r g b a |> log_error;
          Sdl.render_fill_rect t.renderer (Some sdl_rect) |> log_error));
  Sdl.render_present t.renderer

let create ?(title = Defaults.title) ?(width = Defaults.width)
    ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(pixel_scale = Defaults.pixel_scale)
    ?(sample_scale = Defaults.sample_scale)
    ?(sample_to_rgba_01 = Defaults.sample_to_rgb_01) ?(stride = Defaults.stride)
    ?(stable = Defaults.stable) viz_queue =
  Global.init ();
  match
    Sdl.create_window_and_renderer ~w:width ~h:height Sdl.Window.windowed
  with
  | Error (`Msg msg) ->
      Sdl.log "Error creating window: %s" msg;
      exit 1
  | Ok (window, renderer) ->
      let input_signals, input_refs = create_inputs () in
      Sdl.set_window_title window title;
      let style =
        { Visualization_style.pixel_scale; sample_scale; sample_to_rgba_01 }
      in
      let visualization = Visualization.create ~style ~stable ~stride in
      {
        window;
        renderer;
        fps;
        background_rgba_01;
        visualization = ref (Some visualization);
        viz_queue = Some viz_queue;
        input_signals;
        input_refs;
      }

let render' t =
  (match !(t.visualization) with
  | None -> ()
  | Some visualization -> (
      match t.viz_queue with
      | None -> ()
      | Some viz_queue ->
          Llama.Player.Viz_queue.drain viz_queue ~f:(fun sample ->
              Sample_buffer.append_unless_full visualization.sample_buffer
                sample)));
  render t

let frame t ~on_close =
  render' t;
  drain_events t ~on_close;
  Thread.delay (1. /. t.fps)

let rec loop t ~on_close =
  frame t ~on_close;
  loop t ~on_close

let input_signals t = t.input_signals
