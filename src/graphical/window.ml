open Sdl
module Signal = Llama.Signal
module Ctx = Signal.Ctx
module List = Llama.List

module Global = struct
  let initialized = ref false

  let init () =
    if not !initialized then (
      Sdl.init [ `VIDEO ];
      initialized := true)
end

let f_01_to_byte f = Llama.Float.clamp_01 f *. 255.0 |> Float.to_int

let rgba_01_to_bytes (r, g, b, a) =
  (f_01_to_byte r, f_01_to_byte g, f_01_to_byte b, f_01_to_byte a)

module Rect_rgba = struct
  type t = { sdl_rect : Rect.t; rgb : int * int * int; a : int }
end

module Visualization_style = struct
  type t = {
    pixel_scale : int;
    sample_scale : float;
    sample_to_rgba_01 : float -> Types.rgba_01;
  }
end

module Visualization = struct
  type t = {
    style : Visualization_style.t;
    sample_buffer : float Queue.t;
    sample_count_within_current_frame : int option ref;
  }

  let create style =
    {
      style;
      sample_buffer = Queue.create ();
      sample_count_within_current_frame = ref None;
    }

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
    let prev_sample = ref None in
    let rec loop i =
      if Queue.is_empty t.sample_buffer then ()
      else
        let x = i * t.style.pixel_scale in
        let sample = Queue.take t.sample_buffer in
        let scaled_pixel_y = scaled_pixel_y_of_sample sample in
        let interpolated_sample = scaled_pixel_y_to_sample scaled_pixel_y in
        let r, g, b, a =
          rgba_01_to_bytes (t.style.sample_to_rgba_01 interpolated_sample)
        in
        let sdl_rect =
          {
            Rect.h = t.style.pixel_scale;
            w = t.style.pixel_scale;
            x;
            y = scaled_pixel_y * t.style.pixel_scale;
          }
        in
        let rect_rgba = { Rect_rgba.sdl_rect; rgb = (r, g, b); a } in
        f rect_rgba;
        (match !prev_sample with
        | None -> ()
        | Some prev_sample ->
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
                let interpolated_sample =
                  scaled_pixel_y_to_sample scaled_pixel_y
                in
                let r, g, b, a =
                  rgba_01_to_bytes
                    (t.style.sample_to_rgba_01 interpolated_sample)
                in
                let sdl_rect =
                  {
                    Rect.h = t.style.pixel_scale;
                    w = t.style.pixel_scale;
                    x;
                    y = scaled_pixel_y * t.style.pixel_scale;
                  }
                in
                let rect_rgba = { Rect_rgba.sdl_rect; rgb = (r, g, b); a } in
                f rect_rgba));
        prev_sample := Some sample;
        if x >= window_width then () else loop (i + 1)
    in
    loop 0;
    Queue.clear t.sample_buffer;
    t.sample_count_within_current_frame := None
end

type t = {
  window : Window.t;
  render : Render.t;
  fps : float;
  background_rgba_01 : Types.rgba_01;
  visualization : Visualization.t option ref;
}

let proc_events _t = function
  | Event.KeyDown { keycode = Keycode.Q; _ }
  | Event.KeyDown { keycode = Keycode.Escape; _ }
  | Event.Quit _ ->
      Sdl.quit ();
      exit 0
  | _ -> ()

let rec drain_events t =
  match Event.poll_event () with
  | None -> ()
  | Some ev ->
      proc_events t ev;
      drain_events t

let create ~title ~width ~height ~fps ~background_rgba_01 =
  Global.init ();
  let window, render =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  Window.set_title ~window ~title;
  { window; render; fps; background_rgba_01; visualization = ref None }

let render t =
  let r, g, b, a = rgba_01_to_bytes t.background_rgba_01 in
  Render.set_draw_color t.render ~rgb:(r, g, b) ~a;
  Render.clear t.render;
  (match !(t.visualization) with
  | None -> ()
  | Some visualization ->
      Visualization.rect_rgba_drain_iter visualization
        ~window_size:(Window.get_size t.window)
        ~f:(fun { Rect_rgba.sdl_rect; rgb; a } ->
          Render.set_draw_color t.render ~rgb ~a;
          Render.fill_rect t.render sdl_rect));
  Render.render_present t.render

let visualize t ?(pixel_scale = Defaults.pixel_scale)
    ?(sample_scale = Defaults.sample_scale)
    ?(sample_to_rgba_01 = Defaults.sample_to_rgb_01) ?(stride = Defaults.stride)
    ?(stable = Defaults.stable) signal =
  let style =
    { Visualization_style.pixel_scale; sample_scale; sample_to_rgba_01 }
  in
  let visualization = Visualization.create style in
  t.visualization := Some visualization;
  let prev_sample = ref 0.0 in
  Signal.of_raw (fun (ctx : Ctx.t) ->
      let sample = Signal.sample signal ctx in
      (* Wait until the signal is crossing 0 with a positive gradient before
         starting to collect samples for this frame so the visualization is
         stable. When the counter is [Some _] it indicates that we've started
         recording samples this frame. *)
      if (not stable) || (sample >= 0.0 && !prev_sample < 0.0) then
        visualization.sample_count_within_current_frame := Some 0;
      (match !(visualization.sample_count_within_current_frame) with
      | None -> ()
      | Some count ->
          if Int.equal (count mod stride) 0 then
            Queue.add sample visualization.sample_buffer;
          visualization.sample_count_within_current_frame := Some (count + 1));
      prev_sample := sample;
      sample)

let rec main_loop t =
  let open Lwt.Syntax in
  drain_events t;
  render t;
  let* () = Lwt_unix.sleep (1.0 /. t.fps) in
  main_loop t

let with_lwt ?(title = Defaults.title) ?(width = Defaults.width)
    ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(f_delay_s = Defaults.f_delay_s) f =
  let open Lwt.Syntax in
  let t = create ~title ~width ~height ~fps ~background_rgba_01 in
  let f_lwt =
    (* Wait before running the user function to give the window time to open
       and get ready. [f] is probably going to start doing realtime things such as
       run oscillators and opening the window can consume a lot of cpu which can
       interrupt the oscillation and lead to audible artifacts. *)
    let* () = Lwt_unix.sleep f_delay_s in
    f t
  in
  let main_loop_lwt = main_loop t in
  let+ _, f_out = Lwt.both main_loop_lwt f_lwt in
  f_out

let with_ ?(title = Defaults.title) ?(width = Defaults.width)
    ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(f_delay_s = Defaults.f_delay_s) f =
  Lwt_main.run
    (with_lwt ~title ~width ~height ~fps ~background_rgba_01 ~f_delay_s f)
