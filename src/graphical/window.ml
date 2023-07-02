open Sdl

module Global = struct
  let initialized = ref false

  let init () =
    if not !initialized then (
      Sdl.init [ `VIDEO ];
      initialized := true)
end

type t = { window : Window.t; render : Render.t; fps : float }

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

let create ~title ~width ~height ~fps =
  Global.init ();
  let window, render =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  Window.set_title ~window ~title;
  { window; render; fps }

let render t =
  Render.set_draw_color t.render ~rgb:(100, 100, 100) ~a:255;
  Render.clear t.render;
  Render.render_present t.render

let visualize _t signal = signal

let rec main_loop t =
  let open Lwt.Syntax in
  drain_events t;
  render t;
  let* () = Lwt_unix.sleep (1.0 /. t.fps) in
  main_loop t

let with_lwt ?(title = "Llama") ?(width = 960) ?(height = 720) ?(fps = 60.0) f =
  let open Lwt.Syntax in
  let t = create ~title ~width ~height ~fps in
  let f_lwt = f t in
  let main_loop_lwt = main_loop t in
  let+ _, f_out = Lwt.both main_loop_lwt f_lwt in
  f_out

let with_ ?(title = "Llama") ?(width = 960) ?(height = 720) ?(fps = 60.0) f =
  Lwt_main.run (with_lwt ~title ~width ~height ~fps f)
