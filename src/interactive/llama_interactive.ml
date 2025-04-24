include Llama
module Window = Window
module Input = Input

let play_signal_visualized ?buffer_size_in_samples ?(title = Defaults.title)
    ?(width = Defaults.width) ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(pixel_scale = Defaults.pixel_scale)
    ?(sample_scale = Defaults.sample_scale)
    ?(sample_to_rgba_01 = Defaults.sample_to_rgb_01) ?(stride = Defaults.stride)
    ?(stable = Defaults.stable) signal =
  let player = Player.create () in
  let playing =
    Player.play_mono player ?buffer_size_in_samples ~viz:(Some Active) signal
  in
  let window =
    Window.create ~title ~width ~height ~fps ~background_rgba_01 ~pixel_scale
      ~sample_scale ~sample_to_rgba_01 ~stride ~stable
      (Player.Playing.viz_queue playing)
  in
  Window.loop window ~on_close:(fun () -> Llama.Player.close player)

let with_visualization_window ?buffer_size_in_samples ?(title = Defaults.title)
    ?(width = Defaults.width) ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(pixel_scale = Defaults.pixel_scale)
    ?(sample_scale = Defaults.sample_scale)
    ?(sample_to_rgba_01 = Defaults.sample_to_rgb_01) ?(stride = Defaults.stride)
    ?(stable = Defaults.stable) mk_signal =
  let viz_queue = Player.Viz_queue.create () in
  let window =
    Window.create ~title ~width ~height ~fps ~background_rgba_01 ~pixel_scale
      ~sample_scale ~sample_to_rgba_01 ~stride ~stable viz_queue
  in
  let signal = mk_signal window in
  let player = Player.create () in
  let _playing =
    Player.play_mono player ?buffer_size_in_samples ~viz:(Some Active)
      ~viz_queue:(`Use viz_queue) signal
  in
  Window.loop window ~on_close:(fun () -> Llama.Player.close player)
