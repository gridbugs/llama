include Llama
module Window = Window
module Input = Input

let with_window_lwt = Window.with_lwt
let with_window = Window.with_
let visualize = Window.visualize

let play_signal_visualized_lwt ?(downsample = 1) ?(scale_output_volume = 1.0)
    ?(title = Defaults.title) ?(width = Defaults.width)
    ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(f_delay_s = Defaults.f_delay_s) ?(pixel_scale = Defaults.pixel_scale)
    ?(sample_scale = Defaults.sample_scale)
    ?(sample_to_rgba_01 = Defaults.sample_to_rgb_01) ?(stride = Defaults.stride)
    ?(stable = Defaults.stable) signal =
  with_window_lwt ~title ~width ~height ~fps ~background_rgba_01 ~f_delay_s
    (fun window ->
      let viz'd_signal =
        visualize ~pixel_scale ~sample_scale ~sample_to_rgba_01 ~stride ~stable
          window signal
      in
      play_signal_lwt ~downsample ~scale_output_volume viz'd_signal)

let play_signal_visualized ?(downsample = 1) ?(scale_output_volume = 1.0)
    ?(title = Defaults.title) ?(width = Defaults.width)
    ?(height = Defaults.height) ?(fps = Defaults.fps)
    ?(background_rgba_01 = Defaults.background_rgba_01)
    ?(f_delay_s = Defaults.f_delay_s) ?(pixel_scale = Defaults.pixel_scale)
    ?(sample_scale = Defaults.sample_scale)
    ?(sample_to_rgba_01 = Defaults.sample_to_rgb_01) ?(stride = Defaults.stride)
    ?(stable = Defaults.stable) signal =
  Lwt_main.run
    (play_signal_visualized_lwt ~downsample ~scale_output_volume ~title ~width
       ~height ~fps ~background_rgba_01 ~f_delay_s ~pixel_scale ~sample_scale
       ~sample_to_rgba_01 ~stride ~stable signal)
