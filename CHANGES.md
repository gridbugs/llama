## Unreleased

- Remove rust dependency. This removes support for live midi, decoding wav
  files, and windows, but simplifies the build dramatically and allows support
  for more recent versions of the compiler. APIs have been changed decouple
  visualization from playback and to better accommodate libao.

## 0.1.0

- Introduce `Signal.Gate.t` and `Signal.Trigger.t` wrappers of `bool Signal.t`
- Rename `clock` constructor to `clock_of_frequency_hz`
- Use `floatarray` instead of `float array`
- Add `saturation` operator
- Add `map2` and `map3` combinators
- Add missing `downsample` argument to visualized signal players
- Stop exporting labelled stdlib modules
- Add tests in new package
- Rename filter arguments to be more consistent and intuitive
- Rename `Square` waveform to `Pulse`
- Live midi support in core library
- Add package for parsing and representing midi data
- Rename ASR envelope generator to "AR"
- Rename `Output_stream.create_with_downsample` to `Output_stream.create`

## 0.0.1

- Initial release
