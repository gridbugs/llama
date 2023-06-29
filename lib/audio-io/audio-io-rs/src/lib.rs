use cpal::{
    traits::{DeviceTrait, HostTrait, StreamTrait},
    Device, OutputCallbackInfo, SizedSample, Stream, StreamConfig,
};
use std::sync::{mpsc, Arc, RwLock};

struct OutputStreamCore {
    device: Device,
    config: StreamConfig,
}

impl OutputStreamCore {
    fn new() -> anyhow::Result<Self> {
        let host = cpal::default_host();
        log::info!("cpal host: {}", host.id().name());
        let device = host
            .default_output_device()
            .ok_or(anyhow::anyhow!("no output device"))?;
        if let Ok(name) = device.name() {
            log::info!("cpal device: {}", name);
        } else {
            log::info!("cpal device: (no name)");
        }
        let config = device.default_output_config()?;
        log::info!("sample format: {}", config.sample_format());
        log::info!("sample rate: {}", config.sample_rate().0);
        log::info!("num channels: {}", config.channels());
        let config = StreamConfig::from(config);
        Ok(Self { device, config })
    }
}

struct OutputStream<T> {
    core: OutputStreamCore,
    #[allow(unused)]
    stream: Stream,
    sender: mpsc::Sender<T>,
    sink_cursor: Arc<RwLock<u64>>,
    buffer_padding: u64,
    source_cursor: u64,
    downsample: u32,
}

impl<T: SizedSample + Send + 'static> OutputStream<T> {
    pub fn new_with_downsample(downsample: u32) -> anyhow::Result<Self> {
        assert!(downsample > 0, "downsample must be positive");
        let (sender, receiver) = mpsc::channel::<T>();
        let sink_cursor = Arc::new(RwLock::new(0));
        let sink_cursor_for_cpal_thread = Arc::clone(&sink_cursor);
        let core = OutputStreamCore::new()?;
        let channels = core.config.channels;
        let stream = core.device.build_output_stream(
            &core.config,
            move |data: &mut [T], _: &OutputCallbackInfo| {
                let mut sink_cursor = sink_cursor_for_cpal_thread.write().unwrap();
                for output in data.chunks_mut(channels as usize * downsample as usize) {
                    if let Ok(input) = receiver.try_recv() {
                        for element in output {
                            *element = input;
                        }
                        *sink_cursor += 1;
                    } else {
                        break;
                    }
                }
            },
            |err| log::error!("stream error: {}", err),
            None,
        )?;
        stream.play()?;
        let buffer_padding = core.config.sample_rate.0 as u64 / 20;
        Ok(Self {
            core,
            buffer_padding,
            stream,
            sender,
            sink_cursor,
            source_cursor: 0,
            downsample,
        })
    }

    fn sample_rate_hz(&self) -> u32 {
        self.core.config.sample_rate.0 / self.downsample
    }

    fn buffer_padding_mut(&mut self) -> &mut u64 {
        &mut self.buffer_padding
    }

    fn play_sample(&mut self, sample: T) {
        if let Err(_) = self.sender.send(sample) {
            log::error!("failed to send data to cpal thread");
        }
        self.source_cursor += 1;
    }

    fn samples_behind(&self) -> u64 {
        let sink_cursor = *self.sink_cursor.read().unwrap();
        let target_source_cursor = sink_cursor + self.buffer_padding;
        target_source_cursor - self.source_cursor
    }
}

pub struct OutputStreamOcaml {
    output_stream: OutputStream<f32>,
}

impl OutputStreamOcaml {
    fn new_with_downsample(i: i32) -> Self {
        assert!(i > 0, "downsample must be positive");
        Self {
            output_stream: OutputStream::new_with_downsample(i as u32)
                .expect("failed to create output stream"),
        }
    }

    fn sample_rate_hz(&self) -> i32 {
        self.output_stream.sample_rate_hz() as i32
    }

    fn num_channels(&self) -> i32 {
        self.output_stream.core.config.channels as i32
    }

    fn set_buffer_padding(&mut self, buffer_padding: i32) {
        assert!(buffer_padding >= 0, "buffer padding must be non-negactive");
        *self.output_stream.buffer_padding_mut() = buffer_padding as u64;
    }

    fn samples_behind(&self) -> i32 {
        self.output_stream.samples_behind() as i32
    }

    fn send_sample(&mut self, sample: f32) {
        self.output_stream.play_sample(sample);
    }
}

#[ocaml::func]
pub fn env_logger_init() {
    env_logger::init();
}

unsafe extern "C" fn output_stream_finalizer(v: ocaml::Raw) {
    let ptr = v.as_pointer::<OutputStreamOcaml>();
    ptr.drop_in_place()
}
ocaml::custom_finalize!(OutputStreamOcaml, output_stream_finalizer);

#[ocaml::func]
pub fn create_output_stream_with_downsample(downsample: i32) -> ocaml::Pointer<OutputStreamOcaml> {
    ocaml::Pointer::alloc_custom(OutputStreamOcaml::new_with_downsample(downsample))
}

#[ocaml::func]
pub fn sample_rate_hz(t: ocaml::Pointer<OutputStreamOcaml>) -> i32 {
    let output_stream = t.as_ref();
    output_stream.sample_rate_hz()
}

#[ocaml::func]
pub fn num_channels(t: ocaml::Pointer<OutputStreamOcaml>) -> i32 {
    let output_stream = t.as_ref();
    output_stream.num_channels()
}

/// The "buffer padding" is the target amount to over-fill the buffer to prevent gaps in the sample
/// stream presented to the audio device. Increasing this value will increase the latency between
/// updating the stream and hearing the result, but will reduce the chance that the device will run
/// out of samples, resulting in choppy sound. This value will depend on how quickly (in realtitme)
/// the application can add samples to the buffer (by calling `play_sample` or `play_stream`), so
/// it's influenced by your computer's speed and how much work is being done between updating the
/// buffer. It defaults to 1/20 of the sample rate.
#[ocaml::func]
pub fn set_buffer_padding(mut t: ocaml::Pointer<OutputStreamOcaml>, buffer_padding: i32) {
    let output_stream = t.as_mut();
    output_stream.set_buffer_padding(buffer_padding);
}

#[ocaml::func]
pub fn samples_behind(t: ocaml::Pointer<OutputStreamOcaml>) -> i32 {
    let output_stream = t.as_ref();
    output_stream.samples_behind()
}

#[ocaml::func]
pub fn send_sample(mut t: ocaml::Pointer<OutputStreamOcaml>, sample: f32) {
    let output_stream = t.as_mut();
    output_stream.send_sample(sample);
}
