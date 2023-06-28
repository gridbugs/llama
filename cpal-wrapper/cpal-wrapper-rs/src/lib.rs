use cpal::{
    traits::{DeviceTrait, HostTrait, StreamTrait},
    Device, StreamConfig,
};
use std::sync::{mpsc, Arc, RwLock};

pub struct SamplePlayerCore {
    device: Device,
    config: StreamConfig,
}

impl SamplePlayerCore {
    fn new() -> Self {
        let host = cpal::default_host();
        let device = host.default_output_device().unwrap();
        let config = device.default_output_config().unwrap();
        let config = StreamConfig::from(config);
        Self { device, config }
    }
}

pub unsafe fn make_player() -> SamplePlayerCore {
    SamplePlayerCore::new()
}
unsafe extern "C" fn player_finalizer(v: ocaml::Raw) {
    let ptr = v.as_pointer::<SamplePlayerCore>();
    ptr.drop_in_place()
}
ocaml::custom_finalize!(SamplePlayerCore, player_finalizer);

#[ocaml::func]
pub fn new_player() -> ocaml::Pointer<SamplePlayerCore> {
    ocaml::Pointer::alloc_custom(SamplePlayerCore::new())
}
/*

#[ocaml::func]
pub fn player_example(mut t: ocaml::Pointer<SamplePlayerCore>) -> i32 {
    let player = t.as_mut();
    let _ = player.config.sample_rate.0;
    42
}
*/
#[ocaml::func]
pub unsafe fn hello() {
    println!("hi");
}
