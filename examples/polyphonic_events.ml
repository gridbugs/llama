(* Example which implements a polyphonic synthesizer for playing events in a
   custom format *)

open Llama
open Dsl

type event = { note : Music.Note.t; timestamp : int; duration : int }

module Event_player_polyphonic = struct
  type t = { num_voices : int; events : event list; clock : bool Signal.t }
  type voice_state = { release_time : int; current_frequency_hz : float }

  type state = {
    counter : int;
    next_array_index : int;
    next_voice_index : int;
    voices : voice_state list;
  }

  let init_state n =
    {
      counter = 0;
      next_array_index = 0;
      next_voice_index = 0;
      voices =
        List.init ~len:n ~f:(fun _ ->
            { release_time = 0; current_frequency_hz = 0.0 });
    }

  let rec list_set_nth xs n value =
    assert (n >= 0);
    match xs with
    | x :: xs ->
        if n == 0 then value :: xs else x :: list_set_nth xs (n - 1) value
    | [] -> failwith "index out of bounds"

  type output = { frequency_hz : float Signal.t; gate : bool Signal.t }

  let signal t =
    let events_sorted_by_timestamp =
      List.sort t.events ~cmp:(fun a b -> Int.compare a.timestamp b.timestamp)
      |> Array.of_list
    in
    let combined =
      Raw.with_state ~init:(init_state t.num_voices) ~f:(fun state ctx ->
          if Signal.sample t.clock ctx then
            let rec loop state =
              if
                state.next_array_index < Array.length events_sorted_by_timestamp
              then
                let next_event =
                  Array.get events_sorted_by_timestamp state.next_array_index
                in
                if Int.equal next_event.timestamp state.counter then
                  let next_array_index =
                    (state.next_array_index + 1)
                    mod Array.length events_sorted_by_timestamp
                  in
                  let current_frequency_hz =
                    Music.Note.frequency_hz next_event.note
                  in
                  let release_time = state.counter + next_event.duration in
                  let voice_state = { release_time; current_frequency_hz } in
                  let voices =
                    list_set_nth state.voices state.next_voice_index voice_state
                  in
                  let next_voice_index =
                    (state.next_array_index + 1) mod t.num_voices
                  in
                  loop { state with next_array_index; next_voice_index; voices }
                else state
              else state
            in
            let state = loop state in
            { state with counter = state.counter + 1 }
          else state)
      |> Raw.map ~f:(fun state ->
             List.map state.voices ~f:(fun voice ->
                 (voice.current_frequency_hz, voice.release_time > state.counter)))
      |> Signal.of_raw
    in
    List.init ~len:t.num_voices ~f:(fun i ->
        {
          frequency_hz = Signal.map combined ~f:(fun xs -> List.nth xs i |> fst);
          gate = Signal.map combined ~f:(fun xs -> List.nth xs i |> snd);
        })
end

let play_events events =
  let num_voices = 4 in
  let sequencer_clock = clock (const 480.0) in
  let poly_seq_outputs =
    { Event_player_polyphonic.num_voices; events; clock = sequencer_clock }
    |> Event_player_polyphonic.signal
  in
  let synth =
    List.map poly_seq_outputs
      ~f:(fun { Event_player_polyphonic.frequency_hz; gate } ->
        let oscillator_frequency_hz = frequency_hz |> scale 0.5 in
        let osc =
          mean
            [
              oscillator (const Saw) oscillator_frequency_hz;
              oscillator (const Saw) (oscillator_frequency_hz |> scale 2.0)
              |> scale 0.95;
              oscillator (const Saw) (oscillator_frequency_hz |> scale 4.0)
              |> scale 0.9;
              oscillator (const Saw) (oscillator_frequency_hz |> scale 8.0)
              |> scale 0.85;
            ]
        in
        let release_s = const 0.2 in
        let filter_env =
          adsr_linear ~gate ~attack_s:(const 0.01) ~decay_s:(const 0.4)
            ~sustain_01:(const 1.0) ~release_s
          |> exp_01 1.0
        in
        let filtered_osc =
          butterworth_low_pass_filter osc
            ~half_power_frequency_hz:(filter_env |> scale 4000.0 |> offset 200.0)
          |> butterworth_high_pass_filter ~half_power_frequency_hz:(const 500.0)
        in
        filtered_osc *.. ar_linear ~gate ~attack_s:(const 0.05) ~release_s
        |> map ~f:(fun x -> x))
    |> sum
  in
  let with_delay =
    delay synth ~time_s:(const 0.1) ~fill:0.0
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 2000.0)
  in
  let with_delay2 =
    delay synth ~time_s:(const 0.2) ~fill:0.0
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 1000.0)
  in
  synth +.. (with_delay |> scale 0.9) +.. (with_delay2 |> scale 0.8)

let events =
  [
    { note = (`C, 5); timestamp = 90; duration = 90 };
    { note = (`D, 5); timestamp = 180; duration = 90 };
    { note = (`E, 5); timestamp = 270; duration = 90 };
    { note = (`F, 5); timestamp = 360; duration = 90 };
    { note = (`D, 5); timestamp = 450; duration = 90 };
    { note = (`E, 5); timestamp = 540; duration = 90 };
    { note = (`C, 5); timestamp = 630; duration = 90 };
    { note = (`G, 5); timestamp = 720; duration = 180 };
    { note = (`C, 4); timestamp = 810; duration = 90 };
    { note = (`D, 4); timestamp = 900; duration = 90 };
    { note = (`C, 6); timestamp = 900; duration = 180 };
    { note = (`E, 4); timestamp = 990; duration = 90 };
    { note = (`C, 6); timestamp = 1080; duration = 30 };
    { note = (`B, 5); timestamp = 1110; duration = 30 };
    { note = (`C, 6); timestamp = 1140; duration = 30 };
    { note = (`F, 4); timestamp = 1080; duration = 90 };
    { note = (`B, 5); timestamp = 1170; duration = 90 };
    { note = (`D, 4); timestamp = 1170; duration = 90 };
    { note = (`E, 4); timestamp = 1260; duration = 90 };
    { note = (`C, 6); timestamp = 1260; duration = 180 };
    { note = (`C, 4); timestamp = 1350; duration = 90 };
    { note = (`D, 6); timestamp = 1440; duration = 90 };
    { note = (`G, 5); timestamp = 1530; duration = 90 };
    { note = (`G, 4); timestamp = 1440; duration = 180 };
    { note = (`A, 5); timestamp = 1620; duration = 90 };
    { note = (`B, 5); timestamp = 1710; duration = 90 };
    { note = (`G, 3); timestamp = 1620; duration = 180 };
    { note = (`C, 6); timestamp = 1800; duration = 90 };
    { note = (`A, 5); timestamp = 1890; duration = 90 };
    { note = (`B, 5); timestamp = 1980; duration = 90 };
    { note = (`G, 5); timestamp = 2070; duration = 90 };
    { note = (`D, 6); timestamp = 2160; duration = 180 };
    { note = (`G, 4); timestamp = 2250; duration = 90 };
    { note = (`A, 4); timestamp = 2340; duration = 90 };
    { note = (`G, 6); timestamp = 2340; duration = 180 };
    { note = (`B, 4); timestamp = 2430; duration = 90 };
    { note = (`G, 6); timestamp = 2520; duration = 30 };
    { note = (`F, 6); timestamp = 2550; duration = 30 };
    { note = (`G, 6); timestamp = 2580; duration = 30 };
    { note = (`C, 5); timestamp = 2520; duration = 90 };
    { note = (`F, 6); timestamp = 2610; duration = 90 };
    { note = (`A, 4); timestamp = 2610; duration = 90 };
    { note = (`B, 4); timestamp = 2700; duration = 90 };
    { note = (`G, 6); timestamp = 2700; duration = 180 };
    { note = (`G, 4); timestamp = 2790; duration = 90 };
    { note = (`E, 6); timestamp = 2880; duration = 90 };
    { note = (`A, 6); timestamp = 2970; duration = 90 };
    { note = (`C, 5); timestamp = 2880; duration = 180 };
    { note = (`G, 6); timestamp = 3060; duration = 90 };
    { note = (`F, 6); timestamp = 3150; duration = 90 };
    { note = (`B, 4); timestamp = 3060; duration = 180 };
    { note = (`E, 6); timestamp = 3240; duration = 90 };
    { note = (`G, 6); timestamp = 3330; duration = 90 };
    { note = (`C, 5); timestamp = 3240; duration = 180 };
    { note = (`F, 6); timestamp = 3420; duration = 90 };
    { note = (`A, 6); timestamp = 3510; duration = 90 };
    { note = (`D, 5); timestamp = 3420; duration = 180 };
    { note = (`G, 6); timestamp = 3600; duration = 90 };
    { note = (`F, 6); timestamp = 3690; duration = 90 };
    { note = (`E, 5); timestamp = 3600; duration = 180 };
    { note = (`E, 6); timestamp = 3780; duration = 90 };
    { note = (`D, 6); timestamp = 3870; duration = 90 };
    { note = (`G, 4); timestamp = 3780; duration = 180 };
    { note = (`C, 6); timestamp = 3960; duration = 90 };
    { note = (`E, 6); timestamp = 4050; duration = 90 };
    { note = (`A, 4); timestamp = 3960; duration = 180 };
    { note = (`D, 6); timestamp = 4140; duration = 90 };
    { note = (`F, 6); timestamp = 4230; duration = 90 };
    { note = (`B, 4); timestamp = 4140; duration = 180 };
    { note = (`E, 6); timestamp = 4320; duration = 90 };
    { note = (`D, 6); timestamp = 4410; duration = 90 };
    { note = (`C, 5); timestamp = 4320; duration = 180 };
    { note = (`C, 6); timestamp = 4500; duration = 90 };
    { note = (`B, 5); timestamp = 4590; duration = 90 };
    { note = (`E, 4); timestamp = 4500; duration = 180 };
    { note = (`A, 5); timestamp = 4680; duration = 90 };
    { note = (`C, 6); timestamp = 4770; duration = 90 };
    { note = (`F_sharp, 4); timestamp = 4680; duration = 180 };
    { note = (`B, 5); timestamp = 4860; duration = 90 };
    { note = (`D, 6); timestamp = 4950; duration = 90 };
    { note = (`G, 4); timestamp = 4860; duration = 180 };
    { note = (`C, 6); timestamp = 5040; duration = 90 };
    { note = (`B, 5); timestamp = 5130; duration = 90 };
    { note = (`A, 4); timestamp = 5040; duration = 180 };
    { note = (`A, 5); timestamp = 5220; duration = 90 };
    { note = (`G, 5); timestamp = 5310; duration = 90 };
    { note = (`B, 4); timestamp = 5220; duration = 180 };
    { note = (`F_sharp, 5); timestamp = 5400; duration = 90 };
    { note = (`A, 5); timestamp = 5490; duration = 90 };
    { note = (`G, 5); timestamp = 5580; duration = 90 };
    { note = (`B, 5); timestamp = 5670; duration = 90 };
    { note = (`C, 5); timestamp = 5400; duration = 435 };
    { note = (`A, 5); timestamp = 5760; duration = 180 };
    { note = (`D, 4); timestamp = 5850; duration = 90 };
    { note = (`E, 4); timestamp = 5940; duration = 90 };
    { note = (`D, 5); timestamp = 5940; duration = 180 };
    { note = (`F_sharp, 4); timestamp = 6030; duration = 90 };
    { note = (`C, 6); timestamp = 6120; duration = 45 };
    { note = (`B, 5); timestamp = 6165; duration = 45 };
    { note = (`G, 4); timestamp = 6120; duration = 90 };
    { note = (`E, 4); timestamp = 6210; duration = 90 };
    { note = (`C, 6); timestamp = 6210; duration = 180 };
    { note = (`F_sharp, 4); timestamp = 6300; duration = 90 };
    { note = (`D, 6); timestamp = 6390; duration = 90 };
    { note = (`D, 4); timestamp = 6390; duration = 90 };
    { note = (`B, 5); timestamp = 6480; duration = 90 };
    { note = (`A, 5); timestamp = 6570; duration = 90 };
    { note = (`G, 4); timestamp = 6480; duration = 180 };
    { note = (`G, 5); timestamp = 6660; duration = 90 };
    { note = (`F_sharp, 5); timestamp = 6750; duration = 90 };
    { note = (`B, 3); timestamp = 6660; duration = 180 };
    { note = (`E, 5); timestamp = 6840; duration = 90 };
    { note = (`G, 5); timestamp = 6930; duration = 90 };
    { note = (`C, 4); timestamp = 6840; duration = 180 };
    { note = (`F_sharp, 5); timestamp = 7020; duration = 90 };
    { note = (`A, 5); timestamp = 7110; duration = 90 };
    { note = (`D, 4); timestamp = 7020; duration = 180 };
    { note = (`G, 5); timestamp = 7200; duration = 90 };
    { note = (`B, 5); timestamp = 7290; duration = 90 };
    { note = (`E, 4); timestamp = 7200; duration = 180 };
    { note = (`A, 5); timestamp = 7380; duration = 90 };
    { note = (`C, 6); timestamp = 7470; duration = 90 };
    { note = (`F_sharp, 4); timestamp = 7380; duration = 180 };
    { note = (`B, 5); timestamp = 7560; duration = 90 };
    { note = (`D, 6); timestamp = 7650; duration = 90 };
    { note = (`G, 4); timestamp = 7560; duration = 180 };
    { note = (`C, 6); timestamp = 7740; duration = 90 };
    { note = (`E, 6); timestamp = 7830; duration = 90 };
    { note = (`E, 4); timestamp = 7740; duration = 180 };
    { note = (`D, 6); timestamp = 7920; duration = 90 };
    { note = (`B, 5); timestamp = 8010; duration = 45 };
    { note = (`C, 6); timestamp = 8055; duration = 45 };
    { note = (`D, 6); timestamp = 8100; duration = 90 };
    { note = (`B, 3); timestamp = 7920; duration = 270 };
    { note = (`G, 6); timestamp = 8190; duration = 90 };
    { note = (`C, 4); timestamp = 8190; duration = 90 };
    { note = (`C, 6); timestamp = 8280; duration = 30 };
    { note = (`B, 5); timestamp = 8310; duration = 30 };
    { note = (`C, 6); timestamp = 8340; duration = 30 };
    { note = (`B, 5); timestamp = 8370; duration = 90 };
    { note = (`D, 4); timestamp = 8280; duration = 180 };
    { note = (`A, 5); timestamp = 8460; duration = 90 };
    { note = (`G, 5); timestamp = 8550; duration = 90 };
    { note = (`D, 3); timestamp = 8460; duration = 180 };
    { note = (`G, 5); timestamp = 8640; duration = 180 };
    { note = (`G, 3); timestamp = 8730; duration = 90 };
    { note = (`A, 3); timestamp = 8820; duration = 90 };
    { note = (`B, 3); timestamp = 8910; duration = 90 };
    { note = (`C, 4); timestamp = 9000; duration = 90 };
    { note = (`A, 3); timestamp = 9090; duration = 90 };
    { note = (`B, 3); timestamp = 9180; duration = 90 };
    { note = (`G, 3); timestamp = 9270; duration = 90 };
    { note = (`G, 5); timestamp = 9450; duration = 90 };
    { note = (`D, 4); timestamp = 9360; duration = 180 };
    { note = (`A, 5); timestamp = 9540; duration = 90 };
    { note = (`B, 5); timestamp = 9630; duration = 90 };
    { note = (`G, 4); timestamp = 9540; duration = 180 };
    { note = (`C, 6); timestamp = 9720; duration = 90 };
    { note = (`A, 5); timestamp = 9810; duration = 90 };
    { note = (`F_sharp, 4); timestamp = 9720; duration = 180 };
    { note = (`B, 5); timestamp = 9900; duration = 90 };
    { note = (`G, 5); timestamp = 9990; duration = 90 };
    { note = (`G, 4); timestamp = 9900; duration = 180 };
    { note = (`G, 5); timestamp = 10080; duration = 30 };
    { note = (`F_sharp, 5); timestamp = 10110; duration = 30 };
    { note = (`G, 5); timestamp = 10140; duration = 30 };
    { note = (`A, 4); timestamp = 10080; duration = 90 };
    { note = (`F_sharp, 5); timestamp = 10170; duration = 90 };
    { note = (`D, 4); timestamp = 10170; duration = 90 };
    { note = (`E, 4); timestamp = 10260; duration = 90 };
    { note = (`F_sharp, 4); timestamp = 10350; duration = 90 };
    { note = (`G, 4); timestamp = 10440; duration = 90 };
    { note = (`E, 4); timestamp = 10530; duration = 90 };
    { note = (`F_sharp, 4); timestamp = 10620; duration = 90 };
    { note = (`D, 4); timestamp = 10710; duration = 90 };
    { note = (`A, 5); timestamp = 10890; duration = 90 };
    { note = (`A, 4); timestamp = 10800; duration = 180 };
    { note = (`B, 5); timestamp = 10980; duration = 90 };
    { note = (`C, 6); timestamp = 11070; duration = 90 };
    { note = (`D, 5); timestamp = 10980; duration = 180 };
    { note = (`D, 6); timestamp = 11160; duration = 90 };
    { note = (`B, 5); timestamp = 11250; duration = 90 };
    { note = (`C, 5); timestamp = 11160; duration = 180 };
    { note = (`C, 6); timestamp = 11340; duration = 90 };
    { note = (`A, 5); timestamp = 11430; duration = 90 };
    { note = (`D, 5); timestamp = 11340; duration = 180 };
    { note = (`G, 4); timestamp = 11520; duration = 90 };
    { note = (`B, 5); timestamp = 11520; duration = 180 };
    { note = (`G, 5); timestamp = 11610; duration = 90 };
    { note = (`F, 5); timestamp = 11700; duration = 90 };
    { note = (`E, 5); timestamp = 11790; duration = 90 };
    { note = (`D, 5); timestamp = 11880; duration = 90 };
    { note = (`F, 5); timestamp = 11970; duration = 90 };
    { note = (`E, 5); timestamp = 12060; duration = 90 };
    { note = (`G, 5); timestamp = 12150; duration = 90 };
    { note = (`D, 6); timestamp = 12330; duration = 90 };
    { note = (`F, 5); timestamp = 12240; duration = 180 };
    { note = (`C, 6); timestamp = 12420; duration = 90 };
    { note = (`B, 5); timestamp = 12510; duration = 90 };
    { note = (`E, 5); timestamp = 12420; duration = 180 };
    { note = (`A, 5); timestamp = 12600; duration = 90 };
    { note = (`C, 6); timestamp = 12690; duration = 90 };
    { note = (`F, 5); timestamp = 12600; duration = 180 };
    { note = (`B, 5); timestamp = 12780; duration = 90 };
    { note = (`D, 6); timestamp = 12870; duration = 90 };
    { note = (`D, 5); timestamp = 12780; duration = 180 };
    { note = (`E, 5); timestamp = 12960; duration = 90 };
    { note = (`C, 6); timestamp = 12960; duration = 180 };
    { note = (`A, 5); timestamp = 13050; duration = 90 };
    { note = (`G, 5); timestamp = 13140; duration = 90 };
    { note = (`F, 5); timestamp = 13230; duration = 90 };
    { note = (`E, 5); timestamp = 13320; duration = 90 };
    { note = (`G, 5); timestamp = 13410; duration = 90 };
    { note = (`F, 5); timestamp = 13500; duration = 90 };
    { note = (`A, 5); timestamp = 13590; duration = 90 };
    { note = (`E, 6); timestamp = 13770; duration = 90 };
    { note = (`G, 5); timestamp = 13680; duration = 180 };
    { note = (`D, 6); timestamp = 13860; duration = 90 };
    { note = (`C, 6); timestamp = 13950; duration = 90 };
    { note = (`F, 5); timestamp = 13860; duration = 180 };
    { note = (`B, 5); timestamp = 14040; duration = 90 };
    { note = (`D, 6); timestamp = 14130; duration = 90 };
    { note = (`G, 5); timestamp = 14040; duration = 180 };
    { note = (`C_sharp, 6); timestamp = 14220; duration = 90 };
    { note = (`E, 6); timestamp = 14310; duration = 90 };
    { note = (`E, 5); timestamp = 14220; duration = 180 };
    { note = (`F, 5); timestamp = 14400; duration = 90 };
    { note = (`D, 6); timestamp = 14400; duration = 180 };
    { note = (`A_sharp, 5); timestamp = 14490; duration = 90 };
    { note = (`A, 5); timestamp = 14580; duration = 90 };
    { note = (`C_sharp, 6); timestamp = 14580; duration = 180 };
    { note = (`G, 5); timestamp = 14670; duration = 90 };
    { note = (`F, 5); timestamp = 14760; duration = 90 };
    { note = (`D, 6); timestamp = 14760; duration = 180 };
    { note = (`A, 5); timestamp = 14850; duration = 90 };
    { note = (`G, 5); timestamp = 14940; duration = 90 };
    { note = (`E, 6); timestamp = 14940; duration = 180 };
    { note = (`A_sharp, 5); timestamp = 15030; duration = 90 };
    { note = (`A, 5); timestamp = 15120; duration = 90 };
    { note = (`F, 6); timestamp = 15120; duration = 180 };
    { note = (`G, 5); timestamp = 15210; duration = 90 };
    { note = (`F, 5); timestamp = 15300; duration = 90 };
    { note = (`A, 5); timestamp = 15300; duration = 180 };
    { note = (`E, 5); timestamp = 15390; duration = 90 };
    { note = (`D, 5); timestamp = 15480; duration = 90 };
    { note = (`B, 5); timestamp = 15480; duration = 180 };
    { note = (`F, 5); timestamp = 15570; duration = 90 };
    { note = (`E, 5); timestamp = 15660; duration = 90 };
    { note = (`C_sharp, 6); timestamp = 15660; duration = 180 };
    { note = (`G, 5); timestamp = 15750; duration = 90 };
    { note = (`F, 5); timestamp = 15840; duration = 90 };
    { note = (`D, 6); timestamp = 15840; duration = 180 };
    { note = (`E, 5); timestamp = 15930; duration = 90 };
    { note = (`D, 5); timestamp = 16020; duration = 90 };
    { note = (`F_sharp, 5); timestamp = 16020; duration = 180 };
    { note = (`C, 5); timestamp = 16110; duration = 90 };
    { note = (`B, 4); timestamp = 16200; duration = 90 };
    { note = (`G_sharp, 5); timestamp = 16200; duration = 180 };
    { note = (`D, 5); timestamp = 16290; duration = 90 };
    { note = (`C, 5); timestamp = 16380; duration = 90 };
    { note = (`A, 5); timestamp = 16380; duration = 180 };
    { note = (`E, 5); timestamp = 16470; duration = 90 };
    { note = (`D, 5); timestamp = 16560; duration = 90 };
    { note = (`B, 5); timestamp = 16560; duration = 180 };
    { note = (`C, 5); timestamp = 16650; duration = 90 };
    { note = (`B, 4); timestamp = 16740; duration = 90 };
    { note = (`C, 6); timestamp = 16740; duration = 180 };
    { note = (`A, 4); timestamp = 16830; duration = 90 };
    { note = (`G_sharp, 4); timestamp = 16920; duration = 90 };
    { note = (`B, 4); timestamp = 17010; duration = 90 };
    { note = (`A, 4); timestamp = 17100; duration = 90 };
    { note = (`C, 5); timestamp = 17190; duration = 90 };
    { note = (`D, 6); timestamp = 16920; duration = 450 };
    { note = (`E, 5); timestamp = 17370; duration = 90 };
    { note = (`B, 4); timestamp = 17280; duration = 180 };
    { note = (`F_sharp, 5); timestamp = 17460; duration = 90 };
    { note = (`G_sharp, 5); timestamp = 17550; duration = 90 };
    { note = (`E, 4); timestamp = 17460; duration = 180 };
    { note = (`D, 5); timestamp = 17640; duration = 45 };
    { note = (`A, 5); timestamp = 17640; duration = 90 };
    { note = (`C, 5); timestamp = 17685; duration = 45 };
    { note = (`F_sharp, 5); timestamp = 17730; duration = 90 };
    { note = (`G_sharp, 5); timestamp = 17820; duration = 90 };
    { note = (`D, 5); timestamp = 17730; duration = 180 };
    { note = (`E, 5); timestamp = 17910; duration = 90 };
    { note = (`E, 6); timestamp = 18000; duration = 90 };
    { note = (`C, 5); timestamp = 18000; duration = 90 };
    { note = (`D, 6); timestamp = 18090; duration = 90 };
    { note = (`B, 4); timestamp = 18090; duration = 90 };
    { note = (`C, 6); timestamp = 18180; duration = 90 };
    { note = (`A, 4); timestamp = 18180; duration = 90 };
    { note = (`E, 6); timestamp = 18270; duration = 90 };
    { note = (`G, 4); timestamp = 18270; duration = 90 };
    { note = (`D, 6); timestamp = 18360; duration = 90 };
    { note = (`F_sharp, 4); timestamp = 18360; duration = 90 };
    { note = (`C, 6); timestamp = 18450; duration = 90 };
    { note = (`A, 4); timestamp = 18450; duration = 90 };
    { note = (`B, 5); timestamp = 18540; duration = 90 };
    { note = (`G_sharp, 4); timestamp = 18540; duration = 90 };
    { note = (`D, 6); timestamp = 18630; duration = 90 };
    { note = (`B, 4); timestamp = 18630; duration = 90 };
    { note = (`C, 6); timestamp = 18720; duration = 90 };
    { note = (`A, 4); timestamp = 18720; duration = 90 };
    { note = (`A, 6); timestamp = 18810; duration = 90 };
    { note = (`C, 5); timestamp = 18810; duration = 90 };
    { note = (`G_sharp, 6); timestamp = 18900; duration = 90 };
    { note = (`B, 4); timestamp = 18900; duration = 90 };
    { note = (`B, 6); timestamp = 18990; duration = 90 };
    { note = (`D, 5); timestamp = 18990; duration = 90 };
    { note = (`A, 6); timestamp = 19080; duration = 90 };
    { note = (`C, 5); timestamp = 19080; duration = 90 };
    { note = (`E, 6); timestamp = 19170; duration = 90 };
    { note = (`E, 5); timestamp = 19170; duration = 90 };
    { note = (`F, 6); timestamp = 19260; duration = 90 };
    { note = (`D, 5); timestamp = 19260; duration = 90 };
    { note = (`D, 6); timestamp = 19350; duration = 90 };
    { note = (`F, 5); timestamp = 19350; duration = 90 };
    { note = (`G_sharp, 5); timestamp = 19440; duration = 90 };
    { note = (`F, 6); timestamp = 19530; duration = 90 };
    { note = (`E, 5); timestamp = 19440; duration = 180 };
    { note = (`E, 6); timestamp = 19620; duration = 90 };
    { note = (`D, 6); timestamp = 19710; duration = 90 };
    { note = (`A, 4); timestamp = 19620; duration = 180 };
    { note = (`C, 6); timestamp = 19800; duration = 180 };
    { note = (`E, 5); timestamp = 19800; duration = 180 };
    { note = (`B, 5); timestamp = 19980; duration = 90 };
    { note = (`A, 5); timestamp = 20070; duration = 90 };
    { note = (`E, 4); timestamp = 19980; duration = 180 };
    { note = (`A, 5); timestamp = 20160; duration = 90 };
    { note = (`A, 6); timestamp = 20250; duration = 90 };
    { note = (`A, 4); timestamp = 20160; duration = 180 };
    { note = (`G, 6); timestamp = 20340; duration = 90 };
    { note = (`F, 6); timestamp = 20430; duration = 90 };
    { note = (`A, 3); timestamp = 20340; duration = 180 };
    { note = (`E, 6); timestamp = 20520; duration = 90 };
    { note = (`G, 6); timestamp = 20610; duration = 90 };
    { note = (`F, 6); timestamp = 20700; duration = 90 };
    { note = (`A, 6); timestamp = 20790; duration = 90 };
    { note = (`E, 5); timestamp = 20970; duration = 90 };
    { note = (`D, 5); timestamp = 21060; duration = 90 };
    { note = (`C, 5); timestamp = 21150; duration = 90 };
    { note = (`B, 4); timestamp = 21240; duration = 90 };
    { note = (`D, 5); timestamp = 21330; duration = 90 };
    { note = (`C_sharp, 5); timestamp = 21420; duration = 90 };
    { note = (`E, 5); timestamp = 21510; duration = 90 };
    { note = (`G, 6); timestamp = 20880; duration = 795 };
    { note = (`E, 6); timestamp = 21690; duration = 90 };
    { note = (`F, 6); timestamp = 21780; duration = 90 };
    { note = (`G, 6); timestamp = 21870; duration = 90 };
    { note = (`A, 6); timestamp = 21960; duration = 90 };
    { note = (`F, 6); timestamp = 22050; duration = 90 };
    { note = (`G, 6); timestamp = 22140; duration = 90 };
    { note = (`E, 6); timestamp = 22230; duration = 90 };
    { note = (`D, 5); timestamp = 21600; duration = 795 };
    { note = (`A, 4); timestamp = 22410; duration = 90 };
    { note = (`B, 4); timestamp = 22500; duration = 90 };
    { note = (`C, 5); timestamp = 22590; duration = 90 };
    { note = (`D, 5); timestamp = 22680; duration = 90 };
    { note = (`B, 4); timestamp = 22770; duration = 90 };
    { note = (`C, 5); timestamp = 22860; duration = 90 };
    { note = (`A, 4); timestamp = 22950; duration = 90 };
    { note = (`F, 6); timestamp = 22320; duration = 810 };
    { note = (`G, 6); timestamp = 23130; duration = 90 };
    { note = (`F, 6); timestamp = 23220; duration = 90 };
    { note = (`E, 6); timestamp = 23310; duration = 90 };
    { note = (`D, 6); timestamp = 23400; duration = 90 };
    { note = (`F, 6); timestamp = 23490; duration = 90 };
    { note = (`E, 6); timestamp = 23580; duration = 90 };
    { note = (`G, 6); timestamp = 23670; duration = 90 };
    { note = (`B, 4); timestamp = 23040; duration = 795 };
    { note = (`D, 5); timestamp = 23850; duration = 90 };
    { note = (`C, 5); timestamp = 23940; duration = 90 };
    { note = (`B, 4); timestamp = 24030; duration = 90 };
    { note = (`A, 4); timestamp = 24120; duration = 90 };
    { note = (`C, 5); timestamp = 24210; duration = 90 };
    { note = (`B, 4); timestamp = 24300; duration = 90 };
    { note = (`D, 5); timestamp = 24390; duration = 90 };
    { note = (`F, 6); timestamp = 23760; duration = 795 };
    { note = (`D, 6); timestamp = 24570; duration = 90 };
    { note = (`E, 6); timestamp = 24660; duration = 90 };
    { note = (`F, 6); timestamp = 24750; duration = 90 };
    { note = (`G, 6); timestamp = 24840; duration = 90 };
    { note = (`E, 6); timestamp = 24930; duration = 90 };
    { note = (`F, 6); timestamp = 25020; duration = 90 };
    { note = (`D, 6); timestamp = 25110; duration = 90 };
    { note = (`C, 5); timestamp = 24480; duration = 795 };
    { note = (`G, 4); timestamp = 25290; duration = 90 };
    { note = (`A, 4); timestamp = 25380; duration = 90 };
    { note = (`A_sharp, 4); timestamp = 25470; duration = 90 };
    { note = (`C, 5); timestamp = 25560; duration = 90 };
    { note = (`A, 4); timestamp = 25650; duration = 90 };
    { note = (`A_sharp, 4); timestamp = 25740; duration = 90 };
    { note = (`G, 4); timestamp = 25830; duration = 90 };
    { note = (`E, 6); timestamp = 25200; duration = 795 };
    { note = (`C, 6); timestamp = 26010; duration = 90 };
    { note = (`A, 4); timestamp = 25920; duration = 180 };
    { note = (`D, 6); timestamp = 26100; duration = 90 };
    { note = (`E, 6); timestamp = 26190; duration = 90 };
    { note = (`A_sharp, 4); timestamp = 26100; duration = 180 };
    { note = (`F, 6); timestamp = 26280; duration = 90 };
    { note = (`D, 6); timestamp = 26370; duration = 90 };
    { note = (`A, 4); timestamp = 26280; duration = 180 };
    { note = (`E, 6); timestamp = 26460; duration = 90 };
    { note = (`C, 6); timestamp = 26550; duration = 90 };
    { note = (`G, 4); timestamp = 26460; duration = 180 };
    { note = (`D, 6); timestamp = 26640; duration = 90 };
    { note = (`E, 6); timestamp = 26730; duration = 90 };
    { note = (`F, 4); timestamp = 26640; duration = 180 };
    { note = (`F, 6); timestamp = 26820; duration = 90 };
    { note = (`G, 6); timestamp = 26910; duration = 90 };
    { note = (`D, 5); timestamp = 26820; duration = 180 };
    { note = (`A, 6); timestamp = 27000; duration = 90 };
    { note = (`F, 6); timestamp = 27090; duration = 90 };
    { note = (`C, 5); timestamp = 27000; duration = 180 };
    { note = (`G, 6); timestamp = 27180; duration = 90 };
    { note = (`E, 6); timestamp = 27270; duration = 90 };
    { note = (`A_sharp, 4); timestamp = 27180; duration = 180 };
    { note = (`F, 6); timestamp = 27360; duration = 90 };
    { note = (`G, 6); timestamp = 27450; duration = 90 };
    { note = (`A, 4); timestamp = 27360; duration = 180 };
    { note = (`A, 6); timestamp = 27540; duration = 90 };
    { note = (`B, 6); timestamp = 27630; duration = 90 };
    { note = (`F, 5); timestamp = 27540; duration = 180 };
    { note = (`C, 7); timestamp = 27720; duration = 90 };
    { note = (`A, 6); timestamp = 27810; duration = 90 };
    { note = (`E, 5); timestamp = 27720; duration = 180 };
    { note = (`B, 6); timestamp = 27900; duration = 90 };
    { note = (`G, 6); timestamp = 27990; duration = 90 };
    { note = (`D, 5); timestamp = 27900; duration = 180 };
    { note = (`E, 5); timestamp = 28080; duration = 90 };
    { note = (`C, 7); timestamp = 28080; duration = 180 };
    { note = (`D, 4); timestamp = 28170; duration = 90 };
    { note = (`E, 4); timestamp = 28260; duration = 90 };
    { note = (`G, 6); timestamp = 28260; duration = 180 };
    { note = (`F, 4); timestamp = 28350; duration = 90 };
    { note = (`G, 4); timestamp = 28440; duration = 90 };
    { note = (`E, 6); timestamp = 28440; duration = 180 };
    { note = (`E, 4); timestamp = 28530; duration = 90 };
    { note = (`D, 6); timestamp = 28620; duration = 90 };
    { note = (`F, 4); timestamp = 28620; duration = 90 };
    { note = (`C, 6); timestamp = 28710; duration = 90 };
    { note = (`D, 4); timestamp = 28710; duration = 90 };
    { note = (`C, 6); timestamp = 28800; duration = 90 };
    { note = (`A_sharp, 5); timestamp = 28890; duration = 90 };
    { note = (`E, 4); timestamp = 28800; duration = 180 };
    { note = (`A, 5); timestamp = 28980; duration = 90 };
    { note = (`G, 5); timestamp = 29070; duration = 90 };
    { note = (`C, 4); timestamp = 28980; duration = 180 };
    { note = (`F, 5); timestamp = 29160; duration = 90 };
    { note = (`A, 5); timestamp = 29250; duration = 90 };
    { note = (`D, 4); timestamp = 29160; duration = 180 };
    { note = (`G, 5); timestamp = 29340; duration = 90 };
    { note = (`A_sharp, 5); timestamp = 29430; duration = 90 };
    { note = (`E, 4); timestamp = 29340; duration = 180 };
    { note = (`A, 5); timestamp = 29520; duration = 90 };
    { note = (`F, 4); timestamp = 29520; duration = 90 };
    { note = (`B, 5); timestamp = 29610; duration = 90 };
    { note = (`D, 4); timestamp = 29610; duration = 90 };
    { note = (`C, 6); timestamp = 29700; duration = 90 };
    { note = (`E, 4); timestamp = 29700; duration = 90 };
    { note = (`E, 5); timestamp = 29790; duration = 90 };
    { note = (`F, 4); timestamp = 29790; duration = 90 };
    { note = (`D, 5); timestamp = 29880; duration = 90 };
    { note = (`C, 6); timestamp = 29970; duration = 90 };
    { note = (`G, 4); timestamp = 29880; duration = 180 };
    { note = (`F, 5); timestamp = 30060; duration = 90 };
    { note = (`B, 5); timestamp = 30150; duration = 90 };
    { note = (`G, 3); timestamp = 30060; duration = 180 };
    { note = (`C, 6); timestamp = 30284; duration = 3645 };
    { note = (`G, 5); timestamp = 30273; duration = 3656 };
    { note = (`E, 5); timestamp = 30262; duration = 3667 };
    { note = (`C, 4); timestamp = 30251; duration = 3678 };
    { note = (`C, 3); timestamp = 30240; duration = 3689 };
  ]

let () = play_signal (play_events events |> scale 0.1)
