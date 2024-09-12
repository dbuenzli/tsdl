(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Tsdl

let audio_freq    = 44100
let audio_samples = 4096

let audio_setup () =
  let channels = 2 in
  let iscapture = true in
  let desired_audiospec =
    { Sdl.as_freq = audio_freq;
      Sdl.as_format = Sdl.Audio.s32;
      Sdl.as_channels = channels;
      Sdl.as_samples = audio_samples;
      Sdl.as_silence = 0;
      Sdl.as_size = 0l;
      Sdl.as_callback = None; }
  in
  let dev = match Sdl.open_audio_device None iscapture desired_audiospec 0 with
  | Error _ -> Sdl.log "Can't open audio device"; exit 1
  | Ok (device_id, _) -> device_id
  in
  (* Create a buffer to capture some audio data *)
  let dim = channels * audio_freq * 10 in
  let buffer = Bigarray.(Array1.create int32 c_layout dim) in
  (dev, buffer)

let dequeue_audio device_id buffer =
  Sdl.pause_audio_device device_id false;
  Sdl.delay 1000l;
  let captured = Sdl.dequeue_audio device_id buffer in
  Sdl.log "Dequeued bytes: %d" captured;
  Sdl.pause_audio_device device_id true

let video_setup () =
  match Sdl.create_window ~w:640 ~h:480 "SDL Audio Test" Sdl.Window.opengl with
  | Error ( `Msg e ) -> Sdl.log "Create window error: %s" e; exit 1
  | Ok w -> w

let main () = match Sdl.init Sdl.Init.(audio + video) with
| Error ( `Msg e ) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    let window = video_setup () in
    let (device_id, buffer) = audio_setup () in
    Gc.full_major ();
    let () = dequeue_audio device_id buffer in
    let e = Sdl.Event.create () in
    let rec loop () = match Sdl.wait_event (Some e) with
    | Error ( `Msg err ) -> Sdl.log "Could not wait event: %s" err; 1
    | Ok () ->
        match Sdl.Event.(enum (get e typ)) with
        | `Quit ->
            Sdl.pause_audio_device device_id true;
            Sdl.destroy_window window;
            Sdl.quit();
            0
        | _ -> loop ()
    in
    loop ()

let () = if !Sys.interactive then () else exit (main ())
