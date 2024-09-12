(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Tsdl

let ( let* ) = Result.bind

(* Sample code provided by @psqu in issue #13 ported to audio queues. *)

let audio_freq    = 44100
let audio_samples = 4096
let time = ref 0

let create_audio output =
  let open Bigarray in
  for i = 0 to ((Array1.dim output / 2) - 1) do
    let phase = ((float_of_int !time) /.
                 (66100.0 +.
                  1000.0 *. sin (0.0001 *. (float_of_int !time)))) *. 3000.0
    in
    let sample = Int32.of_float ((sin phase) *. 1073741823.0) in
    begin
      output.{ 2 * i     } <- sample;
      output.{ 2 * i + 1 } <- sample;
      time := !time + 1
    end
  done

let audio_setup () =
  let channels = 2 in
  let desired_audiospec =
    { Sdl.as_freq = audio_freq;
      Sdl.as_format = Sdl.Audio.s32;
      Sdl.as_channels = channels;
      Sdl.as_samples = audio_samples;
      Sdl.as_silence = 0;
      Sdl.as_size = 0l; (* open_audio_device calculates this for us *)
      Sdl.as_callback = None; }
  in
  let* dev, _ = Sdl.open_audio_device None false desired_audiospec 0 in
  (* Create a few seconds of audio data *)
  let dim = channels * audio_freq * 10 in
  let buffer = Bigarray.(Array1.create int32 c_layout dim) in
  let () = create_audio buffer in
  Ok (dev, buffer)

let queue_and_start_audio device_id buffer =
  let kind_size = Bigarray.kind_size_in_bytes (Bigarray.Array1.kind buffer) in
  let size = Bigarray.Array1.dim buffer * kind_size in
  Sdl.log "Byte size of buffer to queue: %d" size;
  let* () =  Sdl.queue_audio device_id buffer in
  (* Let's query how much is queued up before we start playing. *)
  Sdl.get_queued_audio_size device_id |> Sdl.log "Queued bytes: %d";
  Sdl.pause_audio_device device_id false;
  Ok ()

let queue_and_clear_audio device_id buffer =
  let* () = Sdl.queue_audio device_id buffer in
  Sdl.get_queued_audio_size device_id |> Sdl.log "Currently queued bytes: %d";
  Sdl.clear_queued_audio device_id;
  Sdl.get_queued_audio_size device_id |> Sdl.log "Queued after clearing: %d";
  Ok ()

let main () = match Sdl.init Sdl.Init.(audio) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL Audio" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; 1
    | Ok window ->
        let device_id =
          let* (device_id, buffer) = audio_setup () in
          let* () = queue_and_clear_audio device_id buffer in
          let* () = queue_and_start_audio device_id buffer in
          Ok device_id
        in
        match device_id with
        | Error (`Msg e) -> Sdl.log "Audio setup error: %s" e; 1
        | Ok device_id ->
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
