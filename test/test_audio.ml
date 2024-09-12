(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Tsdl

(* Sample code provided by @psqu in issue #13. *)

let audio_freq    = 44100
let audio_samples = 4096
let time = ref 0

let audio_callback output =
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

let audio_callback =
  ref (Some (Sdl.audio_callback Bigarray.int32 audio_callback))

let audio_setup () =
  let desired_audiospec =
    { Sdl.as_freq = audio_freq;
      as_format = Sdl.Audio.s32;
      Sdl.as_channels = 2;
      Sdl.as_samples = audio_samples;
      Sdl.as_silence = 0;
      Sdl.as_size =
        Int32.of_int (audio_samples * 4 (* bajty na próbkę *) * 2 (* kanały *));
      Sdl.as_callback = !audio_callback; }
  in
  match Sdl.open_audio_device None false desired_audiospec 0 with
  | Error _ -> Sdl.log "Can't open audio device"; exit 1
  | Ok (device_id, _) -> device_id

let video_setup () =
  match Sdl.create_window ~w:640 ~h:480 "SDL Audio Test" Sdl.Window.opengl with
  | Error ( `Msg e ) -> Sdl.log "Create window error: %s" e; exit 1
  | Ok w -> w

let main () = match Sdl.init Sdl.Init.(audio + video) with
| Error ( `Msg e ) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    let window = video_setup () in
    let device_id = audio_setup () in
    Gc.full_major ();
    let () = Sdl.pause_audio_device device_id false in
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
