
(* Sample code provided by @psqu in issue #13. *)

open Tsdl
open Result

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
  let dev = match Sdl.open_audio_device None false desired_audiospec 0 with
  | Error _ -> Sdl.log "Can't open audio device"; exit 1
  | Ok (device_id, _) -> device_id
  in
  (* Create a few seconds of audio data *)
  let dim = channels * audio_freq * 10 in
  let buffer = Bigarray.(Array1.create int32 c_layout dim) in
  let () = create_audio buffer in
  (dev, buffer)

let queue_and_start_audio device_id buffer =
  Sdl.log "Size of buffer to queue: %d" (Bigarray.Array1.dim buffer);
  match Sdl.queue_audio device_id buffer with
  | Ok () ->
      (* Let's query how much is queued up before we start playing. *)
      Sdl.get_queued_audio_size device_id |> Sdl.log "Queued bytes: %ld";
      Sdl.pause_audio_device device_id false
  | Error _ -> Sdl.log "Can't queue audio"; exit 1

let queue_and_clear_audio device_id buffer =
  match Sdl.queue_audio device_id buffer with
  | Ok () ->
      Sdl.get_queued_audio_size device_id |> Sdl.log "Currently queued bytes: %ld";
      Sdl.clear_queued_audio device_id;
      Sdl.get_queued_audio_size device_id |> Sdl.log "Queued after clearing: %ld";
      ()
  | Error _ -> Sdl.log "Can't queue audio"; exit 1

let video_setup () =
  match Sdl.create_window ~w:640 ~h:480 "SDL Audio Test" Sdl.Window.opengl with
  | Error ( `Msg e ) -> Sdl.log "Create window error: %s" e; exit 1
  | Ok w -> w

let main () = match Sdl.init Sdl.Init.(audio + video) with
| Error ( `Msg e ) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    let window = video_setup () in
    let (device_id, buffer) = audio_setup () in
    Gc.full_major ();
    let () = queue_and_clear_audio device_id buffer in
    let () = queue_and_start_audio device_id buffer in
    let e = Sdl.Event.create () in
    let rec loop () = match Sdl.wait_event (Some e) with
    | Error ( `Msg err ) -> Sdl.log "Could not wait event: %s" err; ()
    | Ok () ->
        match Sdl.Event.(enum (get e typ)) with
        | `Quit ->
            Sdl.pause_audio_device device_id true;
            Sdl.destroy_window window;
            Sdl.quit()
        | _ -> loop ()
    in
    loop ()

let () = main ()
