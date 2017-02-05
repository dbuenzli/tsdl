(* test SDL audio and load_wav *)
(* San Vu Ngoc, Feb 2017 *)

(* compile with
ocamlfind ocamlc -thread -package tsdl,result -linkpkg -o read_wav.byte read_wav.ml 
ocamlfind ocamlopt -thread -package tsdl,result -linkpkg -o read_wav read_wav.ml
 *)

(* based on the SDL sources: *)
(* SDL2-2.0.3/test/testmultiaudio.c *)

(* see also: *)
(* https://github.com/dbuenzli/tsdl/issues/13 *)

(* a complete solution: SDL_mixer, see: *)
(* https://github.com/tokenrove/tsdl-mixer *)

open Tsdl
open Bigarray
open Result
  
let go = function
  | Error _ -> failwith ("SDL ERROR: " ^ (Sdl.get_error ()))
  | Ok r -> r
  
let print_spec spec =
  Sdl.(log "as_freq=%d, as_format=%d, as_channels=%d, as_silence=%d as_samples=%d as_size = %ld"
         spec.as_freq
         spec.as_format
         spec.as_channels
         spec.as_silence
         spec.as_samples
         spec.as_size 
  )

let init_audio () =
  go (Sdl.audio_init None);
  begin
    match Sdl.get_current_audio_driver () with
    | None -> failwith "cannot find audio driver"
    | Some s -> Sdl.log "Using audio driver: %s" s
  end;
  if go(Sdl.get_num_audio_drivers ()) < 1
  then failwith "Don't see any specific audio devices!";
  let devname = go(Sdl.get_audio_device_name 0 false) in
  Sdl.log "Using device #%d: ('%s')..." 0 devname;
  devname

(* This callback sends the sound to the output *)
let play_through_once waveleft sound silence =
  let soundpos = ref 0 in
  let soundlen = Array1.dim sound in
  fun output ->
  let chunk_length = Array1.dim output in
  waveleft := soundlen - !soundpos;
  let cpy = min chunk_length !waveleft in
  Sdl.log "soundlen=%u, output dim=%u, soundpos=%u, waveleft=%u"
    soundlen chunk_length !soundpos !waveleft;
  let chunk = Array1.sub sound !soundpos cpy in
  if cpy = chunk_length
  then Array1.blit chunk output
  else begin
      Array1.blit chunk (Array1.sub output 0 cpy);
      Array1.fill (Array1.sub output cpy (chunk_length-cpy)) silence
    end;
  soundpos := !soundpos + cpy

let play_wav devname filename =
  let file = go (Sdl.rw_from_file filename "rb") in
  let dummy_audio_spec = {
      Sdl.as_freq = 44100; (* will be overwritten *)
      as_format   = Sdl.Audio.s16_sys; (* will be overwritten *)
      as_channels = 2; (* will be overwritten *)
      as_silence  = 0; (* will be overwritten *)
      as_callback = None; (* will be overwritten *)
      as_samples  = 2048; (* will be overwritten *)
      as_size     = 8192l; (* will be overwritten *)
      as_ba_kind  = int8_unsigned;
      (* "silence" will be stored in the array, so we use int8 *)
    } in

  let audio_spec, sound = go (Sdl.load_wav_rw file dummy_audio_spec) in
  go (Sdl.rw_close file);
  (* we have to create a new version of the audio_spec to indicate the callback,
     which depends on the Bigarray "sound" that was just created... *)
  let waveleft = ref 1 in
  let audio_spec = {
      audio_spec with
      Sdl.as_callback =
        Some (play_through_once waveleft sound audio_spec.Sdl.as_silence)
    } in
  let allow = Sdl.Audio.allow_any_change in
  let devid, new_spec = go(Sdl.open_audio_device (Some devname) false audio_spec allow) in
  print_spec new_spec;
  Sdl.pause_audio_device devid false;
  devid, waveleft, sound
  
  
let main () =
  go (Sdl.init Sdl.Init.(audio+video));  
  let window = go(Sdl.create_window ~w:640 ~h:480
                    "SDL Audio Test" Sdl.Window.windowed) in 
  let devname = init_audio () in
  let filename = if Array.length Sys.argv > 1
                 then  Sys.argv.(1) else "sample.wav" in
  let devid, waveleft, sound = play_wav devname filename in  
  let e = Sdl.Event.create () in
  let rec loop () =
    if !waveleft > 0 then 
      if Sdl.poll_event (Some e) then
        match Sdl.Event.(enum (get e typ)) with
        | `Quit -> ()
        | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.escape -> ()
        | _ ->  loop ()
      else (Sdl.delay 10l; loop ())
  in
  loop ();
  Sdl.pause_audio_device devid true;
  Sdl.free_wav sound;
  Sdl.close_audio_device devid;
  Sdl.audio_quit ();
  Sdl.destroy_window window;
  Sdl.quit()

let () = main ()

