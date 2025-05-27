open Ctypes

module Types = Types_generated

module Functions (F : FOREIGN) = struct
  let delay =
    F.(foreign "SDL_Delay" (int32_t @-> returning void))

  let render_present =
    F.(foreign "SDL_RenderPresent" (ptr Types.Renderer.t @-> returning void))

  let wait_event =
    F.(foreign "SDL_WaitEvent" (ptr Types.Event.t @-> returning int))

  let wait_event_timeout =
    F.(foreign "SDL_WaitEventTimeout"
         (ptr Types.Event.t @-> int @-> returning bool))

  let load_wav_rw =
    F.(foreign "SDL_LoadWAV_RW"
         (Types.rw_ops @-> int @-> ptr Types.audio_spec @-> ptr (ptr uint8_t) @->
          ptr uint32_t @-> returning (ptr_opt Types.audio_spec)))
end
