(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** SDL thin bindings.

    Consult the {{!conventions}binding conventions}, the
    {{!Sdl.coverage}binding coverage} and {{!examples}examples} of
    use.  Given the thinness of the binding most functions are
    documented by linking directly to SDL's own documentation.

    Open the module to use it, this defines only the module [Sdl] in
    your scope.

    {b Note.} The module initialization code calls
    {{:http://wiki.libsdl.org/SDL_SetMainReady}SDL_SetMainReady}.

    {b References}
    {ul
    {- {{:http://wiki.libsdl.org/APIByCategory}SDL API}}}

    {e Release %%VERSION%% — SDL %%SDLVERSION%% —
    {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:sdl SDL} *)

(** SDL bindings.

    {ul
    {- {!Sdl.basics}
    {ul
    {- {{!section:Sdl.init}Initialization and shutdown}}
    {- {{!Sdl.hints}Hints}}
    {- {{!Sdl.errors}Errors}}
    {- {{!Sdl.log}Log}}
    {- {{!Sdl.version}Version}}
    }}
    {- {!Sdl.fileabstraction}
    {ul
    {- {{!Sdl.io}IO abstraction}}
    {- {{!Sdl.fspaths}Filesystem paths}}
    }}
    {- {!Sdl.video}
    {ul
    {- {{!Sdl.colors}Colors}}
    {- {{!Sdl.points}Points}}
    {- {{!Sdl.rectangles}Rectangles}}
    {- {{!Sdl.palettes}Pallettes}}
    {- {{!Sdl.pixel_formats}Pixel formats}}
    {- {{!Sdl.surfaces}Surfaces}}
    {- {{!Sdl.renderers}Renderers}}
    {- {{!Sdl.textures}Textures}}
    {- {{!Sdl.videodrivers}Video drivers}}
    {- {{!Sdl.displays}Displays}}
    {- {{!Sdl.windows}Windows}}
    {- {{!Sdl.opengl}OpenGL contexts}}
    {- {{!Sdl.screensaver}Screen saver}}
    {- {{!Sdl.messageboxes}Message boxes}}
    }}
    {- {!Sdl.input}
    {ul
    {- {{!Sdl.keyboard}Keyboard}}
    {- {{!Sdl.mouse}Mouse}}
    {- {{!Sdl.touch}Touch and gestures}}
    {- {{!Sdl.joystick}Joystick}}
    {- {{!Sdl.gamecontroller}Game controller}}
    {- {{!Sdl.events}Events}}
    }}
    {- {{!Sdl.forcefeedback}Force feedback}}
    {- {{!Sdl.audio}Audio}
    {ul
    {- {{!Sdl.audiodrivers}Audio drivers}}
    {- {{!Sdl.audiodevices}Audio devices}}
    }}
    {- {{!Sdl.timer}Timer}}
    {- {!Sdl.platform}}
    {- {{!Sdl.power}Power}}
    {- {!Sdl.coverage}}}
*)
module Sdl : sig

(** {1:types Integer types, bigarrays and results} *)

type uint8 = int
type int16 = int
type uint16 = int
type uint32 = int32
type uint64 = int64
type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
(** The type for bigarrays.*)

type 'a result = ('a, [ `Msg of string ]) Result.result
(** The type for function results. In the error case,
    the string is what {!Sdl.get_error} returned. *)

(** {1:basics Basics} *)

(** {2:init {{:http://wiki.libsdl.org/CategoryInit}
             Initialization and shutdown}} *)

module Init : sig
  type t

  val ( + ) : t -> t -> t
  (** [f + f'] combines flags [f] and [f']. *)

  val test : t -> t -> bool
  (** [test flags mask] is [true] if any of the flags in [mask] is
      set in [flags]. *)

  val eq : t -> t -> bool
  (** [eq f f'] is [true] if the flags are equal. *)

  val timer : t
  val audio : t
  val video : t
  val joystick : t
  val haptic : t
  val gamecontroller : t
  val events : t
  val everything : t
  val noparachute : t
end
(** Subsystem flags. *)

val init : Init.t -> unit result
(** {{:http://wiki.libsdl.org/SDL_Init}SDL_Init} *)

val init_sub_system : Init.t -> unit result
(** {{:http://wiki.libsdl.org/SDL_InitSubSystem}SDL_InitSubSystem} *)

val quit : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_Quit}SDL_Quit} *)

val quit_sub_system : Init.t -> unit
(** {{:http://wiki.libsdl.org/SDL_QuitSubSystem}SDL_QuitSubSystem} *)

val was_init : Init.t option -> Init.t
(** {{:http://wiki.libsdl.org/SDL_WasInit}SDL_WasInit} *)

(** {2:hints {{:http://wiki.libsdl.org/CategoryHints}Hints}} *)

module Hint : sig

  (** {1:hint Hints} *)

  type t = string

  val framebuffer_acceleration : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_FRAMEBUFFER_ACCELERATION}
       SDL_HINT_FRAMEBUFFER_ACCELERATION} *)

  val idle_timer_disabled : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_IDLE_TIMER_DISABLED}
       SDL_HINT_IDLE_TIMER_DISABLED} *)

  val orientations : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_ORIENTATIONS}
      SDL_HINT_ORIENTATIONS} *)

  val render_driver : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_RENDER_DRIVER}
      SDL_HINT_RENDER_DRIVER} *)

  val render_opengl_shaders : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_RENDER_OPENGL_SHADERS}
      SDL_HINT_RENDER_OPENGL_SHADERS} *)

  val render_scale_quality : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_RENDER_SCALE_QUALITY}
      SDL_HINT_RENDER_SCALE_QUALITY} *)

  val render_vsync : t
  (** {{:http://wiki.libsdl.org/SDL_HINT_RENDER_VSYNC}
      SDL_HINT_RENDER_VSYNC} *)

  (** {1:priority Priority} *)

  type priority
  (** {{:http://wiki.libsdl.org/SDL_HintPriority}SDL_HintPriority} *)

  val default : priority
  val normal : priority
  val override : priority
end

val clear_hints : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_ClearHints}SDL_ClearHints} *)

val get_hint : Hint.t -> string option
(** {{:http://wiki.libsdl.org/SDL_GetHint}SDL_GetHint} *)

val set_hint : Hint.t -> string -> bool
(** {{:http://wiki.libsdl.org/SDL_SetHint}SDL_SetHint} *)

val set_hint_with_priority : Hint.t -> string -> Hint.priority -> bool
(** {{:http://wiki.libsdl.org/SDL_SetHintWithPriority}
    SDL_SetHintWithPriority} *)

(** {2:errors {{:http://wiki.libsdl.org/CategoryError}Errors}} *)

val clear_error : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_ClearError}SDL_ClearError} *)

val get_error : unit -> string
(** {{:http://wiki.libsdl.org/SDL_GetError}SDL_GetError} *)

val set_error : ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_SetError}SDL_SetError} *)

(** {2:log {{:http://wiki.libsdl.org/CategoryLog}Log}} *)

module Log : sig

  (** {1:category Category} *)

  type category = int
  (** {{:http://wiki.libsdl.org/SDL_LOG_CATEGORY}SDL_LOG_CATEGORY} *)

  val category_application : category
  val category_error : category
  val category_system : category
  val category_audio : category
  val category_video : category
  val category_render : category
  val category_input : category

  (** {1:priority Priority} *)

  type priority
  val priority_compare : priority -> priority -> int
  val priority_verbose : priority
  val priority_debug : priority
  val priority_info : priority
  val priority_warn : priority
  val priority_error : priority
  val priority_critical : priority
end

val log : ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_Log}SDL_Log} *)

val log_critical : Log.category -> ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogCritical}SDL_LogCritical} *)

val log_debug : Log.category -> ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogDebug}SDL_LogDebug} *)

val log_error : Log.category -> ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogError}SDL_LogError} *)

val log_get_priority : Log.category -> Log.priority
(** {{:http://wiki.libsdl.org/SDL_LogGetPriority}SDL_LogGetPriority} *)

val log_info : Log.category -> ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogInfo}SDL_LogInfo} *)

val log_message : Log.category -> Log.priority ->
  ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogMessage}SDL_LogMessage} *)

val log_reset_priorities : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_LogResetPriorities}SDL_LogResetPriorities} *)

val log_set_all_priority : Log.priority -> unit
(** {{:http://wiki.libsdl.org/SDL_LogSetAllPriority}SDL_LogSetAllPriority} *)

val log_set_priority : Log.category -> Log.priority -> unit
(** {{:http://wiki.libsdl.org/SDL_LogSetPriority}SDL_LogSetPriority} *)

val log_verbose : Log.category -> ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogVerbose}SDL_LogVerbose} *)

val log_warn : Log.category -> ('b, Format.formatter, unit) format -> 'b
(** {{:http://wiki.libsdl.org/SDL_LogWarn}SDL_LogWarn} *)

(** {2:version {{:http://wiki.libsdl.org/CategoryVersion}Version}} *)

val get_version : unit -> (int * int * int)
(** {{:http://wiki.libsdl.org/SDL_GetVersion}SDL_GetVersion} *)

val get_revision : unit -> string
(** {{:http://wiki.libsdl.org/SDL_GetRevision}SDL_GetRevision} *)

val get_revision_number : unit -> int
(** {{:http://wiki.libsdl.org/SDL_GetRevisionNumber}SDL_GetRevisionNumber} *)

(** {1:fileabstraction Files and IO abstraction} *)

(** {2:io {{:https://wiki.libsdl.org/CategoryIO}IO abstraction}} *)

type rw_ops
(** {{:https://wiki.libsdl.org/SDL_RWops}SDL_RWops} *)

val rw_from_file : string -> string -> rw_ops result
(** {{:https://wiki.libsdl.org/SDL_RWFromFile}SDL_RWFromFile} *)

val rw_close : rw_ops -> unit result
(** {{:https://wiki.libsdl.org/SDL_RWclose}SDL_RWclose} *)

(**/**)
val unsafe_rw_ops_of_ptr : nativeint -> rw_ops
val unsafe_ptr_of_rw_ops : rw_ops -> nativeint
(**/**)

(** {1:fspaths {{:https://wiki.libsdl.org/CategoryFilesystem}Filesystem
    Paths}} *)

val get_base_path : unit -> string result
(** {{:https://wiki.libsdl.org/SDL_GetBasePath}SDL_GetBasePath} *)

val get_pref_path : org:string -> app:string -> string result
(** {{:https://wiki.libsdl.org/SDL_GetPrefPath}SDL_GetPrefPath} *)

(** {1:video Video} *)

type window
(** SDL_Window *)

(**/**)
val unsafe_window_of_ptr : nativeint -> window
val unsafe_ptr_of_window : window -> nativeint
(**/**)

(** {2:colors Colors} *)

type color
(** {{:http://wiki.libsdl.org/SDL_Rect}SDL_Color} *)

module Color : sig
  val create : r:uint8 -> g:uint8 -> b:uint8 -> a:uint8 -> color
  val r : color -> uint8
  val g : color -> uint8
  val b : color -> uint8
  val a : color -> uint8
  val set_r : color -> uint8 -> unit
  val set_g : color -> uint8 -> unit
  val set_b : color -> uint8 -> unit
  val set_a : color -> uint8 -> unit
end

(** {2:points Points} *)

type point
(** {{:http://wiki.libsdl.org/SDL_Point}SDL_Point} *)

module Point : sig
  val create : x:int -> y:int -> point
  val x : point -> int
  val y : point -> int
  val set_x : point -> int -> unit
  val set_y : point -> int -> unit
end

(** {2:rectangles
    {{:http://wiki.libsdl.org/CategoryRect}Rectangles}} *)

type rect
(** {{:http://wiki.libsdl.org/SDL_Rect}SDL_Rect} *)

module Rect : sig
  val create : x:int -> y:int -> w:int -> h:int -> rect
  val x : rect -> int
  val y : rect -> int
  val w : rect -> int
  val h : rect -> int
  val set_x : rect -> int -> unit
  val set_y : rect -> int -> unit
  val set_w : rect -> int -> unit
  val set_h : rect -> int -> unit
end

val enclose_points : ?clip:rect -> point list -> rect option
(** {{:http://wiki.libsdl.org/SDL_EnclosePoints}SDL_EnclosePoints}.
    Returns [None] if all the points were outside
    the clipping rectangle (if provided). *)

val enclose_points_ba : ?clip:rect -> (int32, Bigarray.int32_elt) bigarray ->
  rect option
(** See {!enclose_points}. Each consecutive pair in the array defines a
    point.
    @raise Invalid_argument if the length of the array is not
    a multiple of 2. *)

val has_intersection : rect -> rect -> bool
(** {{:http://wiki.libsdl.org/SDL_HasIntersection}SDL_HasIntersection} *)

val intersect_rect : rect -> rect -> rect option
(** {{:http://wiki.libsdl.org/SDL_IntersectRect}SDL_IntersectRect} *)

val intersect_rect_and_line : rect -> int -> int -> int -> int ->
  ((int * int) * (int * int)) option
(** {{:http://wiki.libsdl.org/SDL_IntersectRectAndLine}
    SDL_IntersectRectAndLine}. Returns the clipped segment if it
    intersects. *)

val rect_empty : rect -> bool
(** {{:http://wiki.libsdl.org/SDL_RectEmpty}SDL_RectEmpty} *)

val rect_equals : rect -> rect -> bool
(** {{:http://wiki.libsdl.org/SDL_RectEquals}SDL_RectEquals} *)

val union_rect : rect -> rect -> rect
(** {{:http://wiki.libsdl.org/SDL_UnionRect}SDL_UnionRect} *)

(** {2:palettes {{:http://wiki.libsdl.org/CategoryPixels}Palettes}} *)

type palette
(** {{:https://wiki.libsdl.org/SDL_Palette}SDL_Palette} *)

val alloc_palette : int -> palette result
(** {{:http://wiki.libsdl.org/SDL_AllocPalette}SDL_AllocPalette} *)

val free_palette : palette -> unit
(** {{:http://wiki.libsdl.org/SDL_FreePalette}SDL_FreePalette} *)

val get_palette_ncolors : palette -> int
(** [get_palette_ncolors p] is the field [ncolors] of [p]. *)

val get_palette_colors : palette -> color list
(** [get_palette_colors p] is a copy of the contents of the field [colors]
    of [s]. *)

val get_palette_colors_ba : palette ->
  (int, Bigarray.int8_unsigned_elt) bigarray
(** [get_palette_colors_ba p] is a copy of the contents of the field [colors]
    of [p]. *)

val set_palette_colors : palette -> color list -> fst:int ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_SetPaletteColors}SDL_SetPaletteColors} *)

val set_palette_colors_ba : palette ->
  (int, Bigarray.int8_unsigned_elt) bigarray -> fst:int -> unit result
(** See {!set_palette_colors}. Each consecutive quadruplet defines a
    color. The data is copied.
    @raise Invalid_argument if the length of the array is not
    a multiple of 4. *)

(**/**)
val unsafe_palette_of_ptr : nativeint -> palette
val unsafe_ptr_of_palette : palette -> nativeint
(**/**)

(** {2:pixel_formats {{:http://wiki.libsdl.org/CategoryPixels}Pixels
    formats}} *)

type gamma_ramp = (int, Bigarray.int16_unsigned_elt) bigarray
(** The type for gamma ramps, 256 [uint16] values. *)

val calculate_gamma_ramp : float -> gamma_ramp
(** {{:http://wiki.libsdl.org/SDL_CalculateGammaRamp}SDL_CalculateGammaRamp} *)

module Blend : sig
  type mode
  (** {{:https://wiki.libsdl.org/SDL_BlendMode}SDL_BlendMode} *)

  val mode_none : mode
  val mode_blend : mode
  val mode_add : mode
  val mode_mod : mode
end

module Pixel : sig
  type format_enum
  (** {{:https://wiki.libsdl.org/SDL_PixelFormatEnum}SDL_PixelFormatEnum}. *)

  val eq : format_enum -> format_enum -> bool
  val to_uint32 : format_enum -> uint32
  val format_unknown : format_enum
  val format_index1lsb : format_enum
  val format_index1msb : format_enum
  val format_index4lsb : format_enum
  val format_index4msb : format_enum
  val format_index8 : format_enum
  val format_rgb332 : format_enum
  val format_rgb444 : format_enum
  val format_rgb555 : format_enum
  val format_bgr555 : format_enum
  val format_argb4444 : format_enum
  val format_rgba4444 : format_enum
  val format_abgr4444 : format_enum
  val format_bgra4444 : format_enum
  val format_argb1555 : format_enum
  val format_rgba5551 : format_enum
  val format_abgr1555 : format_enum
  val format_bgra5551 : format_enum
  val format_rgb565 : format_enum
  val format_bgr565 : format_enum
  val format_rgb24 : format_enum
  val format_bgr24 : format_enum
  val format_rgb888 : format_enum
  val format_rgbx8888 : format_enum
  val format_bgr888 : format_enum
  val format_bgrx8888 : format_enum
  val format_argb8888 : format_enum
  val format_rgba8888 : format_enum
  val format_abgr8888 : format_enum
  val format_bgra8888 : format_enum
  val format_argb2101010 : format_enum
  val format_yv12 : format_enum
  val format_iyuv : format_enum
  val format_yuy2 : format_enum
  val format_uyvy : format_enum
  val format_yvyu : format_enum
end

type pixel_format
(** {{:https://wiki.libsdl.org/SDL_PixelFormat}SDL_PixelFormat} *)

val alloc_format : Pixel.format_enum -> pixel_format result
(** {{:http://wiki.libsdl.org/SDL_AllocFormat}SDL_AllocFormat} *)

val free_format : pixel_format -> unit
(** {{:http://wiki.libsdl.org/SDL_FreeFormat}SDL_FreeFormat} *)

val get_pixel_format_name : Pixel.format_enum -> string
(** {{:http://wiki.libsdl.org/SDL_GetPixelFormatName}SDL_GetPixelFormatName} *)

val get_pixel_format_format : pixel_format -> Pixel.format_enum
(** [get_pixel_format_format pf] is the field [format] of [pf]. *)

val get_pixel_format_bits_pp : pixel_format -> int
(** [get_pixel_format_bits_pp pf] is the field [BitsPerPixel] of [pf]. *)

val get_pixel_format_bytes_pp : pixel_format -> int
(** [get_pixel_format_bytes_pp pf] is the field [BytesPerPixel] of [pf]. *)

val get_rgb : pixel_format -> uint32 -> (uint8 * uint8 * uint8)
(** {{:http://wiki.libsdl.org/SDL_GetRGB}SDL_GetRGB} *)

val get_rgba : pixel_format -> uint32 -> (uint8 * uint8 * uint8 * uint8)
(** {{:http://wiki.libsdl.org/SDL_GetRGBA}SDL_GetRGBA} *)

val map_rgb : pixel_format -> uint8 -> uint8 -> uint8 -> uint32
(** {{:http://wiki.libsdl.org/SDL_MapRGB}SDL_MapRGB} *)

val map_rgba : pixel_format -> uint8 -> uint8 -> uint8 -> uint8 -> uint32
(** {{:http://wiki.libsdl.org/SDL_MapRGBA}SDL_MapRGBA} *)

val masks_to_pixel_format_enum :
  int -> uint32 -> uint32 -> uint32 -> uint32 -> Pixel.format_enum
(** {{:http://wiki.libsdl.org/SDL_MasksToPixelFormatEnum}
    SDL_MasksToPixelFormatEnum} *)

val pixel_format_enum_to_masks :
  Pixel.format_enum -> (int * uint32 * uint32 * uint32 * uint32) result
(** {{:http://wiki.libsdl.org/SDL_PixelFormatEnumToMasks}
    SDL_PixelFormatEnumToMasks} *)

val set_pixel_format_palette : pixel_format -> palette -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetPixelFormatPalette}
    SDL_SetPixelFormatPalette}.

    {b Note} If you allocated the palette with {!alloc_palette} you
    can {!free_palette} after. *)

(**/**)
val unsafe_pixel_format_of_ptr : nativeint -> pixel_format
val unsafe_ptr_of_pixel_format : pixel_format -> nativeint
(**/**)

(** {2:surfaces
    {{:http://wiki.libsdl.org/CategorySurface}Surface}} *)

type surface
(** {{:https://wiki.libsdl.org/SDL_Surface}SDL_Surface} *)

val blit_scaled : src:surface -> rect option -> dst:surface -> rect option ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_BlitScaled}SDL_BlitScaled} *)

val blit_surface : src:surface -> rect option -> dst:surface -> rect option ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_BlitSurface}SDL_BlitSurface} *)

val convert_pixels : w:int -> h:int -> src:Pixel.format_enum ->
  ('a, 'b) bigarray -> int -> dst:Pixel.format_enum ->
  ('c, 'd) bigarray -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_ConvertPixels}SDL_ConvertPixels}

    {b Note} Pitches are given in bigarray elements {b not} in bytes.

    {b Warning.} Unsafe, make sure your parameters don't result
    in invalid access to memory. *)

val convert_surface : surface -> pixel_format -> surface result
(** {{:http://wiki.libsdl.org/SDL_ConvertSurface}SDL_ConvertSurface} *)

val convert_surface_format : surface -> Pixel.format_enum -> surface result
(** {{:http://wiki.libsdl.org/SDL_ConvertSurfaceFormat}
    SDL_ConvertSurfaceFormat} *)

val create_rgb_surface : w:int -> h:int -> depth:int -> uint32 -> uint32 ->
  uint32 -> uint32 -> surface result
(** {{:http://wiki.libsdl.org/SDL_CreateRGBSurface}SDL_CreateRGBSurface} *)

val create_rgb_surface_from : ('a, 'b) bigarray -> w:int -> h:int ->
  depth:int -> pitch:int -> uint32 -> uint32 -> uint32 -> uint32 ->
  surface result
(** {{:http://wiki.libsdl.org/SDL_CreateRGBSurfaceFrom}
    SDL_CreateRGBSurfaceFrom}

    {b Note} The pitch is given in bigarray elements {b not} in
    bytes.

    {b Warning} Unsafe, make sure your parameters don't result
    in invalid access to memory. The bigarray data is not copied,
    it must remain valid until {!free_surface} is called on the
    surface. *)

val fill_rect : surface -> rect option -> uint32 -> unit result
(** {{:http://wiki.libsdl.org/SDL_FillRect}SDL_FillRect} *)

val fill_rects : surface -> rect list -> uint32 -> unit result
(** {{:http://wiki.libsdl.org/SDL_FillRects}SDL_FillRects} *)

val fill_rects_ba : surface -> (int32, Bigarray.int32_elt) bigarray ->
  uint32 -> unit result
(** See {!fill_rects}. Each consecutive quadruplet defines a
    rectangle.
    @raise Invalid_argument if the length of the array is not
    a multiple of 4. *)

val free_surface : surface -> unit
(** {{:http://wiki.libsdl.org/SDL_FreeSurface}SDL_FreeSurface} *)

val get_clip_rect : surface -> rect
(** {{:http://wiki.libsdl.org/SDL_GetClipRect}SDL_GetClipRect} *)

val get_color_key : surface -> uint32 result
(** {{:http://wiki.libsdl.org/SDL_GetColorKey}SDL_GetColorKey} *)

val get_surface_alpha_mod : surface -> uint8 result
(** {{:http://wiki.libsdl.org/SDL_GetSurfaceAlphaMod}SDL_GetSurfaceAlphaMod} *)

val get_surface_blend_mode : surface -> Blend.mode result
(** {{:http://wiki.libsdl.org/SDL_GetSurfaceBlendMode}
    SDL_GetSurfaceBlendMode} *)

val get_surface_color_mod : surface -> (int * int * int) result
(** {{:http://wiki.libsdl.org/SDL_GetSurfaceColorMod}
    SDL_GetSurfaceColorMod} *)

val get_surface_format_enum : surface -> Pixel.format_enum
(** [get_surface_format_neum s] is the pixel format enum of the
    field [format] of [s]. *)

val get_surface_pitch : surface -> int
(** [get_surface_pitch s] is the field [pitch] of [s]. *)

val get_surface_pixels : surface -> ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
(** [get_surface_pixels s kind] is the field [pixels] of [s] viewed as
    a [kind] bigarray. Note that you must lock the surface before
    accessing this.

    {b Warning.} The bigarray memory becomes invalid
    once the surface is unlocked or freed.

    @raise Invalid_argument If [kind] can't align with the surface pitch. *)

val get_surface_size : surface -> int * int
(** [get_surface_size s] is the fields [w] and [h] of [s]. *)

val load_bmp : string -> surface result
(** {{:http://wiki.libsdl.org/SDL_LoadBMP}SDL_LoadBMP} *)

val load_bmp_rw : rw_ops -> close:bool -> surface result
(** {{:http://wiki.libsdl.org/SDL_LoadBMP_RW}SDL_LoadBMP_RW} *)

val lock_surface : surface -> unit result
(** {{:http://wiki.libsdl.org/SDL_LockSurface}SDL_LockSurface} *)

val lower_blit : src:surface -> rect -> dst:surface -> rect ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_LowerBlit}SDL_LowerBlit} *)

val lower_blit_scaled : src:surface -> rect -> dst:surface -> rect ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_LowerBlitScaled}SDL_LowerBlitScaled} *)

val save_bmp : surface -> string -> unit result
(** {{:http://wiki.libsdl.org/SDL_SaveBMP}SDL_SaveBMP} *)

val save_bmp_rw : surface -> rw_ops -> close:bool -> unit result
(** {{:http://wiki.libsdl.org/SDL_SaveBMP_RW}SDL_SaveBMP_RW} *)

val set_clip_rect : surface -> rect -> bool
(** {{:http://wiki.libsdl.org/SDL_SetClipRect}SDL_SetClipRect} *)

val set_color_key : surface -> bool -> uint32 -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetColorKey}SDL_SetColorKey} *)

val set_surface_alpha_mod : surface -> uint8 -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetSurfaceAlphaMod}SDL_SetSurfaceAlphaMod} *)

val set_surface_blend_mode : surface -> Blend.mode -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetSurfaceBlendMode}
    SDL_SetSurfaceBlendMode} *)

val set_surface_color_mod : surface -> uint8 -> uint8 -> uint8 -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetSurfaceColorMod}SDL_SetSurfaceColorMod} *)

val set_surface_palette : surface -> palette -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetSurfacePalette}SDL_SetSurfacePalette}

    {b Note} If you allocated the palette with {!alloc_palette} you
    can {!free_palette} after. *)

val set_surface_rle : surface -> bool -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetSurfaceRLE}SDL_SetSurfaceRLE} *)

val unlock_surface : surface -> unit
(** {{:http://wiki.libsdl.org/SDL_UnlockSurface}SDL_UnlockSurface} *)

(**/**)
val unsafe_surface_of_ptr : nativeint -> surface
val unsafe_ptr_of_surface : surface -> nativeint
(**/**)

(** {2:renderers {{:http://wiki.libsdl.org/CategoryRender}Renderers}} *)

type flip
(** {{:https://wiki.libsdl.org/SDL_RendererFlip}SDL_RendererFlip} *)

module Flip : sig
  val ( + ) : flip  -> flip -> flip
  (** [f + f'] combines flips [f] and [f']. *)

  val none : flip
  val horizontal : flip
  val vertical : flip
end

type texture
(** SDL_Texture *)

(**/**)
val unsafe_texture_of_ptr : nativeint -> texture
val unsafe_ptr_of_texture : texture -> nativeint
(**/**)

type renderer

(**/**)
val unsafe_renderer_of_ptr : nativeint -> renderer
val unsafe_ptr_of_renderer : renderer -> nativeint
(**/**)

(** SDL_Renderer *)

module Renderer : sig
  type flags
  (** {{:https://wiki.libsdl.org/SDL_RendererFlags}SDL_RendererFlags} *)

  val ( + ) : flags -> flags -> flags
  (** [f + f'] combines flags [f] and [f']. *)

  val test : flags -> flags -> bool
  (** [test flags mask] is [true] if any of the flags in [mask] is
        set in [flags]. *)

  val eq : flags -> flags -> bool
  (** [eq f f'] is [true] if the flags are equal. *)

  val software : flags
  val accelerated : flags
  val presentvsync : flags
  val targettexture : flags
end

type renderer_info =
  { ri_name : string;
    ri_flags : Renderer.flags;
    ri_texture_formats : Pixel.format_enum list;
    ri_max_texture_width : int;
    ri_max_texture_height : int; }
(** {{:https://wiki.libsdl.org/SDL_RendererInfo}SDL_RendererInfo} *)

val create_renderer : ?index:int -> ?flags:Renderer.flags-> window ->
  renderer result
(** {{:http://wiki.libsdl.org/SDL_CreateRenderer}SDL_CreateRenderer} *)

val create_software_renderer : surface -> renderer result
(** {{:http://wiki.libsdl.org/SDL_CreateSoftwareRenderer}
    SDL_CreateSoftwareRenderer} *)

val destroy_renderer : renderer -> unit
(** {{:http://wiki.libsdl.org/SDL_DestroyRenderer}SDL_DestroyRenderer} *)

val get_num_render_drivers : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_GetNumRenderDrivers}
    SDL_GetNumRenderDrivers} *)

val get_render_draw_blend_mode : renderer -> Blend.mode result
(** {{:http://wiki.libsdl.org/SDL_GetRenderDrawBlendMode}
    SDL_GetRenderDrawBlendMode} *)

val get_render_draw_color : renderer -> (uint8 * uint8 * uint8 * uint8) result
(** {{:http://wiki.libsdl.org/SDL_GetRenderDrawColor}
    SDL_GetRenderDrawColor} *)

val get_render_driver_info : int -> renderer_info result
(** {{:http://wiki.libsdl.org/SDL_GetRenderDriverInfo}
    SDL_GetRenderDriverInfo} *)

val get_render_target : renderer -> texture option
(** {{:http://wiki.libsdl.org/SDL_GetRenderTarget}SDL_GetRenderTarget} *)

val get_renderer : window -> renderer result
(** {{:http://wiki.libsdl.org/SDL_GetRenderer}SDL_GetRenderer} *)

val get_renderer_info : renderer -> renderer_info result
(** {{:http://wiki.libsdl.org/SDL_GetRendererInfo}SDL_GetRendererInfo} *)

val get_renderer_output_size : renderer -> (int * int) result
(** {{:http://wiki.libsdl.org/SDL_GetRendererOutputSize}
    SDL_GetRendererOutputSize} *)

val render_clear : renderer -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderClear}SDL_RenderClear} *)

val render_copy : ?src:rect -> ?dst:rect -> renderer -> texture ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_RenderCopy}SDL_RenderCopy} *)

val render_copy_ex : ?src:rect -> ?dst:rect ->renderer -> texture ->
  float -> point option -> flip -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderCopyEx}SDL_RenderCopyEx} *)

val render_draw_line : renderer -> int -> int -> int -> int ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawLine}SDL_RenderDrawLine} *)

val render_draw_lines : renderer -> point list -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawLines}SDL_RenderDrawLines} *)

val render_draw_lines_ba : renderer -> (int32, Bigarray.int32_elt) bigarray ->
  unit result
(** See {!render_draw_lines}. Each consecutive pair in the array
    defines a point.

    @raise Invalid_argument if the length of the array is not a
    multiple of 2. *)

val render_draw_point : renderer -> int -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawPoint}SDL_RenderDrawPoint} *)

val render_draw_points : renderer -> point list -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawPoints}SDL_RenderDrawPoints} *)

val render_draw_points_ba : renderer -> (int32, Bigarray.int32_elt) bigarray ->
  unit result
(** See {!render_draw_points}. Each consecutive pair in the array
    defines a point.

    @raise Invalid_argument if the length of the array is not a
    multiple of 2. *)

val render_draw_rect : renderer -> rect option -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawRect}SDL_RenderDrawRect} *)

val render_draw_rects : renderer -> rect list -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawRects}SDL_RenderDrawRects} *)

val render_draw_rects_ba : renderer -> (int32, Bigarray.int32_elt) bigarray ->
  unit result
(** See {!render_draw_rects}. Each consecutive quadruple in the array
    defines a rectangle.

    @raise Invalid_argument if the length of the array is not a
    multiple of 4. *)

val render_fill_rect : renderer -> rect option -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderFillRect}SDL_RenderFillRect} *)

val render_fill_rects : renderer -> rect list -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderDrawRects}SDL_RenderDrawRects} *)

val render_fill_rects_ba : renderer -> (int32, Bigarray.int32_elt) bigarray ->
  unit result
(** See {!render_draw_rects}. Each consecutive quadruple in the array
    defines a rectangle.

    @raise Invalid_argument if the length of the array is not a
    multiple of 4. *)

val render_get_clip_rect : renderer -> rect
(** {{:http://wiki.libsdl.org/SDL_RenderGetClipRect}SDL_RenderGetClipRect} *)

val render_get_logical_size : renderer -> int * int
(** {{:http://wiki.libsdl.org/SDL_RenderGetLogicalSize}
    SDL_RenderGetLogicalSize} *)

val render_get_scale : renderer -> float * float
(** {{:http://wiki.libsdl.org/SDL_RenderGetScale}SDL_RenderGetScale} *)

val render_get_viewport : renderer -> rect
(** {{:http://wiki.libsdl.org/SDL_RenderGetViewport}SDL_RenderGetViewport} *)

val render_present : renderer -> unit
(** {{:http://wiki.libsdl.org/SDL_RenderPresent}SDL_RenderPresent} *)

val render_read_pixels : renderer -> rect option -> Pixel.format_enum option ->
  ('a, 'b) bigarray -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderReadPixels}SDL_RenderReadPixels} *)

val render_set_clip_rect : renderer -> rect option -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderSetClipRect}SDL_RenderSetClipRect} *)

val render_set_logical_size : renderer -> int -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderSetLogicalSize}
    SDL_RenderSetLogicalSize} *)

val render_set_scale : renderer -> float -> float -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderSetScale}SDL_RenderSetScale} *)

val render_set_viewport : renderer -> rect option -> unit result
(** {{:http://wiki.libsdl.org/SDL_RenderSetViewport}SDL_RenderSetViewport} *)

val render_target_supported : renderer -> bool
(** {{:http://wiki.libsdl.org/SDL_RenderTargetSupported}
    SDL_RenderTargetSupported} *)

val set_render_draw_blend_mode : renderer -> Blend.mode -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetRenderDrawBlendMode}
    SDL_SetRenderDrawBlendMode} *)

val set_render_draw_color : renderer -> uint8 -> uint8 -> uint8 -> uint8 ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_SetRenderDrawColor}SDL_SetRenderDrawColor} *)

val set_render_target : renderer -> texture option -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetRenderTarget}SDL_SetRenderTarget} *)

(** {2:textures {{:http://wiki.libsdl.org/CategoryRender}Textures}} *)

module Texture : sig
  type access
  (** {{:https://wiki.libsdl.org/SDL_TextureAccess}SDL_TextureAccess} *)
  val access_static : access
  val access_streaming : access
  val access_target : access

  type modulate
  (** {{:https://wiki.libsdl.org/SDL_TextureModulate}SDL_TextureModulate} *)
  val modulate_none : modulate
  val modulate_color : modulate
  val modulate_alpha : modulate
end

val create_texture : renderer -> Pixel.format_enum -> Texture.access ->
  w:int -> h:int -> texture result
(** {{:http://wiki.libsdl.org/SDL_CreateTexture}SDL_CreateTexture} *)

val create_texture_from_surface : renderer -> surface -> texture result
(** {{:http://wiki.libsdl.org/SDL_CreateTextureFromSurface}
    SDL_CreateTextureFromSurface} *)

val destroy_texture : texture -> unit
(** {{:http://wiki.libsdl.org/SDL_DestroyTexture}SDL_DestroyTexture} *)

val get_texture_alpha_mod : texture -> uint8 result
(** {{:http://wiki.libsdl.org/SDL_GetTextureAlphaMod}SDL_GetTextureAlphaMod} *)

val get_texture_blend_mode : texture -> Blend.mode result
(** {{:http://wiki.libsdl.org/SDL_GetTextureBlendMode}
    SDL_GetTextureBlendMode} *)

val get_texture_color_mod : texture -> (uint8 * uint8 * uint8) result
(** {{:http://wiki.libsdl.org/SDL_GetTextureColorMod}SDL_GetTextureColorMod}. *)

val lock_texture : texture -> rect option -> ('a, 'b) Bigarray.kind ->
  (('a, 'b) bigarray * int) result
(** {{:http://wiki.libsdl.org/SDL_LockTexture}SDL_LockTexture}

    {b Note.} The returned pitch is in bigarray element, {b not} in bytes.

    @raise Invalid_argument If [kind] can't align with the texture pitch. *)

val query_texture : texture ->
  (Pixel.format_enum * Texture.access * (int * int)) result
(** {{:http://wiki.libsdl.org/SDL_QueryTexture}SDL_QueryTexture} *)

val set_texture_alpha_mod : texture -> uint8 -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetTextureAlphaMod}
    SDL_SetTextureAlphaMod} *)

val set_texture_blend_mode : texture -> Blend.mode -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetTextureBlendMode}
    SDL_SetTextureBlendMode} *)

val set_texture_color_mod : texture -> uint8 -> uint8 -> uint8 -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetTextureColorMod}
    SDL_SetTextureColorMod} *)

val unlock_texture : texture -> unit
(** {{:http://wiki.libsdl.org/SDL_UnlockTexture}SDL_UnlockTexture} *)

val update_texture : texture -> rect option -> ('a, 'b) bigarray -> int ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_UpdateTexture}SDL_UpdateTexture}

    {b Note} The pitch is given in bigarray elements {b not} in
    bytes. *)

val update_yuv_texture : texture -> rect option ->
  y:(int, Bigarray.int8_unsigned_elt) bigarray -> int ->
  u:(int, Bigarray.int8_unsigned_elt) bigarray -> int ->
  v:(int, Bigarray.int8_unsigned_elt) bigarray -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_UpdateYUVTexture}SDL_UpdateYUVTexture} *)

(** {2:videodrivers {{:http://wiki.libsdl.org/CategoryVideo}Video drivers}} *)

val get_current_video_driver : unit -> string option
(** {{:http://wiki.libsdl.org/SDL_GetCurrentVideoDriver}
    SDL_GetCurrentVideoDriver} *)

val get_num_video_drivers : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_GetNumVideoDrivers}SDL_GetNumVideoDrivers} *)

val get_video_driver : int -> string result
(** {{:http://wiki.libsdl.org/SDL_GetVideoDriver}SDL_GetVideoDriver} *)

val video_init : string option -> unit result
(** {{:http://wiki.libsdl.org/SDL_VideoInit}SDL_VideoInit} *)

val video_quit : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_VideoQuit}SDL_VideoQuit} *)

(** {2:displays {{:http://wiki.libsdl.org/CategoryVideo}Displays}} *)

type driverdata
(** {b Note.} Nothing can be done with that. *)

type display_mode =
  { dm_format : Pixel.format_enum;
    dm_w : int;
    dm_h : int;
    dm_refresh_rate : int option;
    dm_driverdata : driverdata option }
(** {{:http://wiki.libsdl.org/SDL_DisplayMode}SDL_DisplayMode} *)

val get_closest_display_mode : int -> display_mode -> display_mode option
(** {{:http://wiki.libsdl.org/SDL_GetClosestDisplayMode}
    SDL_GetClosestDisplayMode} *)

val get_current_display_mode : int -> display_mode result
(** {{:http://wiki.libsdl.org/SDL_GetCurrentDisplayMode}
    SDL_GetCurrentDisplayMode} *)

val get_desktop_display_mode : int -> display_mode result
(** {{:http://wiki.libsdl.org/SDL_GetDesktopDisplayMode}
    SDL_GetDesktopDisplayMode} *)

val get_display_bounds : int -> rect result
(** {{:http://wiki.libsdl.org/SDL_GetDisplayBounds}SDL_GetDisplayBounds} *)

val get_display_mode : int -> int -> display_mode result
(** {{:http://wiki.libsdl.org/SDL_GetDisplayMode}SDL_GetDisplayMode} *)

val get_display_name : int -> string result
(** {{:http://wiki.libsdl.org/SDL_GetDisplayName}SDL_GetDisplayName} *)

val get_num_display_modes : int -> int result
(** {{:http://wiki.libsdl.org/SDL_GetNumDisplayModes}SDL_GetNumDisplayModes} *)


val get_num_video_displays : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_GetNumVideoDisplays}
    SDL_GetNumVideoDisplays} *)

(** {2:windows {{:http://wiki.libsdl.org/CategoryVideo}Windows}} *)

module Window : sig

  (** {1:position Position} *)

  val pos_undefined : int
  val pos_centered : int

  (** {1:position Flags} *)

  type flags
  (** {{:http://wiki.libsdl.org/SDL_WindowFlags}SDL_WindowFlags} *)

  val ( + ) : flags -> flags -> flags
  (** [f + f'] combines flags [f] and [f']. *)

  val test : flags -> flags -> bool
  (** [test flags mask] is [true] if any of the flags in [mask] is
      set in [flags]. *)

  val eq : flags -> flags -> bool
  (** [eq f f'] is [true] if the flags are equal. *)

  val windowed : flags
  (** Equal to [0]. The flag doesn't exist in SDL, it's for using with
      {!set_window_fullscreen}. *)
  val fullscreen : flags
  val fullscreen_desktop : flags
  val opengl : flags
  val shown : flags
  val hidden : flags
  val borderless : flags
  val resizable : flags
  val minimized : flags
  val maximized : flags
  val input_grabbed : flags
  val input_focus : flags
  val mouse_focus : flags
  val foreign : flags
  val allow_highdpi : flags
end

val create_window : string -> ?x:int -> ?y:int -> w:int -> h:int ->
  Window.flags -> window result
(** {{:http://wiki.libsdl.org/SDL_CreateWindow}SDL_CreateWindow}

    [x] and [y] default to {!Window.pos_undefined}. *)

val create_window_and_renderer : w:int -> h:int -> Window.flags ->
  (window * renderer) result
(** {{:http://wiki.libsdl.org/SDL_CreateWindowAndRenderer}
    SDL_CreateWindowAndRenderer} *)

val destroy_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_DestroyWindow}SDL_DestroyWindow} *)

val get_window_brightness : window -> float
(** {{:http://wiki.libsdl.org/SDL_GetWindowBrightness}
    SDL_GetWindowBrightness} *)

val get_window_display_index : window -> int result
(** {{:http://wiki.libsdl.org/SDL_GetWindowDisplay}SDL_GetWindowDisplayIndex} *)

val get_window_display_mode : window -> display_mode result
(** {{:http://wiki.libsdl.org/SDL_GetWindowDisplayMode}
    SDL_GetWindowDisplayMode} *)

val get_window_flags : window -> Window.flags
(** {{:http://wiki.libsdl.org/SDL_GetWindowFlags}SDL_GetWindowFlags} *)

val get_window_from_id : int -> window result
(** {{:http://wiki.libsdl.org/SDL_GetWindowFromID}SDL_GetWindowFromID} *)

val get_window_gamma_ramp : window ->
  (gamma_ramp * gamma_ramp * gamma_ramp) result
(** {{:http://wiki.libsdl.org/SDL_GetWindowGammaRamp}
    SDL_GetWindowGammaRamp} *)

val get_window_grab : window -> bool
(** {{:http://wiki.libsdl.org/SDL_GetWindowGrab}SDL_GetWindowGrab} *)

val get_window_id : window -> int
(** {{:http://wiki.libsdl.org/SDL_GetWindowID}SDL_GetWindowID} *)

val get_window_maximum_size : window -> int * int
(** {{:http://wiki.libsdl.org/SDL_GetWindowMaximumSize}
    SDL_GetWindowMaximumSize} *)

val get_window_minimum_size : window -> int * int
(** {{:http://wiki.libsdl.org/SDL_GetWindowMinimumSize}
    SDL_GetWindowMinimumSize} *)

val get_window_pixel_format : window -> Pixel.format_enum
(** {{:http://wiki.libsdl.org/SDL_GetWindowPixelFormat}
    SDL_GetWindowPixelFormat} *)

val get_window_position : window -> int * int
(** {{:http://wiki.libsdl.org/SDL_GetWindowPosition}SDL_GetWindowPosition} *)

val get_window_size : window -> int * int
(** {{:http://wiki.libsdl.org/SDL_GetWindowSize}SDL_GetWindowSize} *)

val get_window_surface : window -> surface result
(** {{:http://wiki.libsdl.org/SDL_GetWindowSurface}SDL_GetWindowSurface}.

    {b Note}. According to SDL's documentation the surface
    is freed when the window is destroyed. *)

val get_window_title : window -> string
(** {{:http://wiki.libsdl.org/SDL_GetWindowTitle}SDL_GetWindowTitle} *)

val hide_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_HideWindow}SDL_HideWindow} *)

val maximize_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_MaximizeWindow}SDL_MaximizeWindow} *)

val minimize_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_MinimizeWindow}SDL_MinimizeWindow} *)

val raise_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_RaiseWindow}SDL_RaiseWindow} *)

val restore_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_RestoreWindow}SDL_RestoreWindow} *)

val set_window_bordered : window -> bool -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowBordered}SDL_SetWindowBordered} *)

val set_window_brightness : window -> float -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetWindowBrightness}
    SDL_SetWindowBrightness} *)

val set_window_display_mode : window -> display_mode -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetWindowDisplayMode}
    SDL_SetWindowDisplayMode} *)

val set_window_fullscreen : window -> Window.flags -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetWindowFullscreen}
    SDL_SetWindowFullscreen} *)

val set_window_gamma_ramp : window -> gamma_ramp -> gamma_ramp ->
  gamma_ramp -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetWindowGammaRamp}SDL_SetWindowGammaRamp} *)

val set_window_grab : window -> bool -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowGrab}SDL_SetWindowGrab} *)

val set_window_icon : window -> surface -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowIcon}SDL_SetWindowIcon} *)

val set_window_maximum_size : window -> w:int -> h:int -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowMaximumSize}
    SDL_SetWindowMaximumSize} *)

val set_window_minimum_size : window -> w:int -> h:int -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowMinimumSize}
    SDL_SetWindowMinimumSize} *)

val set_window_position : window -> x:int -> y:int -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowPosition}SDL_SetWindowPosition} *)

val set_window_size : window -> w:int -> h:int -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowSize}SDL_SetWindowSize} *)

val set_window_title : window -> string -> unit
(** {{:http://wiki.libsdl.org/SDL_SetWindowTitle}SDL_SetWindowTitle} *)

val show_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_ShowWindow}SDL_ShowWindow} *)

val update_window_surface : window -> unit result
(** {{:http://wiki.libsdl.org/SDL_UpdateWindowSurface}
    SDL_UpdateWindowSurface} *)

val update_window_surface_rects : window -> rect list -> unit result
(** {{:http://wiki.libsdl.org/SDL_UpdateWindowSurfaceRects}
    SDL_UpdateWindowSurfaceRects} *)

val update_window_surface_rects_ba : window ->
  (int32, Bigarray.int32_elt) bigarray -> unit result
(** See {!update_window_surface_rects}. Each consecutive quadruplet defines a
    rectangle.

    @raise Invalid_argument if the length of the array is not
    a multiple of 4. *)

(** {2:opengl {{:http://wiki.libsdl.org/CategoryVideo}OpenGL contexts}} *)

type gl_context

(**/**)
val unsafe_gl_context_of_ptr : nativeint -> gl_context
val unsafe_ptr_of_gl_context : gl_context -> nativeint
(**/**)

(** SDL_GLContext *)

module Gl : sig
  (** {1:flags Context flags} *)

  type context_flags = int
  (** {{:http://wiki.libsdl.org/SDL_GLcontextFlag}SDL_GLcontextFlag} *)

  val context_debug_flag : context_flags
  val context_forward_compatible_flag : context_flags
  val context_robust_access_flag : context_flags
  val context_reset_isolation_flag : context_flags

  (** {1:profile Profile flags} *)

  type profile = int
  (** {{:http://wiki.libsdl.org/SDL_GLprofile}SDL_GLprofile} *)

  val context_profile_core : profile
  val context_profile_compatibility : profile
  val context_profile_es : profile

  (** {1:attr Attributes} *)

  type attr
  (** {{:http://wiki.libsdl.org/SDL_GLattr}SDL_GLattr} *)

  val red_size : attr
  val green_size : attr
  val blue_size : attr
  val alpha_size : attr
  val buffer_size : attr
  val doublebuffer : attr
  val depth_size : attr
  val stencil_size : attr
  val accum_red_size : attr
  val accum_green_size : attr
  val accum_blue_size : attr
  val accum_alpha_size : attr
  val stereo : attr
  val multisamplebuffers : attr
  val multisamplesamples : attr
  val accelerated_visual : attr
  val context_major_version : attr
  val context_minor_version : attr
  val context_egl : attr
  val context_flags : attr
  val context_profile_mask : attr
  val share_with_current_context : attr
  val framebuffer_srgb_capable : attr
end

val gl_create_context : window -> gl_context result
(** {{:http://wiki.libsdl.org/SDL_GL_CreateContext}SDL_GL_CreateContext} *)

val gl_bind_texture : texture -> (float * float) result
(** {{:http://wiki.libsdl.org/SDL_GL_BindTexture}SDL_GL_BindTexture} *)

val gl_delete_context : gl_context -> unit
(** {{:http://wiki.libsdl.org/SDL_GL_DeleteContext}SDL_GL_DeleteContext} *)

val gl_extension_supported : string -> bool
(** {{:http://wiki.libsdl.org/SDL_GL_ExtensionSupported}
    SDL_GL_ExtensionSupported} *)

val gl_get_attribute : Gl.attr -> int result
(** {{:http://wiki.libsdl.org/SDL_GL_GetAttribute}SDL_GL_GetAttribute} *)

val gl_get_current_context : unit -> gl_context result
(** {{:http://wiki.libsdl.org/SDL_GL_GetCurrentContext}
    SDL_GL_GetCurrentContext} *)

val gl_get_drawable_size : window -> int * int
(** {{:http://wiki.libsdl.org/SDL_GL_GetDrawableSize}SDL_GL_GetDrawableSize} *)

val gl_get_swap_interval : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_GL_GetSwapInterval}SDL_GL_GetSwapInterval} *)

val gl_make_current : window -> gl_context -> unit result
(** {{:http://wiki.libsdl.org/SDL_GL_MakeCurrent}SDL_GL_MakeCurrent} *)

val gl_set_attribute : Gl.attr -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_GL_SetAttribute}SDL_GL_SetAttribute} *)

val gl_set_swap_interval : int -> unit result
(** {{:http://wiki.libsdl.org/SDL_GL_SetSwapInterval}SDL_GL_SetSwapInterval} *)

val gl_swap_window : window -> unit
(** {{:http://wiki.libsdl.org/SDL_GL_SwapWindow}SDL_GL_SwapWindow} *)

val gl_reset_attributes : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_GL_ResetAttributes}SDL_GL_ResetAttributes}
    (SDL 2.0.2). *)

val gl_unbind_texture : texture -> unit result
(** {{:http://wiki.libsdl.org/SDL_GL_UnbindTexture}SDL_GL_UnbindTexture}
    {b Warning} Segfaults on SDL 2.0.1
    see {{:https://bugzilla.libsdl.org/show_bug.cgi?id=2296}this report}.*)

(** {2:screensaver Screen saver} *)

val disable_screen_saver : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_DisableScreenSaver}SDL_DisableScreenSaver} *)

val enable_screen_saver : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_EnableScreenSaver}SDL_EnableScreenSaver} *)

val is_screen_saver_enabled : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_IsScreenSaverEnabled}
    SDL_IsScreenSaverEnabled} *)

(** {2:messageboxes Message boxes} *)

module Message_box : sig

  (** {1 Message box Buttons} *)

  type button_flags
  val button_returnkey_default : button_flags
  val button_escapekey_default : button_flags

  type button_data =
    { button_flags : button_flags;
      button_id : int;
      button_text : string }

  (** {1 Message box flags} *)

  type flags
  val error : flags
  val warning : flags
  val information : flags

  (** {1 Message box color scheme} *)

  type color = int * int * int
  (** r, g, b from 0 to 255 *)

  type color_scheme =
    { color_background : color;
      color_text : color;
      color_button_border : color;
      color_button_background : color;
      color_button_selected : color; }

  (** {1 Message box data} *)

  type data =
    { flags : flags;
      window : window option;
      title : string;
      message : string;
      buttons : button_data list;
      color_scheme : color_scheme option }
end

val show_message_box : Message_box.data -> int result
(** {{:https://wiki.libsdl.org/SDL_ShowMessageBox}SDL_ShowMessageBox} *)

val show_simple_message_box : Message_box.flags -> title:string -> string ->
  window option -> unit result
(** {{:https://wiki.libsdl.org/SDL_ShowSimpleMessageBox}
    SDL_ShowSimpleMessageBox} *)

(** {2:clipboard
    {{:http://wiki.libsdl.org/CategoryClipboard}Clipboard}} *)

val get_clipboard_text : unit -> string result
(** {{:http://wiki.libsdl.org/SDL_GetClipboardText}SDL_GetClipboardText} *)

val has_clipboard_text : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasClipboardText}SDL_HasClipboardText} *)

val set_clipboard_text : string -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetClipboardText}SDL_SetClipboardText} *)

(** {1:input Input} *)

type button_state
val pressed : button_state
val released : button_state

type toggle_state
val disable : toggle_state
val enable : toggle_state

(** {2:keyboard {{:http://wiki.libsdl.org/CategoryKeyboard}Keyboard}} *)

type scancode = int
(** {{:http://wiki.libsdl.org/SDL_Scancode}SDL_Scancode} *)

(** Constants and enumeration for {!scancode} *)
module Scancode : sig

  val enum : scancode ->
    [ `A | `Ac_back | `Ac_bookmarks | `Ac_forward | `Ac_home
    | `Ac_refresh | `Ac_search | `Ac_stop | `Again | `Alterase
    | `Apostrophe | `App1 | `App2 | `Application | `Audiomute
    | `Audionext | `Audioplay | `Audioprev | `Audiostop | `B
    | `Backslash | `Backspace | `Brightnessdown | `Brightnessup | `C
    | `Calculator | `Cancel | `Capslock | `Clear | `Clearagain | `Comma
    | `Computer | `Copy | `Crsel | `Currencysubunit | `Currencyunit
    | `Cut | `D | `Decimalseparator | `Delete | `Displayswitch | `Down
    | `E | `Eject | `End | `Equals | `Escape | `Execute | `Exsel | `F
    | `F1 | `F10 | `F11 | `F12 | `F13 | `F14 | `F15 | `F16 | `F17 | `F18
    | `F19 | `F2 | `F20 | `F21 | `F22 | `F23 | `F24 | `F3 | `F4 | `F5
    | `F6 | `F7 | `F8 | `F9 | `Find | `G | `Grave | `H | `Help | `Home
    | `I | `Insert | `International1 | `International2
    | `International3 | `International4 | `International5
    | `International6 | `International7 | `International8
    | `International9 | `J | `K | `K0 | `K1 | `K2 | `K3 | `K4 | `K5
    | `K6 | `K7 | `K8 | `K9 | `Kbdillumdown | `Kbdillumtoggle
    | `Kbdillumup | `Kp_0 | `Kp_00 | `Kp_000 | `Kp_1 | `Kp_2 | `Kp_3
    | `Kp_4 | `Kp_5 | `Kp_6 | `Kp_7 | `Kp_8 | `Kp_9 | `Kp_a
    | `Kp_ampersand | `Kp_at | `Kp_b | `Kp_backspace | `Kp_binary
    | `Kp_c | `Kp_clear | `Kp_clearentry | `Kp_colon | `Kp_comma | `Kp_d
    | `Kp_dblampersand | `Kp_dblverticalbar | `Kp_decimal | `Kp_divide
    | `Kp_e | `Kp_enter | `Kp_equals | `Kp_equalsas400 | `Kp_exclam
    | `Kp_f | `Kp_greater | `Kp_hash | `Kp_hexadecimal | `Kp_leftbrace
    | `Kp_leftparen | `Kp_less | `Kp_memadd | `Kp_memclear
    | `Kp_memdivide | `Kp_memmultiply | `Kp_memrecall | `Kp_memstore
    | `Kp_memsubtract | `Kp_minus | `Kp_multiply | `Kp_octal
    | `Kp_percent | `Kp_period | `Kp_plus | `Kp_plusminus | `Kp_power
    | `Kp_rightbrace | `Kp_rightparen | `Kp_space | `Kp_tab
    | `Kp_verticalbar | `Kp_xor | `L | `Lalt | `Lang1 | `Lang2 | `Lang3
    | `Lang4 | `Lang5 | `Lang6 | `Lang7 | `Lang8 | `Lang9 | `Lctrl
    | `Left | `Leftbracket | `Lgui | `Lshift | `M | `Mail | `Mediaselect
    | `Menu | `Minus | `Mode | `Mute | `N | `Nonusbackslash
    | `Nonushash | `Numlockclear | `O | `Oper | `Out | `P | `Pagedown
    | `Pageup | `Paste | `Pause | `Period | `Power | `Printscreen
    | `Prior | `Q | `R | `Ralt | `Rctrl | `Return | `Return2 | `Rgui
    | `Right | `Rightbracket | `Rshift | `S | `Scrolllock | `Select
    | `Semicolon | `Separator | `Slash | `Sleep | `Space | `Stop
    | `Sysreq | `T | `Tab | `Thousandsseparator | `U | `Undo | `Unknown
    | `Up | `V | `Volumedown | `Volumeup | `W | `Www | `X | `Y | `Z ]

  val num_scancodes : int
  val unknown : scancode
  val a : scancode
  val b : scancode
  val c : scancode
  val d : scancode
  val e : scancode
  val f : scancode
  val g : scancode
  val h : scancode
  val i : scancode
  val j : scancode
  val k : scancode
  val l : scancode
  val m : scancode
  val n : scancode
  val o : scancode
  val p : scancode
  val q : scancode
  val r : scancode
  val s : scancode
  val t : scancode
  val u : scancode
  val v : scancode
  val w : scancode
  val x : scancode
  val y : scancode
  val z : scancode
  val k1 : scancode
  val k2 : scancode
  val k3 : scancode
  val k4 : scancode
  val k5 : scancode
  val k6 : scancode
  val k7 : scancode
  val k8 : scancode
  val k9 : scancode
  val k0 : scancode
  val return : scancode
  val escape : scancode
  val backspace : scancode
  val tab : scancode
  val space : scancode
  val minus : scancode
  val equals : scancode
  val leftbracket : scancode
  val rightbracket : scancode
  val backslash : scancode
  val nonushash : scancode
  val semicolon : scancode
  val apostrophe : scancode
  val grave : scancode
  val comma : scancode
  val period : scancode
  val slash : scancode
  val capslock : scancode
  val f1 : scancode
  val f2 : scancode
  val f3 : scancode
  val f4 : scancode
  val f5 : scancode
  val f6 : scancode
  val f7 : scancode
  val f8 : scancode
  val f9 : scancode
  val f10 : scancode
  val f11 : scancode
  val f12 : scancode
  val printscreen : scancode
  val scrolllock : scancode
  val pause : scancode
  val insert : scancode
  val home : scancode
  val pageup : scancode
  val delete : scancode
  val kend : scancode
  val pagedown : scancode
  val right : scancode
  val left : scancode
  val down : scancode
  val up : scancode
  val numlockclear : scancode
  val kp_divide : scancode
  val kp_multiply : scancode
  val kp_minus : scancode
  val kp_plus : scancode
  val kp_enter : scancode
  val kp_1 : scancode
  val kp_2 : scancode
  val kp_3 : scancode
  val kp_4 : scancode
  val kp_5 : scancode
  val kp_6 : scancode
  val kp_7 : scancode
  val kp_8 : scancode
  val kp_9 : scancode
  val kp_0 : scancode
  val kp_period : scancode
  val nonusbackslash : scancode
  val application : scancode
  val kp_equals : scancode
  val f13 : scancode
  val f14 : scancode
  val f15 : scancode
  val f16 : scancode
  val f17 : scancode
  val f18 : scancode
  val f19 : scancode
  val f20 : scancode
  val f21 : scancode
  val f22 : scancode
  val f23 : scancode
  val f24 : scancode
  val execute : scancode
  val help : scancode
  val menu : scancode
  val select : scancode
  val stop : scancode
  val again : scancode
  val undo : scancode
  val cut : scancode
  val copy : scancode
  val paste : scancode
  val find : scancode
  val mute : scancode
  val volumeup : scancode
  val volumedown : scancode
  val kp_comma : scancode
  val kp_equalsas400 : scancode
  val international1 : scancode
  val international2 : scancode
  val international3 : scancode
  val international4 : scancode
  val international5 : scancode
  val international6 : scancode
  val international7 : scancode
  val international8 : scancode
  val international9 : scancode
  val lang1 : scancode
  val lang2 : scancode
  val lang3 : scancode
  val lang4 : scancode
  val lang5 : scancode
  val lang6 : scancode
  val lang7 : scancode
  val lang8 : scancode
  val lang9 : scancode
  val alterase : scancode
  val sysreq : scancode
  val cancel : scancode
  val clear : scancode
  val prior : scancode
  val return2 : scancode
  val separator : scancode
  val out : scancode
  val oper : scancode
  val clearagain : scancode
  val crsel : scancode
  val exsel : scancode
  val kp_00 : scancode
  val kp_000 : scancode
  val thousandsseparator : scancode
  val decimalseparator : scancode
  val currencyunit : scancode
  val currencysubunit : scancode
  val kp_leftparen : scancode
  val kp_rightparen : scancode
  val kp_leftbrace : scancode
  val kp_rightbrace : scancode
  val kp_tab : scancode
  val kp_backspace : scancode
  val kp_a : scancode
  val kp_b : scancode
  val kp_c : scancode
  val kp_d : scancode
  val kp_e : scancode
  val kp_f : scancode
  val kp_xor : scancode
  val kp_power : scancode
  val kp_percent : scancode
  val kp_less : scancode
  val kp_greater : scancode
  val kp_ampersand : scancode
  val kp_dblampersand : scancode
  val kp_verticalbar : scancode
  val kp_dblverticalbar : scancode
  val kp_colon : scancode
  val kp_hash : scancode
  val kp_space : scancode
  val kp_at : scancode
  val kp_exclam : scancode
  val kp_memstore : scancode
  val kp_memrecall : scancode
  val kp_memclear : scancode
  val kp_memadd : scancode
  val kp_memsubtract : scancode
  val kp_memmultiply : scancode
  val kp_memdivide : scancode
  val kp_plusminus : scancode
  val kp_clear : scancode
  val kp_clearentry : scancode
  val kp_binary : scancode
  val kp_octal : scancode
  val kp_decimal : scancode
  val kp_hexadecimal : scancode
  val lctrl : scancode
  val lshift : scancode
  val lalt : scancode
  val lgui : scancode
  val rctrl : scancode
  val rshift : scancode
  val ralt : scancode
  val rgui : scancode
  val mode : scancode
  val audionext : scancode
  val audioprev : scancode
  val audiostop : scancode
  val audioplay : scancode
  val audiomute : scancode
  val mediaselect : scancode
  val www : scancode
  val mail : scancode
  val calculator : scancode
  val computer : scancode
  val ac_search : scancode
  val ac_home : scancode
  val ac_back : scancode
  val ac_forward : scancode
  val ac_stop : scancode
  val ac_refresh : scancode
  val ac_bookmarks : scancode
  val brightnessdown : scancode
  val brightnessup : scancode
  val displayswitch : scancode
  val kbdillumtoggle : scancode
  val kbdillumdown : scancode
  val kbdillumup : scancode
  val eject : scancode
  val sleep : scancode
  val app1 : scancode
  val app2 : scancode
end

type keycode = int
(** {{:http://wiki.libsdl.org/SDL_Keycode}SDL_Keycode} *)

(** Constants for {!keycode} *)
module K : sig
  val scancode_mask : int
  val unknown : keycode
  val return : keycode
  val escape : keycode
  val backspace : keycode
  val tab : keycode
  val space : keycode
  val exclaim : keycode
  val quotedbl : keycode
  val hash : keycode
  val percent : keycode
  val dollar : keycode
  val ampersand : keycode
  val quote : keycode
  val leftparen : keycode
  val rightparen : keycode
  val asterisk : keycode
  val plus : keycode
  val comma : keycode
  val minus : keycode
  val period : keycode
  val slash : keycode
  val k0 : keycode
  val k1 : keycode
  val k2 : keycode
  val k3 : keycode
  val k4 : keycode
  val k5 : keycode
  val k6 : keycode
  val k7 : keycode
  val k8 : keycode
  val k9 : keycode
  val colon : keycode
  val semicolon : keycode
  val less : keycode
  val equals : keycode
  val greater : keycode
  val question : keycode
  val at : keycode
  val leftbracket : keycode
  val backslash : keycode
  val rightbracket : keycode
  val caret : keycode
  val underscore : keycode
  val backquote : keycode
  val a : keycode
  val b : keycode
  val c : keycode
  val d : keycode
  val e : keycode
  val f : keycode
  val g : keycode
  val h : keycode
  val i : keycode
  val j : keycode
  val k : keycode
  val l : keycode
  val m : keycode
  val n : keycode
  val o : keycode
  val p : keycode
  val q : keycode
  val r : keycode
  val s : keycode
  val t : keycode
  val u : keycode
  val v : keycode
  val w : keycode
  val x : keycode
  val y : keycode
  val z : keycode
  val capslock : keycode
  val f1 : keycode
  val f2 : keycode
  val f3 : keycode
  val f4 : keycode
  val f5 : keycode
  val f6 : keycode
  val f7 : keycode
  val f8 : keycode
  val f9 : keycode
  val f10 : keycode
  val f11 : keycode
  val f12 : keycode
  val printscreen : keycode
  val scrolllock : keycode
  val pause : keycode
  val insert : keycode
  val home : keycode
  val pageup : keycode
  val delete : keycode
  val kend : keycode
  val pagedown : keycode
  val right : keycode
  val left : keycode
  val down : keycode
  val up : keycode
  val numlockclear : keycode
  val kp_divide : keycode
  val kp_multiply : keycode
  val kp_minus : keycode
  val kp_plus : keycode
  val kp_enter : keycode
  val kp_1 : keycode
  val kp_2 : keycode
  val kp_3 : keycode
  val kp_4 : keycode
  val kp_5 : keycode
  val kp_6 : keycode
  val kp_7 : keycode
  val kp_8 : keycode
  val kp_9 : keycode
  val kp_0 : keycode
  val kp_period : keycode
  val application : keycode
  val power : keycode
  val kp_equals : keycode
  val f13 : keycode
  val f14 : keycode
  val f15 : keycode
  val f16 : keycode
  val f17 : keycode
  val f18 : keycode
  val f19 : keycode
  val f20 : keycode
  val f21 : keycode
  val f22 : keycode
  val f23 : keycode
  val f24 : keycode
  val execute : keycode
  val help : keycode
  val menu : keycode
  val select : keycode
  val stop : keycode
  val again : keycode
  val undo : keycode
  val cut : keycode
  val copy : keycode
  val paste : keycode
  val find : keycode
  val mute : keycode
  val volumeup : keycode
  val volumedown : keycode
  val kp_comma : keycode
  val kp_equalsas400 : keycode
  val alterase : keycode
  val sysreq : keycode
  val cancel : keycode
  val clear : keycode
  val prior : keycode
  val return2 : keycode
  val separator : keycode
  val out : keycode
  val oper : keycode
  val clearagain : keycode
  val crsel : keycode
  val exsel : keycode
  val kp_00 : keycode
  val kp_000 : keycode
  val thousandsseparator : keycode
  val decimalseparator : keycode
  val currencyunit : keycode
  val currencysubunit : keycode
  val kp_leftparen : keycode
  val kp_rightparen : keycode
  val kp_leftbrace : keycode
  val kp_rightbrace : keycode
  val kp_tab : keycode
  val kp_backspace : keycode
  val kp_a : keycode
  val kp_b : keycode
  val kp_c : keycode
  val kp_d : keycode
  val kp_e : keycode
  val kp_f : keycode
  val kp_xor : keycode
  val kp_power : keycode
  val kp_percent : keycode
  val kp_less : keycode
  val kp_greater : keycode
  val kp_ampersand : keycode
  val kp_dblampersand : keycode
  val kp_verticalbar : keycode
  val kp_dblverticalbar : keycode
  val kp_colon : keycode
  val kp_hash : keycode
  val kp_space : keycode
  val kp_at : keycode
  val kp_exclam : keycode
  val kp_memstore : keycode
  val kp_memrecall : keycode
  val kp_memclear : keycode
  val kp_memadd : keycode
  val kp_memsubtract : keycode
  val kp_memmultiply : keycode
  val kp_memdivide : keycode
  val kp_plusminus : keycode
  val kp_clear : keycode
  val kp_clearentry : keycode
  val kp_binary : keycode
  val kp_octal : keycode
  val kp_decimal : keycode
  val kp_hexadecimal : keycode
  val lctrl : keycode
  val lshift : keycode
  val lalt : keycode
  val lgui : keycode
  val rctrl : keycode
  val rshift : keycode
  val ralt : keycode
  val rgui : keycode
  val mode : keycode
  val audionext : keycode
  val audioprev : keycode
  val audiostop : keycode
  val audioplay : keycode
  val audiomute : keycode
  val mediaselect : keycode
  val www : keycode
  val mail : keycode
  val calculator : keycode
  val computer : keycode
  val ac_search : keycode
  val ac_home : keycode
  val ac_back : keycode
  val ac_forward : keycode
  val ac_stop : keycode
  val ac_refresh : keycode
  val ac_bookmarks : keycode
  val brightnessdown : keycode
  val brightnessup : keycode
  val displayswitch : keycode
  val kbdillumtoggle : keycode
  val kbdillumdown : keycode
  val kbdillumup : keycode
  val eject : keycode
  val sleep : keycode
end

type keymod = int
(** {{:http://wiki.libsdl.org/SDL_Keymod}SDL_Keymod}. *)

(** Constants for {!keymod} *)
module Kmod : sig
  val none : keymod
  val lshift : keymod
  val rshift : keymod
  val lctrl : keymod
  val rctrl : keymod
  val lalt : keymod
  val ralt : keymod
  val lgui : keymod
  val rgui : keymod
  val num : keymod
  val caps : keymod
  val mode : keymod
  val reserved : keymod
  val ctrl : keymod
  val shift : keymod
  val alt : keymod
  val gui : keymod
end

val get_keyboard_focus : unit -> window option
(** {{:http://wiki.libsdl.org/SDL_GetKeyboardFocus}
    SDL_GetKeyboardFocus} *)

val get_keyboard_state : unit -> (int, Bigarray.int8_unsigned_elt) bigarray
(** {{:http://wiki.libsdl.org/SDL_GetKeyboardState}SDL_GetKeyboardState} *)

val get_key_from_name : string -> keycode
(** {{:http://wiki.libsdl.org/SDL_GetKeyFromName}SDL_GetKeyFromName} *)

val get_key_from_scancode : scancode -> keycode
(** {{:http://wiki.libsdl.org/SDL_GetKeyFromScancode}SDL_GetKeyFromScancode} *)

val get_key_name : keycode -> string
(** {{:http://wiki.libsdl.org/SDL_GetKeyName}SDL_GetKeyName} *)

val get_mod_state : unit -> keymod
(** {{:http://wiki.libsdl.org/SDL_GetModState}SDL_GetModState} *)

val get_scancode_from_key : keycode -> scancode
(** {{:http://wiki.libsdl.org/SDL_GetScancodeFromKey}SDL_GetScancodeFromKey} *)

val get_scancode_from_name : string -> scancode
(** {{:http://wiki.libsdl.org/SDL_GetScancodeFromName}SDL_GetScancodeFromName}*)

val get_scancode_name : scancode -> string
(** {{:http://wiki.libsdl.org/SDL_GetScancodeName}SDL_GetScancodeName} *)

val has_screen_keyboard_support : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasScreenKeyboardSupport}
    SDL_HasScreenKeyboardSupport} *)

val is_screen_keyboard_shown : window -> bool
(** {{:http://wiki.libsdl.org/SDL_IsScreenKeyboardShown}
    SDL_IsScreenKeyboardShown} *)

val is_text_input_active : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_IsTextInputActive}SDL_IsTextInputActive} *)

val set_mod_state : keymod -> unit
(** {{:http://wiki.libsdl.org/SDL_SetModState}SDL_SetModState} *)

val set_text_input_rect : rect option -> unit
(** {{:http://wiki.libsdl.org/SDL_SetTextInputRect}SDL_SetTextInputRect} *)

val start_text_input : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_StartTextInput}SDL_StartTextInput} *)

val stop_text_input : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_StopTextInput}SDL_StopTextInput} *)

(** {2:mouse {{:http://wiki.libsdl.org/CategoryMouse}Mouse}} *)

type cursor

(**/**)
val unsafe_cursor_of_ptr : nativeint -> cursor
val unsafe_ptr_of_cursor : cursor -> nativeint
(**/**)

(** SDL_Cursor *)

module System_cursor : sig
  type t
  val arrow : t
  val ibeam : t
  val wait : t
  val crosshair : t
  val waitarrow : t
  val size_nw_se : t
  val size_ne_sw : t
  val size_we : t
  val size_ns : t
  val size_all : t
  val no : t
  val hand : t
end

module Button : sig
  val left : int
  val middle : int
  val right : int
  val x1 : int
  val x2 : int

  val lmask : uint32
  val mmask : uint32
  val rmask : uint32
  val x1mask : uint32
  val x2mask : uint32
end

val create_color_cursor : surface -> hot_x:int -> hot_y:int -> cursor result
(** {{:http://wiki.libsdl.org/SDL_CreateColorCursor}SDL_CreateColorCursor} *)

val create_cursor : (int, Bigarray.int8_unsigned_elt) bigarray ->
  (int, Bigarray.int8_unsigned_elt) bigarray -> w:int -> h:int -> hot_x:int ->
  hot_y:int -> cursor result
(** {{:http://wiki.libsdl.org/SDL_CreateCursor}SDL_CreateCursor} *)

val create_system_cursor : System_cursor.t -> cursor result
(** {{:https://wiki.libsdl.org/SDL_CreateSystemCursor}SDL_CreateSystemCursor} *)

val free_cursor : cursor -> unit
(** {{:http://wiki.libsdl.org/SDL_FreeCursor}SDL_FreeCursor} *)

val get_cursor : unit -> cursor option
(** {{:http://wiki.libsdl.org/SDL_GetCursor}SDL_GetCursor} *)

val get_default_cursor : unit -> cursor option
(** {{:http://wiki.libsdl.org/SDL_GetDefaultCursor}SDL_GetDefaultCursor} *)

val get_mouse_focus : unit -> window option
(** {{:http://wiki.libsdl.org/SDL_GetMouseFocus}SDL_GetMouseFocus} *)

val get_mouse_state : unit -> uint32 * (int * int)
(** {{:http://wiki.libsdl.org/SDL_GetMouseState}SDL_GetMouseState} *)

val get_relative_mouse_mode : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_GetRelativeMouseMode}
    SDL_GetRelativeMouseMode} *)

val get_relative_mouse_state : unit -> uint32 * (int * int)
(** {{:http://wiki.libsdl.org/SDL_GetRelativeMouseState}
    SDL_GetRelativeMouseState} *)

val get_cursor_shown : unit -> bool result
(** {{:http://wiki.libsdl.org/SDL_ShowCursor}SDL_ShowCursor} with
    SDL_QUERY. *)

val set_cursor : cursor option -> unit
(** {{:http://wiki.libsdl.org/SDL_SetCursor}SDL_SetCursor} *)

val set_relative_mouse_mode : bool -> unit result
(** {{:http://wiki.libsdl.org/SDL_SetRelativeMouseMode}
    SDL_SetRelativeMouseMode} *)

val show_cursor : bool -> bool result
(** {{:http://wiki.libsdl.org/SDL_ShowCursor}SDL_ShowCursor}. See also
    {!get_cursor_shown}. *)

val warp_mouse_in_window : window option -> x:int -> y:int -> unit
(** {{:http://wiki.libsdl.org/SDL_WarpMouseInWindow}SDL_WarpMouseInWindow} *)

(** {2:touch Touch and gestures} *)

type touch_id = int64
(** SDL_TouchID *)

val touch_mouse_id : touch_id
(** SDL_TOUCH_MOUSEID *)

type gesture_id = int64
(** SDL_GestureID *)

type finger_id = int64
(** SDL_FingerID *)

type finger
(** SDL_Finger *)

module Finger : sig
  val id : finger -> finger_id
  val x : finger -> float
  val y : finger -> float
  val pressure : finger -> float
end

val get_num_touch_devices : unit -> int
(** {{:https://wiki.libsdl.org/SDL_GetNumTouchDevices}SDL_GetNumTouchDevices}.*)

val get_num_touch_fingers : touch_id -> int
(** {{:https://wiki.libsdl.org/SDL_GetNumTouchFingers}SDL_GetNumTouchFingers}.*)

val get_touch_device : int -> touch_id result
(** {{:https://wiki.libsdl.org/SDL_GetTouchDevice}SDL_GetTouchDevice}.*)

val get_touch_finger : touch_id -> int -> finger option
(** {{:https://wiki.libsdl.org/SDL_GetTouchFinger}SDL_GetTouchFinger}.*)

val load_dollar_templates : touch_id -> rw_ops -> unit result
(** {{:https://wiki.libsdl.org/SDL_LoadDollarTemplates}
    SDL_LoadDollarTemplates} *)

val record_gesture : touch_id -> unit result
(** {{:https://wiki.libsdl.org/SDL_RecordGesture}SDL_RecordGesture}.*)

val save_dollar_template : gesture_id -> rw_ops -> unit result
(** {{:https://wiki.libsdl.org/SDL_SaveDollarTemplate}SDL_SaveDollarTemplate}.*)

val save_all_dollar_templates : rw_ops -> unit result
(** {{:https://wiki.libsdl.org/SDL_SaveAllDollarTemplate}
    SDL_SaveAllDollarTemplate}.*)

(** {2:joystick {{:http://wiki.libsdl.org/CategoryJoystick}Joystick}} *)

type joystick_guid
(** SDL_JoystickGUID. *)

type joystick_id = int32
(** SDL_JoystickID *)

type joystick

(**/**)
val unsafe_joystick_of_ptr : nativeint -> joystick
val unsafe_ptr_of_joystick : joystick -> nativeint
(**/**)

(** SDL_Joystick *)

module Hat : sig
  type t = int
  val centered : int
  val up : int
  val right : int
  val down : int
  val left : int
  val rightup : int
  val rightdown : int
  val leftup : int
  val leftdown : int
end

val joystick_close : joystick -> unit
(** {{:http://wiki.libsdl.org/SDL_JoystickClose}SDL_JoystickClose} *)

val joystick_get_event_state : unit -> toggle_state result
(** {{:http://wiki.libsdl.org/SDL_JoystickEventState}
    SDL_JoystickEventState} with SDL_QUERY. *)

val joystick_set_event_state : toggle_state -> toggle_state result
(** {{:http://wiki.libsdl.org/SDL_JoystickEventState}
    SDL_JoystickEventState}. See also {!joystick_get_event_state}. *)

val joystick_get_attached : joystick -> bool
(** {{:https://wiki.libsdl.org/SDL_JoystickGetAttached}
    SDL_JoystickGetAttached} *)

val joystick_get_axis : joystick -> int -> int16
(** {{:http://wiki.libsdl.org/SDL_JoystickGetAxis}SDL_JoystickGetAxis} *)

val joystick_get_ball : joystick -> int -> (int * int) result
(** {{:http://wiki.libsdl.org/SDL_JoystickGetBall}SDL_JoystickGetBall} *)

val joystick_get_button : joystick -> int -> uint8
(** {{:http://wiki.libsdl.org/SDL_JoystickGetButton}SDL_JoystickGetButton} *)

val joystick_get_device_guid : int -> joystick_guid
(** {{:http://wiki.libsdl.org/SDL_JoystickGetDeviceGUID}
    SDL_JoystickGetDeviceGUID} *)

val joystick_get_guid : joystick -> joystick_guid
(** {{:http://wiki.libsdl.org/SDL_JoystickGetGUID}SDL_JoystickGetGUID} *)

val joystick_get_guid_from_string : string -> joystick_guid
(** {{:http://wiki.libsdl.org/SDL_JoystickGetGUIDFromString}
    SDL_JoystickGetGUIDFromString} *)

val joystick_get_guid_string : joystick_guid -> string
(** {{:http://wiki.libsdl.org/SDL_JoystickGetGUIDString}
    SDL_JoystickGetGUIDString} *)

val joystick_get_hat : joystick -> int -> Hat.t
(** {{:http://wiki.libsdl.org/SDL_JoystickGetHat}SDL_JoystickGetHat} *)

val joystick_instance_id : joystick -> joystick_id result
(** {{:http://wiki.libsdl.org/SDL_JoystickInstanceID}SDL_JoystickInstanceID} *)

val joystick_name : joystick -> string result
(** {{:http://wiki.libsdl.org/SDL_JoystickName}SDL_JoystickName} *)

val joystick_name_for_index : int -> string result
(** {{:http://wiki.libsdl.org/SDL_JoystickNameForIndex}
    SDL_JoystickNameForIndex} *)

val joystick_num_axes : joystick -> int result
(** {{:http://wiki.libsdl.org/SDL_JoystickNumAxes}SDL_JoystickNumAxes} *)

val joystick_num_balls : joystick -> int result
(** {{:http://wiki.libsdl.org/SDL_JoystickNumBalls}SDL_JoystickNumBalls} *)

val joystick_num_buttons : joystick -> int result
(** {{:http://wiki.libsdl.org/SDL_JoystickNumButtons}SDL_JoystickNumButtons} *)

val joystick_num_hats : joystick -> int result
(** {{:http://wiki.libsdl.org/SDL_JoystickNumHats}SDL_JoystickNumHats} *)

val joystick_open : int -> joystick result
(** {{:http://wiki.libsdl.org/SDL_JoystickOpen}SDL_JoystickOpen} *)

val joystick_update : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_JoystickUpdate}SDL_JoystickUpdate} *)

val num_joysticks : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_NumJoysticks}SDL_NumJoysticks} *)

(** {2:gamecontroller
  {{:http://wiki.libsdl.org/CategoryGameController}Game controller}} *)

type game_controller

(**/**)
val unsafe_game_controller_of_ptr : nativeint -> game_controller
val unsafe_ptr_of_game_controller : game_controller -> nativeint
(**/**)

(** SDL_GameController *)

module Controller : sig
  type bind_type
  val bind_type_none : bind_type
  val bind_type_button : bind_type
  val bind_type_axis : bind_type
  val bind_type_hat : bind_type

  type axis
  val axis_invalid : axis
  val axis_left_x : axis
  val axis_left_y : axis
  val axis_right_x : axis
  val axis_right_y : axis
  val axis_trigger_left : axis
  val axis_trigger_right : axis
  val axis_max : axis

  type button
  val button_invalid : button
  val button_a : button
  val button_b : button
  val button_x : button
  val button_y : button
  val button_back : button
  val button_guide : button
  val button_start : button
  val button_left_stick : button
  val button_right_stick : button
  val button_left_shoulder : button
  val button_right_shoulder : button
  val button_dpad_up : button
  val button_dpad_down : button
  val button_dpad_left : button
  val button_dpad_right : button
  val button_max : button

  type button_bind
  (** SDL_GameControllerButtonBind *)
  val bind_type : button_bind -> bind_type
  val bind_button_value : button_bind -> int
  val bind_axis_value : button_bind -> int
  val bind_hat_value : button_bind -> int * int
end

val game_controller_add_mapping : string -> bool result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerAddMapping}
     SDL_GameControllerAddMapping} *)

val game_controller_add_mapping_from_file : string -> int result
(** {{:https://wiki.libsdl.org/SDL_GameControllerAddMappingsFromFile}
    SDL_GameControllerAddMappingsFromFile} (SDL 2.0.2). *)

val game_controller_add_mapping_from_rw : rw_ops -> bool -> int result
(** {{:https://wiki.libsdl.org/SDL_GameControllerAddMappingsFromRW}
    SDL_GameControllerAddMappingsFromFile} (SDL 2.0.2). *)

val game_controller_close : game_controller -> unit
(**  {{:http://wiki.libsdl.org/SDL_GameControllerClose}
     SDL_GameControllerClose} *)

val game_controller_get_event_state : unit -> toggle_state result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerEventState}
     SDL_GameControllerEventState} with SDL_QUERY *)

val game_controller_set_event_state : toggle_state -> toggle_state result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerEventState}
     SDL_GameControllerEventState}.
     See also {!game_controller_get_event_state}. *)

val game_controller_get_attached : game_controller -> bool
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetAttached}
     SDL_GameControllerGetAttached} *)

val game_controller_get_axis : game_controller -> Controller.axis -> int16
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetAxis}
     SDL_GameControllerGetAxis} *)

val game_controller_get_axis_from_string : string -> Controller.axis
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetAxisFromString}
     SDL_GameControllerGetAxisFromString} *)

val game_controller_get_bind_for_axis : game_controller -> Controller.axis ->
  Controller.button_bind
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetBindForAxis}
     SDL_GameControllerGetBindForAxis} *)

val game_controller_get_bind_for_button : game_controller ->
  Controller.button -> Controller.button_bind
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetBindForButton}
     SDL_GameControllerGetBindForButton} *)

val game_controller_get_button : game_controller -> Controller.button -> uint8
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetButton}
     SDL_GameControllerGetButton} *)

val game_controller_get_button_from_string : string -> Controller.button
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetButtonFromString}
     SDL_GameControllerGetButtonFromString} *)

val game_controller_get_joystick : game_controller -> joystick result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetJoystick}
     SDL_GameControllerGetJoystick} *)

val game_controller_get_string_for_axis : Controller.axis -> string option
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetStringForAxis}
     SDL_GameControllerGetStringForAxis} *)

val game_controller_get_string_for_button : Controller.button -> string option
(**  {{:http://wiki.libsdl.org/SDL_GameControllerGetStringForButton}
     SDL_GameControllerGetStringForButton} *)

val game_controller_mapping : game_controller -> string result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerMapping}
     SDL_GameControllerMapping} *)

val game_controller_mapping_for_guid : joystick_guid -> string result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerMappingForGUID}
     SDL_GameControllerMappingForGUID} *)

val game_controller_name : game_controller -> string result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerName}SDL_GameControllerName} *)

val game_controller_name_for_index : int -> string result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerNameForIndex}
     SDL_GameControllerNameForIndex} *)

val game_controller_open : int -> game_controller result
(**  {{:http://wiki.libsdl.org/SDL_GameControllerOpen}
     SDL_GameControllerOpen} *)

val game_controller_update : unit -> unit
(**  {{:http://wiki.libsdl.org/SDL_GameControllerUpdate}
     SDL_GameControllerUpdate} *)

val is_game_controller : int -> bool
(**  {{:http://wiki.libsdl.org/SDL_IsGameController}SDL_IsGameController} *)

(** {2:events {{:http://wiki.libsdl.org/CategoryEvents}Events}} *)

type event_type = int
(** {{:http://wiki.libsdl.org/SDL_EventType}SDL_EventType}.
    See {!Event} for constants. *)

type event
(** {{:http://wiki.libsdl.org/SDL_Event}SDL_Event} *)

(** {!event} accessors and {!event_type} constants and {{!enum}enumeration}. *)
module Event : sig

  (** {1:event Event}

    Once you have determined the {!typ} you can access fields
    available for that type. Safe if you use the wrong accessors:
    you will just end with garbage data.  *)

  type 'b field
  (** The type for event fields. *)

  val create : unit -> event
  (** [create ()] is an uninitialized event structure. *)

  val get : event -> 'b field -> 'b
  (** [get e f] gets the field [f] of [e]. *)

  val set : event -> 'b field -> 'b -> unit
  (** [set e f v] sets the field [f] of [e] to [v]. *)

  (** {1 Event types and their fields}

      {ul
      {- {!common}}
      {- {!application}}
      {- {!clipboard}}
      {- {!controller}}
      {- {!dollar}}
      {- {!drop}}
      {- {!touch}}
      {- {!joystickev}}
      {- {!keyboard}}
      {- {!mouse}}
      {- {!multigestureev}}
      {- {!quitev}}
      {- {!syswm}}
      {- {!text}}
      {- {!window}}
      {- {!render_target}}
      {- {!audio}}} *)

  (** {2 Event type aliases and misc} *)

  val first_event : event_type
  val last_event : event_type

  (** {2:common Common}

      These fields are common to all event types.  *)

  val typ : event_type field
  val timestamp : uint32 field

  (** {2:application Application events} *)

  val app_did_enter_background : event_type
  val app_did_enter_foreground : event_type
  val app_low_memory : event_type
  val app_terminating : event_type
  val app_will_enter_background : event_type
  val app_will_enter_foreground : event_type

  (** {2:clipboard Clipboard} *)

  val clipboard_update : event_type

  (** {2:controller Controller events} *)

  val controller_axis_motion : event_type
  val controller_button_down : event_type
  val controller_button_up : event_type
  val controller_device_added : event_type
  val controller_device_remapped : event_type
  val controller_device_removed : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_ControllerAxisEvent}
      SDL_ControllerAxisEvent} fields} *)

  val controller_axis_which : joystick_id field
  val controller_axis_axis : uint8 field
  val controller_axis_value : int16 field

  (** {3 {{:http://wiki.libsdl.org/SDL_ControllerButtonEvent}
      SDL_ControllerButtonEvent} fields} *)

  val controller_button_which : joystick_id field
  val controller_button_button : uint8 field
  val controller_button_state : button_state field

  (** {3 {{:http://wiki.libsdl.org/SDL_ControllerDeviceEvent}
      SDL_ControllerDeviceEvent} fields} *)

  val controller_device_which : joystick_id field

  (** {2:dollar Dollar gesture events} *)

  val dollar_gesture : event_type
  val dollar_record : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_DollarGestureEvent}
      SDL_DollarGestureEvent} fields} *)

  val dollar_gesture_touch_id : touch_id field
  val dollar_gesture_gesture_id : gesture_id field
  val dollar_gesture_num_fingers : int field
  val dollar_gesture_error : float field
  val dollar_gesture_x : float field
  val dollar_gesture_y : float field

  (** {2:drop Drop events}

      {b Warning} If you enable this event {!drop_file_free} must be
      called on the event after you have finished processing it. *)

  val drop_file : event_type
  val drop_text : event_type
  val drop_begin : event_type
  val drop_complete : event_type

  val drop_file_free : event -> unit

  (** {3 {{:http://wiki.libsdl.org/SDL_DropEvent}SDL_DropEvent}
      fields} *)

  val drop_file_file : event -> string

  (** {2:touch Touch events} *)

  val finger_down : event_type
  val finger_motion : event_type
  val finger_up : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_TouchFingerEvent}SDL_TouchFingerEvent}
      fields} *)

  val touch_finger_touch_id : touch_id field
  val touch_finger_finger_id : finger_id field
  val touch_finger_x : float field
  val touch_finger_y : float field
  val touch_finger_dx : float field
  val touch_finger_dy : float field
  val touch_finger_pressure : float field

  (** {2:joystickev Joystick events} *)

  val joy_axis_motion : event_type
  val joy_ball_motion : event_type
  val joy_button_down : event_type
  val joy_button_up : event_type
  val joy_device_added : event_type
  val joy_device_removed : event_type
  val joy_hat_motion : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_JoyAxisEvent}SDL_JoyAxisEvent}
      fields} *)

  val joy_axis_which : joystick_id field
  val joy_axis_axis : uint8 field
  val joy_axis_value : int16 field

  (** {3 {{:http://wiki.libsdl.org/SDL_JoyBallEvent}SDL_JoyBallEvent}
      fields} *)

  val joy_ball_which : joystick_id field
  val joy_ball_ball : uint8 field
  val joy_ball_xrel : int field
  val joy_ball_yrel : int field

  (** {3 {{:http://wiki.libsdl.org/SDL_JoyButtonEvent}SDL_JoyButtonEvent}
      fields} *)

  val joy_button_which : joystick_id field
  val joy_button_button : uint8 field
  val joy_button_state : button_state field

  (** {3 {{:http://wiki.libsdl.org/SDL_JoyDeviceEvent}SDL_JoyDeviceEvent}
      fields} *)

  val joy_device_which : joystick_id field

  (** {3 {{:http://wiki.libsdl.org/SDL_JoyHatEvent}SDL_JoyHatEvent}
      fields} *)

  val joy_hat_which : joystick_id field
  val joy_hat_hat : uint8 field
  val joy_hat_value : Hat.t field

  (** {2:keyboard Keyboard event} *)

  val key_down : event_type
  val key_up : event_type
  val keymap_changed : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_KeyboardEvent}SDL_KeyboardEvent}
      fields} *)

  val keyboard_window_id : int field
  val keyboard_state : button_state field
  val keyboard_repeat : int field
  val keyboard_scancode : scancode field
  val keyboard_keycode : keycode field
  val keyboard_keymod : keymod field

  (** {2:mouse Mouse events} *)

  val mouse_button_down : event_type
  val mouse_button_up : event_type
  val mouse_motion : event_type
  val mouse_wheel : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_MouseButtonEvent}SDL_MouseButtonEvent}
      fields} *)

  val mouse_button_window_id : int field
  val mouse_button_which : uint32 field
  val mouse_button_button : uint8 field
  val mouse_button_state : button_state field
  val mouse_button_clicks : uint8 field (** SDL 2.0.2 *)
  val mouse_button_x : int field
  val mouse_button_y : int field

  (** {3 {{:http://wiki.libsdl.org/SDL_MouseMotionEvent}SDL_MouseMotionEvent}
      fields} *)

  val mouse_motion_window_id : int field
  val mouse_motion_which : uint32 field
  val mouse_motion_state : uint32 field
  val mouse_motion_x : int field
  val mouse_motion_y : int field
  val mouse_motion_xrel : int field
  val mouse_motion_yrel : int field

  (** {3 {{:http://wiki.libsdl.org/SDL_MouseWheelEvent}SDL_MouseWheelEvent}
      fields} *)

  val mouse_wheel_window_id : int field
  val mouse_wheel_which : uint32 field
  val mouse_wheel_x : int field
  val mouse_wheel_y : int field

  (** {2:multigestureev Multi gesture events} *)

  val multi_gesture : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_MultiGestureEvent}SDL_MultiGestureEvent}
      fields} *)

  val multi_gesture_touch_id : touch_id field
  val multi_gesture_dtheta : float field
  val multi_gesture_ddist : float field
  val multi_gesture_x : float field
  val multi_gesture_y : float field
  val multi_gesture_num_fingers : int field

  (** {2:quitev Quit events} *)

  val quit : event_type

  (** {2:syswm System window manager events} *)

  val sys_wm_event : event_type

  (** {2:text Text events} *)

  val text_editing : event_type
  val text_input : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_TextEditingEvent}SDL_TextEditingEvent}
      fields}  *)

  val text_editing_window_id : int field
  val text_editing_text : string field
  val text_editing_start : int field
  val text_editing_length : int field

  (** {3 {{:http://wiki.libsdl.org/SDL_TextInputEvent}SDL_TextInputEvent}
      fields} *)

  val text_input_window_id : int field
  val text_input_text : string field

  (** {2:user User events} *)

  val user_event : event_type

  (** {3 {{:http://wiki.libsdl.org/SDL_UserEvent}SDL_UserEvent} fields} *)

  val user_window_id : int field
  val user_code : int field

  (** {2:window Window events} *)

  val window_event : event_type

  type window_event_id = int
  (** {{:https://wiki.libsdl.org/SDL_WindowEventID}SDL_WindowEventID} *)

  val window_event_enum : window_event_id ->
    [ `Close | `Enter | `Exposed | `Focus_gained | `Focus_lost | `Hidden
    | `Hit_test | `Leave | `Maximized | `Minimized | `Moved | `Resized
    | `Restored | `Shown | `Size_changed | `Take_focus
    | `Unknown of window_event_id ]

  val window_event_shown : window_event_id
  val window_event_hidden : window_event_id
  val window_event_exposed : window_event_id
  val window_event_moved : window_event_id
  val window_event_resized : window_event_id
  val window_event_size_changed : window_event_id
  val window_event_minimized : window_event_id
  val window_event_maximized : window_event_id
  val window_event_restored : window_event_id
  val window_event_enter : window_event_id
  val window_event_leave : window_event_id
  val window_event_focus_gained : window_event_id
  val window_event_focus_lost : window_event_id
  val window_event_close : window_event_id
  val window_event_take_focus : window_event_id
  val window_event_hit_test : window_event_id

  (** {3 {{:http://wiki.libsdl.org/SDL_WindowEvent}SDL_WindowEvent} fields} *)

  val window_window_id : int field
  val window_event_id : window_event_id field
  val window_data1 : int32 field
  val window_data2 : int32 field

  (** {2:render Render target} *)

  val render_targets_reset : event_type
  val render_device_reset : event_type

  (** {2:audio Audio hotplug events} *)

  val audio_device_added : event_type
  val audio_device_removed : event_type

  (** {3 {{:https://wiki.libsdl.org/SDL_AudioDeviceEvent}SDL_AudioDeviceEvent}
      fields} *)

  val audio_device_timestamp : uint32 field
  val audio_device_which : uint32 field
  val audio_device_is_capture : int field

  (** {1:enum Event type enum} *)

  val enum : event_type ->
    [ `App_did_enter_background | `App_did_enter_foreground
    | `App_low_memory | `App_terminating | `App_will_enter_background
    | `App_will_enter_foreground | `Clipboard_update
    | `Controller_axis_motion | `Controller_button_down
    | `Controller_button_up | `Controller_device_added
    | `Controller_device_remapped | `Controller_device_removed
    | `Dollar_gesture | `Dollar_record | `Drop_file | `Finger_down
    | `Finger_motion | `Finger_up | `Joy_axis_motion | `Joy_ball_motion
    | `Joy_button_down | `Joy_button_up | `Joy_device_added
    | `Joy_device_removed | `Joy_hat_motion | `Key_down | `Key_up
    | `Mouse_button_down | `Mouse_button_up | `Mouse_motion
    | `Mouse_wheel | `Multi_gesture | `Quit | `Sys_wm_event
    | `Text_editing | `Text_input | `Unknown of int | `User_event
    | `Window_event ]
end

val get_event_state : event_type -> toggle_state
(** {{:http://wiki.libsdl.org/SDL_EventState}SDL_EventState}
    with SDL_QUERY. *)

val set_event_state : event_type -> toggle_state -> unit
(** {{:http://wiki.libsdl.org/SDL_EventState}SDL_EventState}.
    See also {!get_event_state}.  *)

val flush_event : event_type -> unit
(** {{:http://wiki.libsdl.org/SDL_FlushEvent}SDL_FlushEvent} *)

val flush_events : event_type -> event_type -> unit
(** {{:http://wiki.libsdl.org/SDL_FlushEvents}SDL_FlushEvents} *)

val has_event : event_type -> bool
(** {{:http://wiki.libsdl.org/SDL_HasEvent}SDL_HasEvent} *)

val has_events : event_type -> event_type -> bool
(** {{:http://wiki.libsdl.org/SDL_HasEvents}SDL_HasEvents} *)

val poll_event : event option -> bool
(** {{:http://wiki.libsdl.org/SDL_PollEvent}SDL_PollEvent} *)

val pump_events : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_PumpEvents}SDL_PumpEvents} *)

val push_event : event -> bool result
(** {{:http://wiki.libsdl.org/SDL_PushEvent}SDL_PushEvent} *)

val register_event : unit -> event_type option
(** {{:http://wiki.libsdl.org/SDL_RegisterEvents}SDL_RegisterEvents}
    called with [1]. *)

val wait_event : event option -> unit result
(** {{:http://wiki.libsdl.org/SDL_WaitEvent}SDL_WaitEvent} *)

val wait_event_timeout : event option -> int -> bool
(** {{:http://wiki.libsdl.org/SDL_WaitEventTimeout}SDL_WaitEventTimeout} *)

(** {1:forcefeedback
    {{:http://wiki.libsdl.org/CategoryForceFeedback}Force Feedback}} *)

type haptic
type haptic_effect
type haptic_effect_id

module Haptic : sig

  val infinity : uint32

  (** {1 Features} *)

  type feature = int
  val gain : feature
  val autocenter : feature
  val status : feature
  val pause : feature

  (** {1 Directions} *)

  type direction_type = int
  val polar : direction_type
  val cartesian : direction_type
  val spherical : direction_type

  module Direction : sig
    type t
    val create : int -> int32 -> int32 -> int32 -> t
    val typ : t -> direction_type
    val dir_0 : t -> int32
    val dir_1 : t -> int32
    val dir_2 : t -> int32
  end

  (** {1 Effects} *)

  type effect_type = int

  type 'a field
  (** The type for effect fields. *)

  val create_effect : unit -> haptic_effect
  (** [create_effect ()] is an uninitialized haptic effect *)

  val get : haptic_effect -> 'a field -> 'a
  (** [get e f] gets the field f of [e]. *)

  val set : haptic_effect -> 'a field -> 'a -> unit
  (** [set e f v] sets the field f of [e] to [v]. *)

  val typ : effect_type field

  (** {2 Constant effect} *)

  val constant : effect_type

  (** {3 {{:http://wiki.libsdl.org/SDL_HapticConstant}
      SDL_HapticConstant} fields} *)

  val constant_type : effect_type field
  val constant_direction : Direction.t field
  val constant_length : uint32 field
  val constant_delay : uint16 field
  val constant_button : uint16 field
  val constant_interval : uint16 field
  val constant_level : int16 field
  val constant_attack_length : uint16 field
  val constant_attack_level : uint16 field
  val constant_fade_length : uint16 field
  val constant_fade_level : uint16 field

  (** {2 Periodic effect} *)

  val sine : effect_type
  val left_right : effect_type
  val triangle : effect_type
  val sawtooth_up : effect_type
  val sawtooth_down : effect_type

  (** {3 {{:http://wiki.libsdl.org/SDL_HapticPeriodic}
      SDL_HapticPeriodic} fields} *)

  val periodic_type : effect_type field
  val periodic_direction : Direction.t field
  val periodic_length : uint32 field
  val periodic_delay : uint16 field
  val periodic_button : uint16 field
  val periodic_interval : uint16 field
  val periodic_period : uint16 field
  val periodic_magnitude : int16 field
  val periodic_offset : int16 field
  val periodic_phase : uint16 field
  val periodic_attack_length : uint16 field
  val periodic_attack_level : uint16 field
  val periodic_fade_length : uint16 field
  val periodic_fade_level : uint16 field

  (** {2 Condition effect} *)

  val spring : effect_type
  val damper : effect_type
  val inertia : effect_type
  val friction : effect_type

  (** {3 {{:http://wiki.libsdl.org/SDL_HapticCondition}
      SDL_HapticCondition} fields} *)

  val condition_type : effect_type field
  val condition_direction : Direction.t field
  val condition_length : uint32 field
  val condition_delay : uint16 field
  val condition_button : uint16 field
  val condition_interval : uint16 field
  val condition_right_sat_0 : uint16 field
  val condition_right_sat_1 : uint16 field
  val condition_right_sat_2 : uint16 field
  val condition_left_sat_0 : uint16 field
  val condition_left_sat_1 : uint16 field
  val condition_left_sat_2 : uint16 field
  val condition_right_coeff_0 : int16 field
  val condition_right_coeff_1 : int16 field
  val condition_right_coeff_2 : int16 field
  val condition_left_coeff_0 : int16 field
  val condition_left_coeff_1 : int16 field
  val condition_left_coeff_2 : int16 field
  val condition_deadband_0 : uint16 field
  val condition_deadband_1 : uint16 field
  val condition_deadband_2 : uint16 field
  val condition_center_0 : int16 field
  val condition_center_1 : int16 field
  val condition_center_2 : int16 field

  (** {2 Ramp effect} *)

  val ramp : effect_type

  (** {3 {{:http://wiki.libsdl.org/SDL_HapticRamp}SDL_HapticRamp} fields} *)

  val ramp_type : effect_type field
  val ramp_direction : Direction.t field
  val ramp_length : uint32 field
  val ramp_delay : uint16 field
  val ramp_button : uint16 field
  val ramp_interval : uint16 field
  val ramp_start : int16 field
  val ramp_end : int16 field
  val ramp_attack_length : uint16 field
  val ramp_attack_level : uint16 field
  val ramp_fade_length : uint16 field
  val ramp_fade_level : uint16 field

  (** {2 Left right effect}

      For {!left_right}. *)

  (** {3 {{:http://wiki.libsdl.org/SDL_HapticLeftRight}SDL_HapticLeftRight}
      fields} *)

  val left_right_type : effect_type field
  val left_right_length : uint32 field
  val left_right_large_magnitude : uint16 field
  val left_right_small_magnitude : uint16 field

  (** {2 Custom effect} *)

  val custom : effect_type

  (** {3 {{:http://wiki.libsdl.org/SDL_HapticCustom}SDL_HapticCustom} fields} *)

  val custom_type : effect_type field
  val custom_direction : Direction.t field
  val custom_length : uint32 field
  val custom_delay : uint16 field
  val custom_button : uint16 field
  val custom_interval : uint16 field
  val custom_channels : uint8 field
  val custom_period : uint16 field
  val custom_samples : uint16 field
  val custom_data : uint16 list field
  (** {b Note.} Only {!set}able. *)
  val custom_attack_length : uint16 field
  val custom_attack_level : uint16 field
  val custom_fade_length : uint16 field
  val custom_fade_level : uint16 field
end

val haptic_close : haptic -> unit
(** {{:http://wiki.libsdl.org/SDL_HapticClose}SDL_HapticClose} *)

val haptic_destroy_effect : haptic -> haptic_effect_id -> unit
(** {{:http://wiki.libsdl.org/SDL_HapticDestroyEffect}
    SDL_HapticDestroyEffect} *)

val haptic_effect_supported : haptic -> haptic_effect -> bool result
(** {{:http://wiki.libsdl.org/SDL_HapticEffectSupported}
    SDL_HapticEffectSupported} *)

val haptic_get_effect_status : haptic -> haptic_effect_id -> bool result
(** {{:http://wiki.libsdl.org/SDL_HapticGetEffectStatus}
    SDL_HapticGetEffectStatus} *)

val haptic_index : haptic -> int result
(** {{:http://wiki.libsdl.org/SDL_HapticIndex}SDL_HapticIndex} *)

val haptic_name : int -> string result
(** {{:http://wiki.libsdl.org/SDL_HapticName}SDL_HapticName} *)

val haptic_new_effect : haptic -> haptic_effect -> haptic_effect_id result
(** {{:http://wiki.libsdl.org/SDL_HapticNewEffect}SDL_HapticNewEffect} *)

val haptic_num_axes : haptic -> int result
(** {{:http://wiki.libsdl.org/SDL_HapticNumAxes}SDL_HapticNumAxes} *)

val haptic_num_effects : haptic -> int result
(** {{:http://wiki.libsdl.org/SDL_HapticNumEffects}SDL_HapticNumEffects} *)

val haptic_num_effects_playing : haptic -> int result
(** {{:http://wiki.libsdl.org/SDL_HapticNumEffectsPlaying}
    SDL_HapticNumEffectsPlaying} *)

val haptic_open : int -> haptic result
(** {{:http://wiki.libsdl.org/SDL_HapticOpen}SDL_HapticOpen} *)

val haptic_open_from_joystick : joystick -> haptic result
(** {{:http://wiki.libsdl.org/SDL_HapticOpenFromJoystick}
    SDL_HapticOpenFromJoystick} *)

val haptic_open_from_mouse : unit -> haptic result
(** {{:http://wiki.libsdl.org/SDL_HapticOpenFromMouse}
    SDL_HapticOpenFromMouse} *)

val haptic_opened : int -> bool
(** {{:http://wiki.libsdl.org/SDL_HapticOpened}SDL_HapticOpened} *)

val haptic_pause : haptic -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticPause}SDL_HapticPause} *)

val haptic_query : haptic -> int
(** {{:http://wiki.libsdl.org/SDL_HapticQuery}SDL_HapticQuery} *)

val haptic_rumble_init : haptic -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticRumbleInit}SDL_HapticRumbleInit} *)

val haptic_rumble_play : haptic -> float -> uint32 -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticRumblePlay}SDL_HapticRumblePlay} *)

val haptic_rumble_stop : haptic -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticRumbleStop}SDL_HapticRumbleStop} *)

val haptic_rumble_supported : haptic -> bool result
(** {{:http://wiki.libsdl.org/SDL_HapticRumbleSupported}
    SDL_HapticRumbleSupported} *)

val haptic_run_effect : haptic -> haptic_effect_id -> uint32 ->
  unit result
(** {{:http://wiki.libsdl.org/SDL_HapticRunEffect}SDL_HapticRunEffect} *)

val haptic_set_autocenter : haptic -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticSetAutocenter}
    SDL_HapticSetAutocenter} *)

val haptic_set_gain : haptic -> int -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticSetGain}SDL_HapticSetGain} *)

val haptic_stop_all : haptic -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticStopAll}SDL_HapticStopAll} *)

val haptic_stop_effect : haptic -> haptic_effect_id -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticStopEffect}SDL_HapticStopEffect} *)

val haptic_unpause : haptic -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticUnpause}SDL_HapticUnpause} *)

val haptic_update_effect :
  haptic -> haptic_effect_id -> haptic_effect -> unit result
(** {{:http://wiki.libsdl.org/SDL_HapticUpdateEffect}SDL_HapticUpdateEffect} *)

val joystick_is_haptic : joystick -> bool result
(** {{:http://wiki.libsdl.org/SDL_JoystickIsHaptic}SDL_JoystickIsHaptic} *)

val mouse_is_haptic : unit -> bool result
(** {{:http://wiki.libsdl.org/SDL_MouseIsHaptic}SDL_MouseIsHaptic} *)

val num_haptics : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_NumHaptics}SDL_NumHaptics} *)

(** {1:audio {{:http://wiki.libsdl.org/CategoryAudio}Audio}} *)

module Audio : sig

  (** {1:status Audio status} *)

  type status = int
  val stopped : status
  val playing : status
  val paused : status

  (** {1:format Audio format} *)

  type format = int
  (** {{:https://wiki.libsdl.org/SDL_AudioFormat}SDL_AudioFormat} *)

  val s8 : format
  val u8 : format
  val s16_lsb : format
  val s16_msb : format
  val s16_sys : format
  val s16 : format
  val s16_lsb : format
  val u16_lsb : format
  val u16_msb : format
  val u16_sys : format
  val u16 : format
  val u16_lsb : format
  val s32_lsb : format
  val s32_msb : format
  val s32_sys : format
  val s32 : format
  val s32_lsb : format
  val f32_lsb : format
  val f32_msb : format
  val f32_sys : format
  val f32 : format

  (** {1:format Audio allowed changes} *)

  type allow = int
  val allow_frequency_change : int
  val allow_format_change : int
  val allow_channels_change : int
  val allow_any_change : int
end

(** {2:audiodrivers Audio drivers} *)

val audio_init : string option -> unit result
(** {{:http://wiki.libsdl.org/SDL_AudioInit}
    SDL_AudioInit} *)

val audio_quit : unit -> unit
(** {{:http://wiki.libsdl.org/SDL_AudioQuit}
    SDL_AudioQuit} *)

val get_audio_driver : int -> string result
(** {{:http://wiki.libsdl.org/SDL_GetAudioDriver}
    SDL_GetAudioDriver} *)

val get_current_audio_driver : unit -> string option
(** {{:http://wiki.libsdl.org/SDL_GetCurrentAudioDriver}
    SDL_GetCurrentAudioDriver} *)

val get_num_audio_drivers : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_GetNumAudioDrivers}
    SDL_GetNumAudioDrivers} *)

(** {2:audiodevices Audio devices} *)

type audio_device_id = uint32

type audio_callback
(** The type for audio callbacks. *)

val audio_callback :
    ('a, 'b) Bigarray.kind -> (('a, 'b) bigarray -> unit) -> audio_callback
(** [audio_callback k f] is an audio callback. A reference needs to be kept
    on the callback value until it is no longer needed. *)

type audio_spec =
  { as_freq : int;
    as_format : Audio.format;
    as_channels : uint8;
    as_silence : uint8;
    as_samples : uint8;
    as_size : uint32;
    as_callback : audio_callback option; }
(** {{:http://wiki.libsdl.org/SDL_AudioSpec}SDL_AudioSpec} *)

val close_audio_device : audio_device_id -> unit
(** {{:http://wiki.libsdl.org/SDL_CloseAudioDevice}
    SDL_CloseAudioDevice} *)

val free_wav : ('a, 'b) bigarray -> unit
(** {{:https://wiki.libsdl.org/SDL_FreeWAV}SDL_FreeWAV}. *)

val get_audio_device_name : int -> bool -> string result
(** {{:http://wiki.libsdl.org/SDL_GetAudioDeviceName}
    SDL_GetAudioDeviceName} *)

val get_audio_device_status : audio_device_id -> Audio.status
(** {{:http://wiki.libsdl.org/SDL_GetAudioDeviceStatus}
    SDL_GetAudioDeviceStatus} *)

val get_num_audio_devices : bool -> int result
(** {{:http://wiki.libsdl.org/SDL_GetNumAudioDevices}
    SDL_GetNumAudioDevices} *)

val load_wav_rw : rw_ops -> audio_spec -> ('a, 'b) Bigarray.kind ->
  (audio_spec * ('a, 'b) bigarray) result
(** {{:https://wiki.libsdl.org/SDL_LoadWAV_RW}
    SDL_LoadWAV_RW}. *)

val lock_audio_device : audio_device_id -> unit
(** {{:http://wiki.libsdl.org/SDL_LockAudioDevice}
    SDL_LockAudioDevice} *)

val open_audio_device : string option -> bool -> audio_spec ->
  Audio.allow -> (audio_device_id * audio_spec) result
(** {{:http://wiki.libsdl.org/SDL_OpenAudioDevice}
    SDL_OpenAudioDevice} *)

val pause_audio_device : audio_device_id -> bool -> unit
(** {{:http://wiki.libsdl.org/SDL_PauseAudioDevice}
    SDL_PauseAudioDevice} *)

val unlock_audio_device : audio_device_id -> unit
(** {{:http://wiki.libsdl.org/SDL_UnlockAudioDevice}
    SDL_UnlockAudioDevice} *)

val queue_audio : audio_device_id -> ('a, 'b) bigarray -> unit result
(** {{:http://wiki.libsdl.org/SDL_QueueAudio}
    QueueAudio} *)

val dequeue_audio : audio_device_id -> ('a, 'b) bigarray -> int
(** {{:http://wiki.libsdl.org/SDL_DequeueAudio}
    DequeueAudio} *)

val get_queued_audio_size : audio_device_id -> int
(** {{:http://wiki.libsdl.org/SDL_GetQueuedAudioSize}
    GetQueuedAudioSize} *)

val clear_queued_audio : audio_device_id -> unit
(** {{:http://wiki.libsdl.org/SDL_ClearQueuedAudio}
    ClearQueuedAudio} *)

(*

(** {2:audioconvert Audio conversion} *)

type audio_cvt
(** {{:https://wiki.libsdl.org/SDL_AudioCVT}SDL_AudioCVT} *)

val audio_cvt_mult : audio_cvt -> int * float
(** [audio_cvt_mult cvt] is the [len_mult] and [len_ratio] fields of [cvt] *)

val build_audio_cvt : ~src:Audio.format -> uint8 -> uint8 ~dst:Audio.format ->
  uint8 -> uint8 -> audio_cvt option result
(** {{:http://wiki.libsdl.org/SDL_BuildAudioCVT}
    SDL_BuildAudioCVT}. [None] is returned if no conversion is needed. *)

val convert_audio : audio_cvt -> ('a, 'b) bigarray -> unit
(** {{:http://wiki.libsdl.org/SDL_ConvertAudio}
    SDL_ConvertAudio}. The bigarray has the source and destination *)
*)

(** {1:timer {{:http://wiki.libsdl.org/CategoryTimer}Timer}} *)

val delay : uint32 -> unit
(** {{:http://wiki.libsdl.org/SDL_Delay}SDL_Delay} *)

val get_ticks : unit -> uint32
(** {{:http://wiki.libsdl.org/SDL_GetTicks}SDL_GetTicks} *)

val get_performance_counter : unit -> uint64
(** {{:http://wiki.libsdl.org/SDL_GetPerformanceCounter}
    SDL_GetPerformanceCounter} *)

val get_performance_frequency : unit -> uint64
(** {{:http://wiki.libsdl.org/SDL_GetPerformanceFrequency}
    SDL_GetPerformanceFrequency} *)

(** {1:platform Platform and CPU information} *)

val get_platform : unit -> string
(** {{:http://wiki.libsdl.org/SDL_GetPlatform}SDL_GetPlatform} *)

val get_cpu_cache_line_size : unit -> int result
(** {{:http://wiki.libsdl.org/SDL_GetCPUCacheLineSize}
    SDL_GetCPUCacheLineSize} *)

val get_cpu_count : unit -> int
(** {{:http://wiki.libsdl.org/SDL_GetCPUCount}SDL_GetCPUCount} *)

val get_system_ram : unit -> int
(** {{:http://wiki.libsdl.org/SDL_GetSystemRAM}SDL_GetSystemRAM} *)

val has_3d_now : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_Has3DNow}SDL_Has3DNow} *)

val has_altivec : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasAltiVec}SDL_HasAltiVec} *)

val has_avx : unit -> bool
(** {{:https://wiki.libsdl.org/SDL_HasAVX}SDL_HasAVX} (SDL 2.0.2) *)

val has_mmx : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasMMX}SDL_HasMMX} *)

val has_rdtsc : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasRDTSC}SDL_HasRDTSC} *)

val has_sse : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasSSE}SDL_HasSSE} *)

val has_sse2 : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasSSE2}SDL_HasSSE2} *)

val has_sse3 : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasSSE3}SDL_HasSSE3} *)

val has_sse41 : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasSSE3}SDL_HasSSE41} *)

val has_sse42 : unit -> bool
(** {{:http://wiki.libsdl.org/SDL_HasSSE3}SDL_HasSSE42} *)

(** {1:power {{:http://wiki.libsdl.org/CategoryPower}Power}} *)

type power_state =
  [ `Unknown | `On_battery | `No_battery | `Charging | `Charged ]
(** {{:http://wiki.libsdl.org/SDL_PowerState}SDL_PowerState} *)

type power_info =
  { pi_state : power_state;
    pi_secs : int option;
    pi_pct : int option; }

val get_power_info : unit -> power_info
(** {{:http://wiki.libsdl.org/SDL_GetPowerInfo}SDL_GetPowerInfo} *)

(**     {1:coverage Binding Coverage}

    Everything except the following functions/categories are available.

    {2 Unbound categories}

    {ul
    {- {{:http://wiki.libsdl.org/CategoryAssertions}Assertions}
        (cpp based).}
    {- {{:https://wiki.libsdl.org/CategorySWM}Platform-specific Window
        Management} (not useful at the moment)}
    {- {{:http://wiki.libsdl.org/CategoryThread}Thread Management}
        (better use another OCaml API)}
    {- {{:http://wiki.libsdl.org/CategoryMutex}Thread Synchronization
        Primitives} (better use another OCaml API)}
    {- {{:http://wiki.libsdl.org/CategoryAtomic}Atomic Operations}
        (mostly cpp based)}
    {- {{:http://wiki.libsdl.org/CategoryIO}File I/O Abstraction}
        (only the minimum was covered for other parts of the API that needs
        it, better use another OCaml API)}
    {- {{:http://wiki.libsdl.org/CategorySharedObject}
       Shared Object Loading and Function Lookup} (use ocaml-ctypes)}
    {- {{:http://wiki.libsdl.org/CategoryEndian}Byte Order and Byte Swapping}
        (cpp based)}
    {- {{:http://wiki.libsdl.org/CategoryBits}Bit Manipulation}
        (cpp based)}}

    {2 Unbound functions}

    {ul
    {- {{:https://wiki.libsdl.org/SDL_AddHintCallback}SDL_AddHintCallback}
        (avoid callbacks from C to OCaml)}
    {- {{:https://wiki.libsdl.org/SDL_DelHintCallback}SDL_DelHintCallback}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_LogGetOutputFunction}
       SDL_LogGetOutputFunction} (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_LogSetOutputFunction}
       SDL_LogSetOutputFunction} (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_CreateWindowFrom}SDL_CreateWindowFrom}
        (avoid [void *] type in the interface)}
    {- {{:http://wiki.libsdl.org/SDL_GetWindowData}SDL_GetWindowData}
        (avoid storing OCaml values in C)}
    {- {{:http://wiki.libsdl.org/SDL_SetWindowData}SDL_SetWindowData}
        (avoid storing OCaml values in C)}
    {- {{:http://wiki.libsdl.org/SDL_GetWindowWMInfo}SDL_GetWindowWMInfo}
        (avoid [void *] type in the interface)}
    {- {{:http://wiki.libsdl.org/SDL_GL_GetProcAddress}SDL_GL_GetProcAddress}
        (use another OCaml API)}
    {- {{:http://wiki.libsdl.org/SDL_GL_LoadLibrary}SDL_GL_LoadLibrary}
        (use another OCaml API)}
    {- {{:http://wiki.libsdl.org/SDL_GL_UnloadLibrary}SDL_GL_UnloadLibrary}
        (use another OCaml API)}
    {- {{:http://wiki.libsdl.org/SDL_AddEventWatch}SDL_AddEventWatch}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_DelEventWatch}SDL_DelEventWatch}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_FilterEvents}SDL_FilterEvents}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_GetEventFilter}SDL_GetEventFilter}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_SetEventFilter}SDL_SetEventFilter}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_PeepEvents}SDL_PeepEvents}
        (Should certainly be split into more than one fun,
        functionality also available through other bound functions.)}
    {- {{:http://wiki.libsdl.org/SDL_QuitRequested}SDL_QuitRequested}
        (cpp based)}
    {- {{:http://wiki.libsdl.org/SDL_AddTimer}SDL_AddTimer}
        (avoid callbacks from C to OCaml, besides callbacks are
        run on another thread, thus runtime lock support in ocaml-ctypes
        is needed. Probably better to use another OCaml API anyway)}
    {- {{:http://wiki.libsdl.org/SDL_RemoveTimer}SDL_RemoveTimer}
        (avoid callbacks from C to OCaml)}
    {- {{:http://wiki.libsdl.org/SDL_GetAudioStatus}SDL_GetAudioStatus}
        (SDL legacy function)}
    {- {{:http://wiki.libsdl.org/SDL_OpenAudio}SDL_OpenAudio}
        (SDL legacy function)}
    {- {{:http://wiki.libsdl.org/SDL_CloseAudio}SDL_CloseAudio}
        (SDL legacy function)}
    {- {{:http://wiki.libsdl.org/SDL_LockAudio}SDL_LockAudio}
        (SDL legacy function)}
    {- {{:http://wiki.libsdl.org/SDL_MixAudio}SDL_MixAudio}
        (SDL legacy function)}
    {- {{:http://wiki.libsdl.org/SDL_MixAudioFormat}
        SDL_MixAudioFormat} (limited functionality, do your own mixing).}
    {- {{:http://wiki.libsdl.org/SDL_PauseAudio}SDL_PauseAudio}
        (SDL legacy function)}
    {- {{:http://wiki.libsdl.org/SDL_UnlockAudio}SDL_UnlockAudio}
        (SDL legacy function)}} *)
end

(** {1:conventions Binding conventions}
    {2:naming Naming}

    C names are transformed as follows. The [SDL_] is mapped to the
    module name {!Sdl}, for the rest add an underscore between each
    minuscule and majuscule and lower case the result
    (e.g. [SDL_GetError] maps to {!Sdl.get_error}). Part of the name
    may also be wrapped by a module, (e.g. SDL_INIT_VIDEO becomes
    {!Sdl.Init.video}). If you open {!Tsdl}, your code will look
    mostly like SDL code but in accordance with OCaml's programming
    conventions. Exceptions to the naming convention do occur for
    technical reasons.

    {2:errors Errors}

    All functions that return an {!Sdl.result} have the string
    returned by [Sdl.get_error ()] in the [Error (`Msg _)] case.

    {2:enums Bit fields and enumerants}

    Most bit fields and enumerants are not mapped to variants, they
    are represented by OCaml values of a given abstract type in a
    specific module with a composition operator to combine them and a
    testing operator to test them. The flags for initializing SDL in the
    module {!Sdl.Init} is an example of that:
{[
match Sdl.init Sdl.Init.(video + timer + audio) with
| Error _ -> ...
| Ok () -> ...
]}
    Using variants in that case is inconvenient for the binding
    function and of limited use since most of the time bit fields are
    given to setup state and, as such, are less likley to be used for
    pattern matching. *)

(** {1:examples Examples}

    {2:toplevel Toplevel}

To use [Tsdl] in the toplevel with [findlib] just issue:
{[
> #use "topfind";;
> #require "tsdl.top";;
]}

This automatically loads the library and opens the [Tsdl] module.

    {2:opengl OpenGL window}

    The following is the minimum you need to get a working OpenGL window
    with SDL.
{[
open Tsdl
open Result

let main () = match Sdl.init Sdl.Init.video with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
        Sdl.delay 3000l;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
]}

This can be compiled to byte and native code with:
{v
> ocamlfind ocamlc -package tsdl -linkpkg -o min.byte min.ml
> ocamlfind ocamlopt -package tsdl -linkpkg -o min.native min.ml
v}

*)

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
