open Ctypes

module Types = Types_generated

let const_string_opt =
  Ctypes_std_views.nullable_view string Ctypes_static.(const char)

module Functions (F : FOREIGN) = struct
  let get_error =
    F.(foreign "SDL_GetError" (void @-> returning string))
  let sdl_free = F.(foreign "SDL_free" (ptr void @-> returning void))
  let set_main_ready = F.(foreign "SDL_SetMainReady" (void @-> returning void))
  let init =
    F.(foreign "SDL_Init" (uint32_t @-> returning int))
  let init_sub_system =
    F.(foreign "SDL_InitSubSystem" (uint32_t @-> returning int))
  let was_init =
    F.(foreign "SDL_WasInit" (uint32_t @-> returning uint32_t))
  let quit =
    F.(foreign "SDL_Quit" (void @-> returning void))
  let quit_sub_system =
    F.(foreign "SDL_QuitSubSystem" (uint32_t @-> returning void))

  module Hint = struct
    let framebuffer_acceleration =
      F.foreign_value "SDL_HINT_FRAMEBUFFER_ACCELERATION" (array 29 char)
    let idle_timer_disabled =
      F.foreign_value "SDL_HINT_IDLE_TIMER_DISABLED" (array 28 char)
    let mouse_focus_clickthrough =
      F.foreign_value "SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH" (array 29 char)
    let orientations =
      F.foreign_value "SDL_HINT_ORIENTATIONS" (array 21 char)
    let render_driver =
      F.foreign_value "SDL_HINT_RENDER_DRIVER" (array 18 char)
    let render_opengl_shaders =
      F.foreign_value "SDL_HINT_RENDER_OPENGL_SHADERS" (array 26 char)
    let render_logical_size_mode =
      F.foreign_value "SDL_HINT_RENDER_LOGICAL_SIZE_MODE" (array 29 char)
    let render_scale_quality =
      F.foreign_value "SDL_HINT_RENDER_SCALE_QUALITY" (array 25 char)
    let render_vsync =
      F.foreign_value "SDL_HINT_RENDER_VSYNC" (array 17 char)

    let no_signal_handlers =
      F.foreign_value "SDL_HINT_NO_SIGNAL_HANDLERS" (array 23 char)
    let thread_stack_size =
      F.foreign_value "SDL_HINT_THREAD_STACK_SIZE" (array 22 char)
    let window_frame_usable_while_cursor_hidden =
      F.foreign_value
        "SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN" (array 44 char)

    let audio_resampling_mode =
      F.foreign_value "SDL_HINT_AUDIO_RESAMPLING_MODE" (array 26 char)
    let mouse_normal_speed_scale =
      F.foreign_value "SDL_HINT_MOUSE_NORMAL_SPEED_SCALE" (array 29 char)
    let mouse_relative_speed_scale =
      F.foreign_value "SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE" (array 31 char)
    let touch_mouse_events =
      F.foreign_value "SDL_HINT_TOUCH_MOUSE_EVENTS" (array 23 char)
    let mouse_touch_events =
      F.foreign_value "SDL_HINT_MOUSE_TOUCH_EVENTS" (array 23 char)
  end

  let clear_hints =
    F.(foreign "SDL_ClearHints" (void @-> returning void))

  let get_hint =
    F.(foreign "SDL_GetHint" (string @-> returning const_string_opt))

  let get_hint_boolean =
    F.(foreign "SDL_GetHintBoolean" (string @-> bool @-> returning bool))

  let set_hint =
    F.(foreign "SDL_SetHint" (string @-> string @-> returning bool))

  let set_hint_with_priority =
    F.(foreign "SDL_SetHintWithPriority"
         (string @-> string @-> int @-> returning bool))

  (* Errors *)

  let clear_error =
    F.(foreign "SDL_ClearError" (void @-> returning void))

  let set_error =
    F.(foreign "SDL_SetError" (string @-> returning int))

  (* Log *)

  let log_get_priority =
    F.(foreign "SDL_LogGetPriority" (int @-> returning int))

  let log_reset_priorities =
    F.(foreign "SDL_LogResetPriorities" (void @-> returning void))

  let log_set_all_priority =
    F.(foreign "SDL_LogSetAllPriority" (int @-> returning void))

  let log_set_priority =
    F.(foreign "SDL_LogSetPriority" (int @-> int @-> returning void))

  (* Version *)

  let get_version =
    F.(foreign "SDL_GetVersion" (ptr Types.version @-> returning void))

  let get_revision =
    F.(foreign "SDL_GetRevision" (void @-> returning string))

  (* IO absraction *)

  let load_file_rw =
    F.(foreign "SDL_LoadFile_RW"
         (Types.rw_ops @-> ptr size_t @-> bool @-> returning string_opt))

  let rw_close =
    F.(foreign "SDL_RWclose" (Types.rw_ops @-> returning int))

  let rw_from_file =
    F.(foreign "SDL_RWFromFile"
         (string @-> string @-> returning Types.rw_ops_opt))

  let rw_from_const_mem =
    F.(foreign "SDL_RWFromConstMem"
         (ocaml_string @-> int @-> returning Types.rw_ops_opt))

  let rw_from_mem =
    F.(foreign "SDL_RWFromMem"
         (ocaml_bytes @-> int @-> returning Types.rw_ops_opt))

  (* File system paths *)

  let get_base_path =
    F.(foreign "SDL_GetBasePath" (void @-> returning (ptr char)))

  let get_pref_path =
    F.(foreign "SDL_GetPrefPath" (string @-> string @-> returning (ptr char)))

  (* Rectangles *)

  let enclose_points =
    F.(foreign "SDL_EnclosePoints"
         (ptr void @-> int @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning bool))

  let has_intersection =
    F.(foreign "SDL_HasIntersection"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> returning bool))

  let intersect_rect =
    F.(foreign "SDL_IntersectRect"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning bool))

  let intersect_rect_and_line =
    F.(foreign "SDL_IntersectRectAndLine"
         (ptr Types.Rect.t @-> ptr int @-> ptr int @-> ptr int @-> ptr int @->
          returning bool))

  let point_in_rect =
    F.(foreign "SDL_PointInRect"
         (ptr Types.Point.t @-> ptr Types.Rect.t @-> returning bool))

  let rect_empty =
    F.(foreign "SDL_RectEmpty" (ptr Types.Rect.t @-> returning bool))

  let rect_equals =
    F.(foreign "SDL_RectEquals"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> returning bool))

  let union_rect =
    F.(foreign "SDL_UnionRect"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning void))

  let alloc_palette =
    F.(foreign "SDL_AllocPalette" (int @-> returning (ptr_opt Types.palette)))

  let free_palette =
    F.(foreign "SDL_FreePalette" (ptr Types.palette @-> returning void))

  let set_palette_colors =
    F.(foreign "SDL_SetPaletteColors"
         (ptr Types.palette @-> ptr void(*Types.Color.t*) @-> int @-> int @-> returning int))

  (* See https://github.com/yallop/ocaml-ctypes/issues/109 for why (*u*) *)
  let calculate_gamma_ramp =
    F.(foreign "SDL_CalculateGammaRamp" (float @-> ptr (*u*)int16_t @-> returning void))

  let compose_custom_blend_mode =
    F.(foreign "SDL_ComposeCustomBlendMode"
         (int @-> int @-> int @-> int @-> int @-> int @-> returning Types.Blend.mode))

  let alloc_format =
    F.(foreign "SDL_AllocFormat"
         (uint32_t @-> returning (ptr_opt Types.pixel_format)))

  let free_format =
    F.(foreign "SDL_FreeFormat" (ptr Types.pixel_format @-> returning void))

  let get_pixel_format_name =
    F.(foreign "SDL_GetPixelFormatName" (uint32_t @-> returning string))

  let get_rgb =
    F.(foreign "SDL_GetRGB"
         (uint32_t @-> ptr Types.pixel_format @-> ptr uint8_t @->
          ptr uint8_t @-> ptr uint8_t @-> returning void))

  let get_rgba =
    F.(foreign "SDL_GetRGBA"
         (uint32_t @-> ptr Types.pixel_format @-> ptr uint8_t @->
          ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> returning void))

  let map_rgb =
    F.(foreign "SDL_MapRGB"
         (ptr Types.pixel_format @-> uint8_t @-> uint8_t @-> uint8_t @->
          returning uint32_t))

  let map_rgba =
    F.(foreign "SDL_MapRGBA"
         (ptr Types.pixel_format @-> uint8_t @-> uint8_t @-> uint8_t @->
          uint8_t @-> returning uint32_t))

  let masks_to_pixel_format_enum =
    F.(foreign "SDL_MasksToPixelFormatEnum"
         (int @-> uint32_t @-> uint32_t @-> uint32_t @-> uint32_t @->
          returning uint32_t))

  let pixel_format_enum_to_masks =
    F.(foreign "SDL_PixelFormatEnumToMasks"
      (uint32_t @-> ptr int @->
       ptr uint32_t @-> ptr uint32_t @-> ptr uint32_t @-> ptr uint32_t @->
       returning bool))

  let set_pixel_format_palette =
    F.(foreign "SDL_SetPixelFormatPalette"
         (ptr Types.pixel_format @-> ptr Types.palette @-> returning int))

  let blit_scaled =
    (* SDL_BlitScaled is #ifdef'd to SDL_UpperBlitScaled *)
    F.(foreign "SDL_UpperBlitScaled"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let blit_surface =
    (* SDL_BlitSurface is #ifdef'd to SDL_UpperBlit *)
    F.(foreign "SDL_UpperBlit"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let convert_pixels =
    F.(foreign "SDL_ConvertPixels"
         (int @-> int @-> uint32_t @-> ptr void @-> int @-> uint32_t @->
          ptr void @-> int @-> returning int))

  let convert_surface =
    F.(foreign "SDL_ConvertSurface"
         (ptr Types.surface @-> ptr Types.pixel_format @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let convert_surface_format =
    F.(foreign "SDL_ConvertSurfaceFormat"
         (ptr Types.surface @-> uint32_t @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let create_rgb_surface =
    F.(foreign "SDL_CreateRGBSurface"
         (uint32_t @-> int @-> int @-> int @-> uint32_t @-> uint32_t @->
          uint32_t @-> uint32_t @-> returning (ptr_opt Types.surface)))

  let create_rgb_surface_from =
    F.(foreign "SDL_CreateRGBSurfaceFrom"
         (ptr void @-> int @-> int @-> int @-> int @-> uint32_t @->
          uint32_t @-> uint32_t @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let create_rgb_surface_with_format =
    F.(foreign "SDL_CreateRGBSurfaceWithFormat"
         (uint32_t @-> int @-> int @-> int @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let create_rgb_surface_with_format_from =
    F.(foreign "SDL_CreateRGBSurfaceWithFormatFrom"
         (ptr void @-> int @-> int @-> int @-> int @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let duplicate_surface =
    F.(foreign "SDL_DuplicateSurface"
         (ptr Types.surface @-> returning (ptr Types.surface)))

  let fill_rect =
    F.(foreign "SDL_FillRect"
         (ptr Types.surface @-> ptr Types.Rect.t @-> uint32_t @->
          returning int))

  let fill_rects =
    F.(foreign "SDL_FillRects"
         (ptr Types.surface @-> ptr void (* Types.Rect.t *) @-> int @->
          uint32_t @-> returning int))

  let free_surface =
    F.(foreign "SDL_FreeSurface" (ptr Types.surface @-> returning void))

  let get_clip_rect =
    F.(foreign "SDL_GetClipRect"
         (ptr Types.surface @-> ptr Types.Rect.t @-> returning void))

  let get_color_key =
    F.(foreign "SDL_GetColorKey"
         (ptr Types.surface @-> ptr uint32_t @-> returning int))

  let get_surface_alpha_mod =
    F.(foreign "SDL_GetSurfaceAlphaMod"
         (ptr Types.surface @-> ptr uint8_t @-> returning int))

  let get_surface_blend_mode =
    F.(foreign "SDL_GetSurfaceBlendMode"
         (ptr Types.surface @-> ptr Types.Blend.mode @-> returning int))

  let get_surface_color_mod =
    F.(foreign "SDL_GetSurfaceColorMod"
         (ptr Types.surface @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @->
          returning int))

  let load_bmp_rw =
    F.(foreign "SDL_LoadBMP_RW"
         (Types.rw_ops @-> bool @-> returning (ptr_opt Types.surface)))

  let lock_surface =
    F.(foreign "SDL_LockSurface" (ptr Types.surface @-> returning int))

  let lower_blit =
    F.(foreign "SDL_LowerBlit"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let lower_blit_scaled =
    F.(foreign "SDL_LowerBlitScaled"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let save_bmp_rw =
    F.(foreign "SDL_SaveBMP_RW"
         (ptr Types.surface @-> Types.rw_ops @-> bool @-> returning int))

  let set_clip_rect =
    F.(foreign "SDL_SetClipRect"
         (ptr Types.surface @-> ptr Types.Rect.t @-> returning bool))

  let set_color_key =
    F.(foreign "SDL_SetColorKey"
         (ptr Types.surface @-> bool @-> uint32_t @-> returning int))

  let set_surface_alpha_mod =
    F.(foreign "SDL_SetSurfaceAlphaMod"
         (ptr Types.surface @-> uint8_t @-> returning int))

  let set_surface_blend_mode =
    F.(foreign "SDL_SetSurfaceBlendMode"
         (ptr Types.surface @-> Types.Blend.mode @-> returning int))

  let set_surface_color_mod =
    F.(foreign "SDL_SetSurfaceColorMod"
         (ptr Types.surface @-> uint8_t @-> uint8_t @-> uint8_t @->
          returning int))

  let set_surface_palette =
    F.(foreign "SDL_SetSurfacePalette"
         (ptr Types.surface @-> ptr Types.palette @-> returning int))

  let set_surface_rle =
    F.(foreign "SDL_SetSurfaceRLE"
         (ptr Types.surface @-> bool @-> returning int))

  let unlock_surface =
    F.(foreign "SDL_UnlockSurface" (ptr Types.surface @-> returning void))

  let create_renderer =
    F.(foreign "SDL_CreateRenderer"
         (Types.Window.t @-> int @-> uint32_t @->
          returning (ptr_opt Types.Renderer.t)))

  let create_software_renderer =
    F.(foreign "SDL_CreateSoftwareRenderer"
         (ptr Types.surface @-> returning (ptr_opt Types.Renderer.t)))

  let destroy_renderer =
    F.(foreign "SDL_DestroyRenderer" (ptr Types.Renderer.t @-> returning void))

  let get_num_render_drivers =
    F.(foreign "SDL_GetNumRenderDrivers" (void @-> returning int))

  let get_render_draw_blend_mode =
    F.(foreign "SDL_GetRenderDrawBlendMode"
         (ptr Types.Renderer.t @-> ptr Types.Blend.mode @-> returning int))

  let get_render_draw_color =
    F.(foreign "SDL_GetRenderDrawColor"
         (ptr Types.Renderer.t @-> ptr uint8_t @-> ptr uint8_t @->
          ptr uint8_t @-> ptr uint8_t @-> returning int))

  let get_render_driver_info =
    F.(foreign "SDL_GetRenderDriverInfo"
         (int @-> ptr Types.renderer_info @-> returning int))

  let get_render_target =
    F.(foreign "SDL_GetRenderTarget"
         (ptr Types.Renderer.t @-> returning (ptr_opt Types.Texture.t)))

  let get_renderer =
    F.(foreign "SDL_GetRenderer"
         (Types.Window.t @-> returning (ptr_opt Types.Renderer.t)))

  let get_renderer_info =
    F.(foreign "SDL_GetRendererInfo"
         (ptr Types.Renderer.t @-> ptr Types.renderer_info @-> returning int))

  let get_renderer_output_size =
    F.(foreign "SDL_GetRendererOutputSize"
         (ptr Types.Renderer.t @-> ptr int @-> ptr int @-> returning int))

  let render_clear =
    F.(foreign "SDL_RenderClear" (ptr Types.Renderer.t @-> returning int))

  let render_copy =
    F.(foreign "SDL_RenderCopy"
         (ptr Types.Renderer.t @-> ptr Types.Texture.t @-> ptr Types.Rect.t @->
          ptr Types.Rect.t @-> returning int))

  let render_copy_ex =
    F.(foreign "SDL_RenderCopyEx"
         (ptr Types.Renderer.t @-> ptr Types.Texture.t @-> ptr Types.Rect.t @->
          ptr Types.Rect.t @-> double @-> ptr Types.Point.t @-> int @->
          returning int))

  let render_draw_line =
    F.(foreign "SDL_RenderDrawLine"
         (ptr Types.Renderer.t @-> int @-> int @-> int @-> int @->
          returning int))

  let render_draw_line_f =
    F.(foreign "SDL_RenderDrawLineF"
         (ptr Types.Renderer.t @-> float @-> float @-> float @-> float @->
          returning int))

  let render_draw_lines =
    F.(foreign "SDL_RenderDrawLines"
         (ptr Types.Renderer.t @-> ptr void @-> int @-> returning int))

  let render_draw_point =
    F.(foreign "SDL_RenderDrawPoint"
         (ptr Types.Renderer.t @-> int @-> int @-> returning int))

  let render_draw_points =
    F.(foreign "SDL_RenderDrawPoints"
         (ptr Types.Renderer.t @-> ptr void @-> int @-> returning int))

  let render_draw_point_f =
    F.(foreign "SDL_RenderDrawPointF"
         (ptr Types.Renderer.t @-> float @-> float @-> returning int))

  let render_draw_points_f =
    F.(foreign "SDL_RenderDrawPointsF"
         (ptr Types.Renderer.t @-> ptr void @-> int @-> returning int))

  let render_draw_rect =
    F.(foreign "SDL_RenderDrawRect"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> returning int))

  let render_draw_rects =
    F.(foreign "SDL_RenderDrawRects"
         (ptr Types.Renderer.t @-> ptr void @-> int @-> returning int))

  let render_fill_rect =
    F.(foreign "SDL_RenderFillRect"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> returning int))

  let render_fill_rects =
    F.(foreign "SDL_RenderFillRects"
         (ptr Types.Renderer.t @-> ptr void @-> int @-> returning int))

  let render_geometry =
    F.(foreign "SDL_RenderGeometry"
         (ptr Types.Renderer.t @-> ptr_opt Types.Texture.t @->
          ptr Types.Vertex.t @-> int @-> ptr_opt int @-> int @-> returning int))

  let render_geometry_raw =
    F.(foreign "SDL_RenderGeometryRaw"
         (ptr Types.Renderer.t @-> ptr_opt Types.Texture.t @->
          ptr float @-> int @->
          ptr void (*Types.Color.t*) @-> int @->
          ptr float @-> int @->
          int @-> ptr void @-> int @-> int @-> returning int))

  let render_get_clip_rect =
    F.(foreign "SDL_RenderGetClipRect"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> returning void))

  let render_is_clip_enabled =
    F.(foreign "SDL_RenderIsClipEnabled"
         (ptr Types.Renderer.t @-> returning bool))

  let render_get_integer_scale =
    F.(foreign "SDL_RenderGetIntegerScale"
         (ptr Types.Renderer.t @-> returning bool))

  let render_get_logical_size =
    F.(foreign "SDL_RenderGetLogicalSize"
         (ptr Types.Renderer.t @-> ptr int @-> ptr int @-> returning void))

  let render_get_scale =
    F.(foreign "SDL_RenderGetScale"
         (ptr Types.Renderer.t @-> ptr float @-> ptr float @-> returning void))

  let render_get_viewport =
    F.(foreign "SDL_RenderGetViewport"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> returning void))

  let render_read_pixels =
    F.(foreign "SDL_RenderReadPixels"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> uint32_t @->
          ptr void @-> int @-> returning int))

  let render_set_clip_rect =
    F.(foreign "SDL_RenderSetClipRect"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> returning int))

  let render_set_integer_scale =
    F.(foreign "SDL_RenderSetIntegerScale"
         (ptr Types.Renderer.t @-> bool @-> returning int))

  let render_set_logical_size =
    F.(foreign "SDL_RenderSetLogicalSize"
         (ptr Types.Renderer.t @-> int @-> int @-> returning int))

  let render_set_scale =
    F.(foreign "SDL_RenderSetScale"
         (ptr Types.Renderer.t @-> float @-> float @-> returning int))

  let render_set_viewport =
    F.(foreign "SDL_RenderSetViewport"
         (ptr Types.Renderer.t @-> ptr Types.Rect.t @-> returning int))

  let render_target_supported =
    F.(foreign "SDL_RenderTargetSupported"
         (ptr Types.Renderer.t @-> returning bool))

  let set_render_draw_blend_mode =
    F.(foreign "SDL_SetRenderDrawBlendMode"
         (ptr Types.Renderer.t @-> Types.Blend.mode @-> returning int))

  let set_render_draw_color =
    F.(foreign "SDL_SetRenderDrawColor"
         (ptr Types.Renderer.t @-> uint8_t @-> uint8_t @-> uint8_t @->
          uint8_t @-> returning int))

  let set_render_target =
    F.(foreign "SDL_SetRenderTarget"
         (ptr Types.Renderer.t @-> ptr_opt Types.Texture.t @-> returning int))

  let create_texture =
    F.(foreign "SDL_CreateTexture"
         (ptr Types.Renderer.t @-> uint32_t @-> int @-> int @-> int @->
          returning (ptr_opt Types.Texture.t)))

  let create_texture_from_surface =
    F.(foreign "SDL_CreateTextureFromSurface"
         (ptr Types.Renderer.t @-> ptr Types.surface @->
          returning (ptr_opt Types.Texture.t)))

  let destroy_texture =
    F.(foreign "SDL_DestroyTexture" (ptr Types.Texture.t @-> returning void))

  let get_texture_alpha_mod =
    F.(foreign "SDL_GetTextureAlphaMod"
         (ptr Types.Texture.t @-> ptr uint8_t @-> returning int))

  let get_texture_blend_mode =
    F.(foreign "SDL_GetTextureBlendMode"
         (ptr Types.Texture.t @-> ptr Types.Blend.mode @-> returning int))

  let get_texture_color_mod =
    F.(foreign "SDL_GetTextureColorMod"
         (ptr Types.Texture.t @-> ptr uint8_t @-> ptr uint8_t @->
          ptr uint8_t @-> returning int))

  let query_texture =
    F.(foreign "SDL_QueryTexture"
         (ptr Types.Texture.t @-> ptr uint32_t @-> ptr int @-> ptr int @->
          ptr int @-> returning int))

  let lock_texture =
    F.(foreign "SDL_LockTexture"
         (ptr Types.Texture.t @-> ptr Types.Rect.t @-> ptr (ptr void) @->
          ptr int @-> returning int))

  let set_texture_alpha_mod =
    F.(foreign "SDL_SetTextureAlphaMod"
         (ptr Types.Texture.t @-> uint8_t @-> returning int))

  let set_texture_blend_mode =
    F.(foreign "SDL_SetTextureBlendMode"
         (ptr Types.Texture.t @-> Types.Blend.mode @-> returning int))

  let set_texture_color_mod =
    F.(foreign "SDL_SetTextureColorMod"
         (ptr Types.Texture.t @-> uint8_t @-> uint8_t @-> uint8_t @-> returning int))

  let unlock_texture =
    F.(foreign "SDL_UnlockTexture" (ptr Types.Texture.t @-> returning void))

  let update_texture =
    F.(foreign "SDL_UpdateTexture"
         (ptr Types.Texture.t @-> ptr Types.Rect.t @-> ptr void @-> int @->
          returning int))

  let update_yuv_texture =
    F.(foreign "SDL_UpdateYUVTexture"
         (ptr Types.Texture.t @-> ptr Types.Rect.t @->
          ptr (*u*)int8_t @-> int @-> ptr (*u*)int8_t @-> int @->
          ptr (*u*)int8_t @-> int @-> returning int))

  (* Video drivers *)

  let get_current_video_driver =
    F.(foreign "SDL_GetCurrentVideoDriver" (void @-> returning const_string_opt))

  let get_num_video_drivers =
    F.(foreign "SDL_GetNumVideoDrivers" (void @-> returning int))

  let get_video_driver =
    F.(foreign "SDL_GetVideoDriver" (int @-> returning const_string_opt))

  let video_init =
    F.(foreign "SDL_VideoInit" (string_opt @-> returning int))

  let video_quit =
    F.(foreign "SDL_VideoQuit" (void @-> returning void))

  (* Displays *)

  let get_closest_display_mode =
    F.(foreign "SDL_GetClosestDisplayMode"
         (int @-> ptr Types.display_mode @-> ptr Types.display_mode @->
          returning (ptr_opt void)))

  let get_current_display_mode =
    F.(foreign "SDL_GetCurrentDisplayMode"
         (int @-> ptr Types.display_mode @-> returning int))

  let get_desktop_display_mode =
    F.(foreign "SDL_GetDesktopDisplayMode"
         (int @-> ptr Types.display_mode @-> returning int))

  let get_display_bounds =
    F.(foreign "SDL_GetDisplayBounds"
         (int @-> ptr Types.Rect.t @-> returning int))

  let get_display_dpi =
    F.(foreign "SDL_GetDisplayDPI"
         (int @-> ptr float @-> ptr float @-> ptr float @-> returning int))

  let get_display_mode =
    F.(foreign "SDL_GetDisplayMode"
         (int @-> int @-> ptr Types.display_mode @-> returning int))

  let get_display_usable_bounds =
    F.(foreign "SDL_GetDisplayUsableBounds"
         (int @-> ptr Types.Rect.t @-> returning int))

  let get_num_display_modes =
    F.(foreign "SDL_GetNumDisplayModes" (int @-> returning int))

  let get_display_name =
    F.(foreign "SDL_GetDisplayName" (int @-> returning const_string_opt))

  let get_num_video_displays =
    F.(foreign "SDL_GetNumVideoDisplays" (void @-> returning int))

  let create_window =
    F.(foreign "SDL_CreateWindow"
         (string @-> int @-> int @-> int @-> int @-> uint32_t @->
          returning Types.Window.opt))

  let create_window_and_renderer =
    F.(foreign "SDL_CreateWindowAndRenderer"
         (int @-> int @-> uint32_t @-> ptr Types.Window.t @->
          ptr (ptr Types.Renderer.t) @-> (returning int)))

  let destroy_window =
    F.(foreign "SDL_DestroyWindow" (Types.Window.t @-> returning void))

  let get_window_brightness =
    F.(foreign "SDL_GetWindowBrightness" (Types.Window.t @-> returning float))

  let get_window_borders_size =
    F.(foreign "SDL_GetWindowBordersSize"
         (Types.Window.t @-> ptr int @-> ptr int @-> ptr int @-> ptr int @->
          returning int))

  let get_window_display_index =
    F.(foreign "SDL_GetWindowDisplayIndex" (Types.Window.t @-> returning int))

  let get_window_display_mode =
    F.(foreign "SDL_GetWindowDisplayMode"
         (Types.Window.t @-> (ptr Types.display_mode) @-> returning int))

  let get_window_flags =
    F.(foreign "SDL_GetWindowFlags" (Types.Window.t @-> returning uint32_t))

  let get_window_from_id =
    F.(foreign "SDL_GetWindowFromID"
         (uint32_t @-> returning Types.Window.opt))

  let get_window_gamma_ramp =
    F.(foreign "SDL_GetWindowGammaRamp"
         (Types.Window.t @-> ptr (*u*)int16_t @-> ptr (*u*)int16_t @->
          ptr (*u*)int16_t @-> returning int))

  let get_window_grab =
    F.(foreign "SDL_GetWindowGrab" (Types.Window.t @-> returning bool))

  let get_grabbed_window =
    F.(foreign "SDL_GetGrabbedWindow" (void @-> returning Types.Window.t))

  let get_window_id =
    F.(foreign "SDL_GetWindowID" (Types.Window.t @-> returning uint32_t))

  let get_window_maximum_size =
    F.(foreign "SDL_GetWindowMaximumSize"
         (Types.Window.t @-> (ptr int) @-> (ptr int) @-> returning void))

  let get_window_minimum_size =
    F.(foreign "SDL_GetWindowMinimumSize"
         (Types.Window.t @-> (ptr int) @-> (ptr int) @-> returning void))

  let get_window_opacity =
    F.(foreign "SDL_GetWindowOpacity"
         (Types.Window.t @-> (ptr float) @-> returning int))

  let get_window_pixel_format =
    F.(foreign "SDL_GetWindowPixelFormat"
         (Types.Window.t @-> returning uint32_t))

  let get_window_position =
    F.(foreign "SDL_GetWindowPosition"
         (Types.Window.t @-> (ptr int) @-> (ptr int) @-> returning void))

  let get_window_size =
    F.(foreign "SDL_GetWindowSize"
         (Types.Window.t @-> (ptr int) @-> (ptr int) @-> returning void))

  let get_window_surface =
    F.(foreign "SDL_GetWindowSurface"
         (Types.Window.t @-> returning (ptr_opt Types.surface)))

  let get_window_title =
    F.(foreign "SDL_GetWindowTitle" (Types.Window.t @-> returning string))

  let hide_window =
    F.(foreign "SDL_HideWindow" (Types.Window.t @-> returning void))

  let maximize_window =
    F.(foreign "SDL_MaximizeWindow" (Types.Window.t @-> returning void))

  let minimize_window =
    F.(foreign "SDL_MinimizeWindow" (Types.Window.t @-> returning void))

  let raise_window =
    F.(foreign "SDL_RaiseWindow" (Types.Window.t @-> returning void))

  let restore_window =
    F.(foreign "SDL_RestoreWindow" (Types.Window.t @-> returning void))

  let set_window_bordered =
    F.(foreign "SDL_SetWindowBordered"
         (Types.Window.t @-> bool @-> returning void))

  let set_window_brightness =
    F.(foreign "SDL_SetWindowBrightness"
         (Types.Window.t @-> float @-> returning int))

  let set_window_display_mode =
    F.(foreign "SDL_SetWindowDisplayMode"
         (Types.Window.t @-> (ptr Types.display_mode) @-> returning int))

  let set_window_fullscreen =
    F.(foreign "SDL_SetWindowFullscreen"
         (Types.Window.t @-> uint32_t @-> returning int))

  let set_window_gamma_ramp =
    F.(foreign "SDL_SetWindowGammaRamp"
         (Types.Window.t @-> ptr (*u*)int16_t @-> ptr (*u*)int16_t @->
          ptr (*u*)int16_t @-> returning int))

  let set_window_grab =
    F.(foreign "SDL_SetWindowGrab" (Types.Window.t @-> bool @-> returning void))

  let set_window_icon =
    F.(foreign "SDL_SetWindowIcon"
         (Types.Window.t @-> ptr Types.surface @-> returning void))

  let set_window_input_focus =
    F.(foreign "SDL_SetWindowInputFocus" (Types.Window.t @-> returning int))

  let set_window_maximum_size =
    F.(foreign "SDL_SetWindowMaximumSize"
         (Types.Window.t @-> int @-> int @-> returning void))

  let set_window_minimum_size =
    F.(foreign "SDL_SetWindowMinimumSize"
         (Types.Window.t @-> int @-> int @-> returning void))

  let set_window_modal_for =
    F.(foreign "SDL_SetWindowModalFor"
         (Types.Window.t @-> Types.Window.t @-> returning int))

  let set_window_opacity =
    F.(foreign "SDL_SetWindowOpacity"
         (Types.Window.t @-> float @-> returning int))

  let set_window_position =
    F.(foreign "SDL_SetWindowPosition"
         (Types.Window.t @-> int @-> int @-> returning void))

  let set_window_resizable =
    F.(foreign "SDL_SetWindowResizable"
         (Types.Window.t @-> bool @-> returning void))

  let set_window_size =
    F.(foreign "SDL_SetWindowSize"
         (Types.Window.t @-> int @-> int @-> returning void))

  let set_window_title =
    F.(foreign "SDL_SetWindowTitle"
         (Types.Window.t @-> string @-> returning void))

  let show_window =
    F.(foreign "SDL_ShowWindow" (Types.Window.t @-> returning void))

  let update_window_surface =
    F.(foreign "SDL_UpdateWindowSurface"
         (Types.Window.t @-> returning int))

  let update_window_surface_rects =
    F.(foreign "SDL_UpdateWindowSurfaceRects"
         (Types.Window.t @-> ptr void @-> int @-> returning int))

  let gl_bind_texture =
    F.(foreign "SDL_GL_BindTexture"
         (ptr Types.Texture.t @-> ptr float @-> ptr float @-> returning int))

  let gl_create_context =
    F.(foreign "SDL_GL_CreateContext"
         (Types.Window.t @-> returning  (ptr_opt Types.Gl.context)))

  let gl_delete_context =
    F.(foreign "SDL_GL_DeleteContext" (ptr Types.Gl.context @-> returning void))

  let gl_extension_supported =
    F.(foreign "SDL_GL_ExtensionSupported" (string @-> returning bool))

  let gl_get_attribute =
    F.(foreign "SDL_GL_GetAttribute" (int @-> (ptr int) @-> returning int))

  let gl_get_current_context =
    F.(foreign "SDL_GL_GetCurrentContext"
         (void @-> returning (ptr_opt Types.Gl.context)))

  let gl_get_drawable_size =
    F.(foreign "SDL_GL_GetDrawableSize"
         (Types.Window.t @-> ptr int @-> ptr int @-> returning void))

  let gl_get_swap_interval =
    F.(foreign "SDL_GL_GetSwapInterval" (void @-> returning int))

  let gl_make_current =
    F.(foreign "SDL_GL_MakeCurrent"
         (Types.Window.t @-> ptr Types.Gl.context @-> returning int))

  let gl_reset_attributes =
    F.(foreign "SDL_GL_ResetAttributes" (void @-> returning void))

  let gl_set_attribute =
    F.(foreign "SDL_GL_SetAttribute" (int @-> int @-> returning int))

  let gl_set_swap_interval =
    F.(foreign "SDL_GL_SetSwapInterval" (int @-> returning int))

  let gl_swap_window =
    F.(foreign "SDL_GL_SwapWindow" (Types.Window.t @-> returning void))

  let gl_unbind_texture =
    F.(foreign "SDL_GL_UnbindTexture" (ptr Types.Texture.t @-> returning int))

  module Vulkan = struct
    let load_library =
      F.(foreign "SDL_Vulkan_LoadLibrary" (const_string_opt @-> returning int))

    let unload_library =
      F.(foreign "SDL_Vulkan_UnloadLibrary" (void @-> returning void))

    let get_instance_extensions =
      F.(foreign "SDL_Vulkan_GetInstanceExtensions"
           (Types.Window.t @-> ptr int @-> ptr string @-> returning bool))

    let create_surface =
      F.(foreign "SDL_Vulkan_CreateSurface"
           (Types.Window.t @-> ptr void @-> ptr Types.Vulkan.surface @->
            returning bool))

    let get_drawable_size =
      F.(foreign "SDL_Vulkan_GetDrawableSize"
           (Types.Window.t @-> ptr int @-> ptr int @-> returning void))
  end

  let disable_screen_saver =
    F.(foreign "SDL_DisableScreenSaver" (void @-> returning void))

  let enable_screen_saver =
    F.(foreign "SDL_EnableScreenSaver" (void @-> returning void))

  let is_screen_saver_enabled =
    F.(foreign "SDL_IsScreenSaverEnabled" (void @-> returning bool))

  module Message_box = struct
    let show =
      F.(foreign "SDL_ShowMessageBox"
           (ptr Types.Message_box.data @-> ptr int @-> returning int))

    let show_simple =
      F.(foreign "SDL_ShowSimpleMessageBox"
           (uint32_t @-> string @-> string @-> Types.Window.opt @-> returning int))
  end

  let get_clipboard_text =
    F.(foreign "SDL_GetClipboardText" (void @-> returning (ptr char)))

  let has_clipboard_text =
    F.(foreign "SDL_HasClipboardText" (void @-> returning bool))

  let set_clipboard_text =
    F.(foreign "SDL_SetClipboardText" (string @-> returning int))

  let scancode = int
  let keycode = int
  let keymod = uint16_t

  let get_keyboard_focus =
    F.(foreign "SDL_GetKeyboardFocus" (void @-> returning Types.Window.opt))

  let get_keyboard_state =
    F.(foreign "SDL_GetKeyboardState" (ptr int @-> returning (ptr (const uint8_t))))

  let get_key_from_name =
    F.(foreign "SDL_GetKeyFromName" (string @-> returning keycode))

  let get_key_from_scancode =
    F.(foreign "SDL_GetKeyFromScancode" (scancode @-> returning keycode))

  let get_key_name =
    F.(foreign "SDL_GetKeyName" (keycode @-> returning string))

  let get_mod_state =
    F.(foreign "SDL_GetModState" (void @-> returning keymod))

  let get_scancode_from_key =
    F.(foreign "SDL_GetScancodeFromKey" (keycode @-> returning scancode))

  let get_scancode_from_name =
    F.(foreign "SDL_GetScancodeFromName" (string @-> returning scancode))

  let get_scancode_name =
    F.(foreign "SDL_GetScancodeName" (scancode @-> returning string))

  let has_screen_keyboard_support =
    F.(foreign "SDL_HasScreenKeyboardSupport" (void @-> returning bool))

  let is_screen_keyboard_shown =
    F.(foreign "SDL_IsScreenKeyboardShown" (Types.Window.t @-> returning bool))

  let is_text_input_active =
    F.(foreign "SDL_IsTextInputActive" (void @-> returning bool))

  let set_mod_state =
    F.(foreign "SDL_SetModState" (keymod @-> returning void))

  let set_text_input_rect =
    F.(foreign "SDL_SetTextInputRect" (ptr Types.Rect.t @-> returning void))

  let start_text_input =
    F.(foreign "SDL_StartTextInput" (void @-> returning void))

  let stop_text_input =
    F.(foreign "SDL_StopTextInput" (void @-> returning void))

  let capture_mouse =
    F.(foreign "SDL_CaptureMouse" (bool @-> returning int))

  let create_color_cursor =
    F.(foreign "SDL_CreateColorCursor"
         (ptr Types.surface @-> int @-> int @->
          returning (ptr_opt Types.cursor)))

  let create_cursor =
    F.(foreign "SDL_CreateCursor"
         (ptr (*u*)int8_t @-> ptr (*u*)int8_t @-> int @-> int @-> int @->
          int @-> returning (ptr_opt Types.cursor)))

  let create_system_cursor =
    F.(foreign "SDL_CreateSystemCursor"
         (int @-> returning (ptr_opt Types.cursor)))

  let free_cursor =
    F.(foreign "SDL_FreeCursor" (ptr Types.cursor @-> returning void))

  let get_cursor =
    F.(foreign "SDL_GetCursor" (void @-> returning (ptr_opt Types.cursor)))

  let get_default_cursor =
    F.(foreign "SDL_GetDefaultCursor" (void @-> returning (ptr_opt Types.cursor)))

  let get_global_mouse_state =
    F.(foreign "SDL_GetGlobalMouseState"
         (ptr int @-> ptr int @-> returning uint32_t))

  let get_mouse_focus =
    F.(foreign "SDL_GetMouseFocus" (void @-> returning Types.Window.opt))

  let get_mouse_state =
    F.(foreign "SDL_GetMouseState"
         (ptr int @-> ptr int @-> returning uint32_t))

  let get_relative_mouse_mode =
    F.(foreign "SDL_GetRelativeMouseMode" (void @-> returning bool))

  let get_relative_mouse_state =
    F.(foreign "SDL_GetRelativeMouseState"
         (ptr int @-> ptr int @-> returning uint32_t))

  let show_cursor =
    F.(foreign "SDL_ShowCursor" (int @-> returning int))

  let set_cursor =
    F.(foreign "SDL_SetCursor" (ptr_opt Types.cursor @-> returning void))

  let set_relative_mouse_mode =
    F.(foreign "SDL_SetRelativeMouseMode" (bool @-> returning int))

  let warp_mouse_in_window =
    F.(foreign "SDL_WarpMouseInWindow"
         (Types.Window.opt @-> int @-> int @-> returning void))

  let warp_mouse_global=
    F.(foreign "SDL_WarpMouseGlobal" (int @-> int @-> returning int))

  let get_num_touch_devices =
    F.(foreign "SDL_GetNumTouchDevices" (void @-> returning int))

  let get_num_touch_fingers =
    F.(foreign "SDL_GetNumTouchFingers" (int64_t @-> returning int))

  let get_touch_device =
    F.(foreign "SDL_GetTouchDevice" (int @-> returning int64_t))

  let get_touch_finger =
    F.(foreign "SDL_GetTouchFinger"
         (int64_t @-> int @-> returning (ptr_opt Types.Finger.t)))

  let load_dollar_templates =
    F.(foreign "SDL_LoadDollarTemplates"
         (int64_t @-> Types.rw_ops @-> returning int))

  let record_gesture =
    F.(foreign "SDL_RecordGesture" (int64_t @-> returning int))

  let save_dollar_template =
    F.(foreign "SDL_SaveDollarTemplate"
         (int64_t @-> Types.rw_ops @-> returning int))

  let save_all_dollar_templates =
    F.(foreign "SDL_SaveAllDollarTemplates" (Types.rw_ops @-> returning int))

  let joystick_close =
    F.(foreign "SDL_JoystickClose" (ptr Types.joystick @-> returning void))

  let joystick_current_power_level =
    F.(foreign "SDL_JoystickCurrentPowerLevel"
         (ptr Types.joystick @-> returning int))

  let joystick_event_state =
    F.(foreign "SDL_JoystickEventState" (int @-> returning int))

  let joystick_from_instance_id =
    F.(foreign "SDL_JoystickFromInstanceID"
         (int32_t @-> returning (ptr Types.joystick)))

  let joystick_get_attached =
    F.(foreign "SDL_JoystickGetAttached"
         (ptr Types.joystick @-> returning bool))

  let joystick_get_axis =
    F.(foreign "SDL_JoystickGetAxis"
         (ptr Types.joystick @-> int @-> returning int16_t))

  let joystick_get_axis_initial_state =
    F.(foreign "SDL_JoystickGetAxisInitialState"
         (ptr Types.joystick @-> int @-> ptr int16_t @-> returning bool))

  let joystick_get_ball =
    F.(foreign "SDL_JoystickGetBall"
         (ptr Types.joystick @-> int @-> ptr int @-> ptr int @-> returning int))

  let joystick_get_button =
    F.(foreign "SDL_JoystickGetButton"
         (ptr Types.joystick @-> int @-> returning uint8_t))

  let joystick_get_device_guid =
    F.(foreign "SDL_JoystickGetDeviceGUID" (int @-> returning Types.joystick_guid))

  let joystick_get_device_product =
    F.(foreign "SDL_JoystickGetDeviceProduct" (int @-> returning uint16_t))

  let joystick_get_device_product_version =
    F.(foreign "SDL_JoystickGetDeviceProductVersion"
         (int @-> returning uint16_t))

  let joystick_get_device_type =
    F.(foreign "SDL_JoystickGetDeviceType" (int @-> returning int))

  let joystick_get_device_instance_id =
    F.(foreign "SDL_JoystickGetDeviceInstanceID" (int @-> returning int32_t))

  let joystick_get_device_vendor =
    F.(foreign "SDL_JoystickGetDeviceVendor" (int @-> returning uint16_t))

  let joystick_get_guid =
    F.(foreign "SDL_JoystickGetGUID"
         (ptr Types.joystick @-> returning Types.joystick_guid))

  let joystick_get_guid_from_string =
    F.(foreign "SDL_JoystickGetGUIDFromString"
         (string @-> returning Types.joystick_guid))

  let joystick_get_guid_string =
    F.(foreign "SDL_JoystickGetGUIDString"
         (Types.joystick_guid @-> ptr char @-> int @-> returning void))

  let joystick_get_hat =
    F.(foreign "SDL_JoystickGetHat"
         (ptr Types.joystick @-> int @-> returning uint8_t))

  let joystick_get_product =
    F.(foreign "SDL_JoystickGetProduct"
         (ptr Types.joystick @-> returning uint16_t))

  let joystick_get_product_version =
    F.(foreign "SDL_JoystickGetProductVersion"
         (ptr Types.joystick @-> returning uint16_t))

  let joystick_get_type =
    F.(foreign "SDL_JoystickGetType" (ptr Types.joystick @-> returning int))

  let joystick_get_vendor =
    F.(foreign "SDL_JoystickGetVendor"
         (ptr Types.joystick @-> returning uint16_t))

  let joystick_instance_id =
    F.(foreign "SDL_JoystickInstanceID"
         (ptr Types.joystick @-> returning int32_t))

  let joystick_name =
    F.(foreign "SDL_JoystickName"
         (ptr Types.joystick @-> returning const_string_opt))

  let joystick_name_for_index =
    F.(foreign "SDL_JoystickNameForIndex" (int @-> returning const_string_opt))

  let joystick_num_axes =
    F.(foreign "SDL_JoystickNumAxes" (ptr Types.joystick @-> returning int))

  let joystick_num_balls =
    F.(foreign "SDL_JoystickNumBalls" (ptr Types.joystick @-> returning int))

  let joystick_num_buttons =
    F.(foreign "SDL_JoystickNumButtons" (ptr Types.joystick @-> returning int))

  let joystick_num_hats =
    F.(foreign "SDL_JoystickNumHats" (ptr Types.joystick @-> returning int))

  let joystick_open =
    F.(foreign "SDL_JoystickOpen" (int @-> returning (ptr_opt Types.joystick)))

  let joystick_update =
    F.(foreign "SDL_JoystickUpdate" (void @-> returning void))

  let num_joysticks =
    F.(foreign "SDL_NumJoysticks" (void @-> returning int))

  type _button_bind
  let button_bind : _button_bind structure typ =
    structure "SDL_GameControllerButtonBind"
  let button_bind_bind_type = field button_bind "bindType" int
  let button_bind_value1 = field button_bind "value1" int  (* simplified enum *)
  let button_bind_value2 = field button_bind "value2" int
  let () = seal button_bind

  let game_controller_add_mapping =
    F.(foreign "SDL_GameControllerAddMapping" (string @-> returning int))

  let game_controller_add_mapping_from_rw =
    F.(foreign "SDL_GameControllerAddMappingsFromRW"
         (Types.rw_ops @-> bool @-> returning int))

  let game_controller_close =
    F.(foreign "SDL_GameControllerClose" (ptr Types.game_controller @-> returning void))

  let game_controller_event_state =
    F.(foreign "SDL_GameControllerEventState" (int @-> returning int))

  let game_controller_from_instance_id =
    F.(foreign "SDL_GameControllerFromInstanceID"
         (int32_t @-> returning (ptr Types.game_controller)))

  let game_controller_get_attached =
    F.(foreign "SDL_GameControllerGetAttached"
         (ptr Types.game_controller @-> returning bool))

  let game_controller_get_axis =
    F.(foreign "SDL_GameControllerGetAxis"
         (ptr Types.game_controller @-> int @-> returning int16_t))

  let game_controller_get_axis_from_string =
    F.(foreign "SDL_GameControllerGetAxisFromString"
         (string @-> returning int))

  let game_controller_get_bind_for_axis =
    F.(foreign "SDL_GameControllerGetBindForAxis"
         (ptr Types.game_controller @-> int @-> returning button_bind))

  let game_controller_get_bind_for_button =
    F.(foreign "SDL_GameControllerGetBindForButton"
         (ptr Types.game_controller @-> int @-> returning button_bind))

  let game_controller_get_button =
    F.(foreign "SDL_GameControllerGetButton"
         (ptr Types.game_controller @-> int @-> returning uint8_t))

  let game_controller_get_button_from_string =
    F.(foreign "SDL_GameControllerGetButtonFromString" (string @-> returning int))

  let game_controller_get_joystick =
    F.(foreign "SDL_GameControllerGetJoystick"
         (ptr Types.game_controller @-> returning (ptr_opt Types.joystick)))

  let game_controller_get_product =
    F.(foreign "SDL_GameControllerGetProduct"
         (ptr Types.game_controller @-> returning uint16_t))

  let game_controller_get_product_version =
    F.(foreign "SDL_GameControllerGetProductVersion"
         (ptr Types.game_controller @-> returning uint16_t))

  let game_controller_get_string_for_axis =
    F.(foreign "SDL_GameControllerGetStringForAxis"
         (int @-> returning const_string_opt))

  let game_controller_get_string_for_button =
    F.(foreign "SDL_GameControllerGetStringForButton"
         (int @-> returning const_string_opt))

  let game_controller_get_vendor =
    F.(foreign "SDL_GameControllerGetVendor"
         (ptr Types.game_controller @-> returning uint16_t))

  let game_controller_mapping =
    F.(foreign "SDL_GameControllerMapping"
         (ptr Types.game_controller @-> returning const_string_opt))

  let game_controller_mapping_for_index =
    F.(foreign "SDL_GameControllerMappingForIndex"
         (int @-> returning const_string_opt))

  let game_controller_mapping_for_guid =
    F.(foreign "SDL_GameControllerMappingForGUID"
         (Types.joystick_guid @-> returning const_string_opt))

  let game_controller_name =
    F.(foreign "SDL_GameControllerName"
         (ptr Types.game_controller @-> returning const_string_opt))

  let game_controller_name_for_index =
    F.(foreign "SDL_GameControllerNameForIndex"
         (int @-> returning const_string_opt))

  let game_controller_num_mappings =
    F.(foreign "SDL_GameControllerNumMappings" (void @-> returning int))

  let game_controller_open =
    F.(foreign "SDL_GameControllerOpen"
         (int @-> returning (ptr_opt Types.game_controller)))

  let game_controller_update =
    F.(foreign "SDL_GameControllerUpdate" (void @-> returning void))

  let is_game_controller =
    F.(foreign "SDL_IsGameController" (int @-> returning bool))

  let event_state =
    F.(foreign "SDL_EventState" (uint32_t @-> int @-> returning uint8_t))

  let flush_event =
    F.(foreign "SDL_FlushEvent" (uint32_t @-> returning void))

  let flush_events =
    F.(foreign "SDL_FlushEvents" (uint32_t @-> uint32_t @-> returning void))

  let has_event =
    F.(foreign "SDL_HasEvent" (uint32_t @-> returning bool))

  let has_events =
    F.(foreign "SDL_HasEvents" (uint32_t @-> uint32_t @-> returning bool))

  let poll_event =
    F.(foreign "SDL_PollEvent" (ptr Types.Event.t @-> returning bool))

  let pump_events =
    F.(foreign "SDL_PumpEvents" (void @-> returning void))

  let push_event =
    F.(foreign "SDL_PushEvent" (ptr Types.Event.t @-> returning int))

  let register_events =
    F.(foreign "SDL_RegisterEvents" (int @-> returning uint32_t))

  (* Force feedback *)

  let haptic_close =
    F.(foreign "SDL_HapticClose" (ptr Types.Haptic.t @-> returning void))

  let haptic_destroy_effect =
    F.(foreign "SDL_HapticDestroyEffect"
         (ptr Types.Haptic.t @-> int @-> returning void))

  let haptic_effect_supported =
    F.(foreign "SDL_HapticEffectSupported"
         (ptr Types.Haptic.t @-> ptr Types.Haptic.Effect.t @-> returning int))

  let haptic_get_effect_status =
    F.(foreign "SDL_HapticGetEffectStatus"
         (ptr Types.Haptic.t @-> int @-> returning int))

  let haptic_index =
    F.(foreign "SDL_HapticIndex" (ptr Types.Haptic.t @-> returning int))

  let haptic_name =
    F.(foreign "SDL_HapticName" (int @-> returning const_string_opt))

  let haptic_new_effect =
    F.(foreign "SDL_HapticNewEffect"
         (ptr Types.Haptic.t @-> ptr Types.Haptic.Effect.t @-> returning int))

  let haptic_num_axes =
    F.(foreign "SDL_HapticNumAxes" (ptr Types.Haptic.t @-> returning int))

  let haptic_num_effects =
    F.(foreign "SDL_HapticNumEffects" (ptr Types.Haptic.t @-> returning int))

  let haptic_num_effects_playing =
    F.(foreign "SDL_HapticNumEffectsPlaying" (ptr Types.Haptic.t @-> returning int))

  let haptic_open =
    F.(foreign "SDL_HapticOpen" (int @-> returning (ptr_opt Types.Haptic.t)))

  let haptic_open_from_joystick =
    F.(foreign "SDL_HapticOpenFromJoystick"
         (ptr Types.joystick @-> returning (ptr_opt Types.Haptic.t)))

  let haptic_open_from_mouse =
    F.(foreign "SDL_HapticOpenFromMouse"
         (void @-> returning (ptr_opt Types.Haptic.t)))

  let haptic_opened =
    F.(foreign "SDL_HapticOpened" (int @-> returning int))

  let haptic_pause =
    F.(foreign "SDL_HapticPause" (ptr Types.Haptic.t @-> returning int))

  let haptic_query =
    F.(foreign "SDL_HapticQuery" (ptr Types.Haptic.t @-> returning int))

  let haptic_rumble_init =
    F.(foreign "SDL_HapticRumbleInit" (ptr Types.Haptic.t @-> returning int))

  let haptic_rumble_play =
    F.(foreign "SDL_HapticRumblePlay"
         (ptr Types.Haptic.t @-> float @-> int32_t @-> returning int))

  let haptic_rumble_stop =
    F.(foreign "SDL_HapticRumbleStop" (ptr Types.Haptic.t @-> returning int))

  let haptic_rumble_supported =
    F.(foreign "SDL_HapticRumbleSupported" (ptr Types.Haptic.t @-> returning int))

  let haptic_run_effect =
    F.(foreign "SDL_HapticRunEffect"
         (ptr Types.Haptic.t @-> int  @-> int32_t @-> returning int))

  let haptic_set_autocenter =
    F.(foreign "SDL_HapticSetAutocenter" (ptr Types.Haptic.t @-> int @-> returning int))

  let haptic_set_gain =
    F.(foreign "SDL_HapticSetGain" (ptr Types.Haptic.t @-> int @-> returning int))

  let haptic_stop_all =
    F.(foreign "SDL_HapticStopAll" (ptr Types.Haptic.t @-> returning int))

  let haptic_stop_effect =
    F.(foreign "SDL_HapticStopEffect"
         (ptr Types.Haptic.t @-> int @-> returning int))

  let haptic_unpause =
    F.(foreign "SDL_HapticUnpause" (ptr Types.Haptic.t @-> returning int))

  let haptic_update_effect =
    F.(foreign "SDL_HapticUpdateEffect"
         (ptr Types.Haptic.t @-> int @-> ptr Types.Haptic.Effect.t @->
          returning int))

  let joystick_is_haptic =
    F.(foreign "SDL_JoystickIsHaptic" (ptr Types.joystick @-> returning int))

  let mouse_is_haptic =
    F.(foreign "SDL_MouseIsHaptic" (void @-> returning int))

  let num_haptics =
    F.(foreign "SDL_NumHaptics" (void @-> returning int))

  (* Audio *)

  (* Audio drivers *)

  let audio_init =
    F.(foreign "SDL_AudioInit" (const_string_opt @-> returning int))

  let audio_quit =
    F.(foreign "SDL_AudioQuit" (void @-> returning void))

  let get_audio_driver =
    F.(foreign "SDL_GetAudioDriver" (int @-> returning const_string_opt))

  let get_current_audio_driver =
    F.(foreign "SDL_GetCurrentAudioDriver"
         (void @-> returning const_string_opt))

  let get_num_audio_drivers =
    F.(foreign "SDL_GetNumAudioDrivers" (void @-> returning int))

  (* Audio devices *)

  let close_audio_device =
    F.(foreign "SDL_CloseAudioDevice" (uint32_t @-> returning void))

  let free_wav =
    F.(foreign "SDL_FreeWAV" (ptr void @-> returning void))

  let get_audio_device_name =
    F.(foreign "SDL_GetAudioDeviceName"
         (int @-> bool @-> returning const_string_opt))

  let get_audio_device_status =
    F.(foreign "SDL_GetAudioDeviceStatus" (uint32_t @-> returning int))

  let get_num_audio_devices =
    F.(foreign "SDL_GetNumAudioDevices" (bool @-> returning int))

  let lock_audio_device =
    F.(foreign "SDL_LockAudioDevice" (uint32_t @-> returning void))

  let open_audio_device =
    F.(foreign "SDL_OpenAudioDevice"
         (const_string_opt @-> bool @-> ptr Types.audio_spec @->
          ptr Types.audio_spec @-> int @-> returning uint32_t))

  let pause_audio_device =
    F.(foreign "SDL_PauseAudioDevice" (uint32_t @-> bool @-> returning void))

  let unlock_audio_device =
    F.(foreign "SDL_UnlockAudioDevice" (uint32_t @-> returning void))

  let queue_audio =
    F.(foreign "SDL_QueueAudio"
         (uint32_t @-> ptr void @-> uint32_t @-> returning int))

  let dequeue_audio =
    F.(foreign "SDL_DequeueAudio"
         (uint32_t @-> ptr void @-> int @-> returning uint32_t))

  let get_queued_audio_size =
    F.(foreign "SDL_GetQueuedAudioSize" (uint32_t @-> returning uint32_t))

  let clear_queued_audio =
    F.(foreign "SDL_ClearQueuedAudio" (uint32_t @-> returning void))

  (* Timer *)

  let get_ticks =
    F.(foreign "SDL_GetTicks" (void @-> returning int32_t))

  let get_ticks64 =
    F.(foreign "SDL_GetTicks64" (void @-> returning int64_t))

  let get_performance_counter =
    F.(foreign "SDL_GetPerformanceCounter" (void @-> returning int64_t))

  let get_performance_frequency =
    F.(foreign "SDL_GetPerformanceFrequency" (void @-> returning int64_t))

  (* Platform and CPU information *)

  let get_platform =
    F.(foreign "SDL_GetPlatform" (void @-> returning string))

  let get_cpu_cache_line_size =
    F.(foreign "SDL_GetCPUCacheLineSize" (void @-> returning int))

  let get_cpu_count =
    F.(foreign "SDL_GetCPUCount" (void @-> returning int))

  let get_system_ram =
    F.(foreign "SDL_GetSystemRAM" (void @-> returning int))

  let has_3d_now =
    F.(foreign "SDL_Has3DNow" (void @-> returning bool))

  let has_altivec =
    F.(foreign "SDL_HasAltiVec" (void @-> returning bool))

  let has_avx =
    F.(foreign "SDL_HasAVX" (void @-> returning bool))

  let has_avx2 =
    F.(foreign  "SDL_HasAVX2" (void @-> returning bool))

  let has_mmx =
    F.(foreign "SDL_HasMMX" (void @-> returning bool))

  let has_neon =
    F.(foreign "SDL_HasNEON" (void @-> returning bool))

  let has_rdtsc =
    F.(foreign "SDL_HasRDTSC" (void @-> returning bool))

  let has_sse =
    F.(foreign "SDL_HasSSE" (void @-> returning bool))

  let has_sse2 =
    F.(foreign "SDL_HasSSE2" (void @-> returning bool))

  let has_sse3 =
    F.(foreign "SDL_HasSSE3" (void @-> returning bool))

  let has_sse41 =
    F.(foreign "SDL_HasSSE41" (void @-> returning bool))

  let has_sse42 =
    F.(foreign "SDL_HasSSE42" (void @-> returning bool))

  let get_power_info =
    F.(foreign "SDL_GetPowerInfo" ((ptr int) @-> (ptr int) @-> returning int))
end
