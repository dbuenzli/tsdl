(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tsdl tests, should exit with 0. *)

open Tsdl;;
open Result;;

(* Logging *)

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf (fmt ^^ "@.")

(* Bigarrays *)

let create_bigarray k len = Bigarray.Array1.create k Bigarray.c_layout len

(* Tests. *)

let test_init () =
  log "Testing initialization";
  let flags = Sdl.Init.(video + audio) in
  assert (Sdl.Init.eq (Sdl.was_init (Some flags)) flags);
  ()

let test_hints () =
  log "Testing hints";
  assert (Sdl.set_hint Sdl.Hint.framebuffer_acceleration "1");
  assert (Sdl.get_hint Sdl.Hint.framebuffer_acceleration = (Some "1"));
  let unique_hint = "ef0386b3-a8c3-4200-9393-f6b6bd9a4194" in
  assert (Sdl.get_hint unique_hint = None);
  assert (Sdl.set_hint_with_priority unique_hint "bli" Sdl.Hint.override);
  assert (Sdl.get_hint unique_hint = Some "bli");
  Sdl.clear_hints ();
  assert (Sdl.get_hint unique_hint = None);
  ()

let test_error () =
  log "Testing errors";
  assert (Sdl.get_error () = "");
  Sdl.set_error "Ho %a" Format.pp_print_int 3;
  assert (Sdl.get_error () = "Ho 3");
  Sdl.clear_error ();
  assert (Sdl.get_error () = "");
  ()

let test_log () =
  let cmp_priority c p = Sdl.(Log.priority_compare (log_get_priority c) p) in
  log "Testing logging";
  Sdl.log "Escape %%s";
  Sdl.log "1 @[%a@]" Format.pp_print_string "info";
  Sdl.(log_critical Log.category_application "2 Pif critical");
  Sdl.(log_debug Log.category_video "Hey silent debug");
  assert (cmp_priority Sdl.Log.category_video Sdl.Log.priority_debug > 0);
  Sdl.log_set_priority Sdl.Log.category_video Sdl.Log.priority_debug;
  assert (cmp_priority Sdl.Log.category_video Sdl.Log.priority_debug = 0);
  Sdl.(log_debug Log.category_video "3 show debug");
  Sdl.(log_error Log.category_application "4 error");
  Sdl.(log_info Log.category_application "5 info");
  Sdl.(log_message Log.category_application Log.priority_info "6 info");
  let paudio = Sdl.(log_get_priority Log.category_audio) in
  Sdl.(log_set_all_priority Log.priority_verbose);
  assert (cmp_priority Sdl.Log.category_audio Sdl.Log.priority_verbose = 0);
  Sdl.(log_reset_priorities ());
  assert (cmp_priority Sdl.Log.category_audio paudio = 0);
  Sdl.(log_set_priority Log.category_application Log.priority_verbose);
  Sdl.(log_verbose Log.category_application "7 verbose");
  Sdl.(log_warn Log.category_application "8 warn");
  ()

let test_version () =
  log "Testing version";
  let min, maj, patch = Sdl.get_version () in
  log " Version: SDL %d.%d.%d" min maj patch;
  log " Revision: %s" (Sdl.get_revision ());
  log " Revision number: %d" (Sdl.get_revision_number ());
  ()

let test_rw_ops () =
  log "Testing IO abstraction";
  let file = "/tmp/bla" in
  begin match Sdl.rw_from_file "/tmp/bli" "wb" with
  | Error (`Msg e) -> log_err " Could not get IO abstraction from %s: %s" file e
  | Ok rw ->
      begin match Sdl.rw_close rw with
      | Error (`Msg e) -> log_err " Could not close IO abstraction: %s" e
      | Ok () -> ()
      end
  end

let test_file_system_paths () =
  log "Testing filesystem paths functions";
  begin match Sdl.get_base_path () with
  | Error (`Msg e) -> log " Could not get base path: %s" e
  | Ok path -> log " base path: %s" path
  end;
  begin match Sdl.get_pref_path ~org:"ch.erratique" ~app:"tsdl-test" with
  | Error (`Msg e) -> log " Could not get pref path: %s" e
  | Ok path -> log " pref path: %s" path
  end;
  ()

let test_colors () =
  log "Testing colors";
  let c = Sdl.Color.create 1 2 3 4 in
  assert (Sdl.Color.r c = 1);
  assert (Sdl.Color.g c = 2);
  assert (Sdl.Color.b c = 3);
  assert (Sdl.Color.a c = 4);
  ()

let test_points () =
  log "Testing points";
  let p = Sdl.Point.create 1 2 in
  assert (Sdl.Point.x p = 1);
  assert (Sdl.Point.y p = 2);
  ()

let test_rectangles () =
  log "Testing rectangles";
  let r = Sdl.Rect.create 1 2 3 4 in
  assert (Sdl.Rect.x r = 1);
  assert (Sdl.Rect.y r = 2);
  assert (Sdl.Rect.w r = 3);
  assert (Sdl.Rect.h r = 4);
  let bound = Sdl.Rect.create 1 2 2 3 in
  let ba = create_bigarray Bigarray.int32 4 in
  ba.{0} <- 1l; ba.{1} <- 2l;
  ba.{2} <- 2l; ba.{3} <- 4l;
  begin match Sdl.enclose_points_ba ba with
  | None -> assert false
  | Some r -> assert (Sdl.rect_equals bound r)
  end;
  let o = Sdl.Point.create 0 0 in
  let p0 = Sdl.Point.create 1 2 in
  let p1 = Sdl.Point.create 2 4 in
  let p2 = Sdl.Point.create 3 5 in
  let a = match Sdl.enclose_points [p0; p1] with
  | None -> assert false
  | Some r -> assert (Sdl.rect_equals bound r); r
  in
  ignore (Sdl.enclose_points []);
  begin match Sdl.enclose_points ~clip:a [p0; p1; p2] with
  | None -> assert false
  | Some r -> assert (Sdl.rect_equals r a)
  end;
  begin match Sdl.enclose_points ~clip:a [o] with
  | None -> assert true | Some _ -> assert false
  end;
  assert (not (Sdl.rect_empty a));
  assert (Sdl.rect_empty (Sdl.Rect.create 1 2 0 3));
  let b = Sdl.Rect.create 0 0 3 3 in
  assert (Sdl.has_intersection a b);
  begin match Sdl.intersect_rect a b with
  | None -> assert false
  | Some r -> assert (Sdl.rect_equals r (Sdl.Rect.create 1 2 2 1))
  end;
  assert (Sdl.rect_equals (Sdl.union_rect a b) (Sdl.Rect.create 0 0 3 5));
  begin match Sdl.intersect_rect_and_line a (-1) 0 0 2 with
  | None -> assert true | Some _ -> assert false
  end;
  begin match Sdl.intersect_rect_and_line b (-1) 0 4 5 with
  | Some ((0, 1), (1, 2)) -> assert true
  | None | Some _ -> assert false
  end;
  ()

let test_palettes () =
  log "Testing palettes";
  let eq_col c0 c1 =
    Sdl.Color.(r c0 = r c1 && g c0 = g c1 && b c0 = b c1 && a c0 = a c1)
  in
  let r = Sdl.Color.create 255 0 0 255 in
  let g = Sdl.Color.create 0 255 0 255 in
  let b = Sdl.Color.create 0 0 255 255 in
  let cs = [r; g; b] in
  match Sdl.alloc_palette 3 with
  | Error (`Msg e) -> log " Error while creating palette: %s" e
  | Ok p ->
      assert (Sdl.get_palette_ncolors p = 3);
      begin match Sdl.set_palette_colors p cs ~fst:0 with
      | Error (`Msg e) -> log_err " Could not set palette colors: %s" e
      | Ok () ->
          assert (List.for_all2 eq_col cs (Sdl.get_palette_colors p));
          let ba = Sdl.get_palette_colors_ba p in
          assert(ba.{0} = 255 && ba.{1} = 0 && ba.{2} = 0 && ba.{3} = 255 &&
                 ba.{4} = 0 && ba.{5} = 255 && ba.{6} = 0 && ba.{7} = 255 &&
                 ba.{8} = 0 && ba.{9} = 0 && ba.{10} = 255 && ba.{11} = 255);
          for i = 0 to Bigarray.Array1.dim ba - 1 do ba.{i} <- ba.{i} / 2 done;
          begin match Sdl.set_palette_colors_ba p ba ~fst:0 with
          | Error (`Msg e) -> log_err " Could not set palette colors: %s" e;
          | Ok () ->
              let ba' = Sdl.get_palette_colors_ba p in
              for i = 0 to Bigarray.Array1.dim ba' - 1 do
                assert (ba'.{i} = ba.{i})
              done;
              Sdl.free_palette p;
              ba'.{0} <- 4; (* We don't expose the palette's buffer *)
          end;
      end;
      ()

let test_pixel_formats () =
  log "Testing pixel formats";
  let gramp = Sdl.calculate_gamma_ramp 1.0 in
  assert (Bigarray.Array1.dim gramp = 256);
  assert (gramp.{0} = 0);
  assert (gramp.{255} = 65535);
  assert (Sdl.get_pixel_format_name Sdl.Pixel.format_rgba8888 =
          "SDL_PIXELFORMAT_RGBA8888");
  begin match Sdl.alloc_format Sdl.Pixel.format_rgba8888 with
  | Error (`Msg e) -> log_err " Could not alloc format: %s" e
  | Ok pf ->
      assert (Sdl.Pixel.eq
                (Sdl.get_pixel_format_format pf)
                (Sdl.Pixel.format_rgba8888));
      assert (Sdl.get_pixel_format_bits_pp pf = 32);
      assert (Sdl.get_pixel_format_bytes_pp pf = 4);
      assert (Sdl.get_rgb pf 0xAABBCCDDl = (0xAA, 0xBB, 0xCC));
      assert (Sdl.get_rgba pf 0xAABBCCDDl = (0xAA, 0xBB, 0xCC, 0xDD));
      assert (Sdl.map_rgb pf 0xAA 0xBB 0xCC = 0xAABBCCFFl);
      assert (Sdl.map_rgba pf 0xAA 0xBB 0xCC 0xDD = 0xAABBCCDDl);
      Sdl.free_format pf;
  end;
  begin match Sdl.pixel_format_enum_to_masks Sdl.Pixel.format_argb8888 with
  | Error (`Msg e) -> log_err " Could not get pixel format masks: %s" e
  | Ok (bpp, rm, gm, bm, am) ->
      let bpp', rm', gm', bm', am' =
        32, 0x00FF0000l, 0x0000FF00l, 0x000000FFl, 0xFF000000l
      in
      assert (Sdl.Pixel.eq
                (Sdl.masks_to_pixel_format_enum bpp' rm' gm' bm' am')
                Sdl.Pixel.format_argb8888);
  end;
  begin match Sdl.alloc_format Sdl.Pixel.format_index8 with
  | Error (`Msg e) -> log_err " Could not alloc format: %s" e
  | Ok pf ->
      begin match Sdl.alloc_palette 256 with
      | Error (`Msg e) -> log_err " Could not alloc palette: %s" e
      | Ok p ->
          let r = Sdl.Color.create 0xFF 0x00 0x00 0xFF in
          let g = Sdl.Color.create 0x00 0xFF 0x00 0xFF in
          let b = Sdl.Color.create 0x00 0x00 0xFF 0xFF in
          let cs = [r; g; b] in
          begin match Sdl.set_palette_colors p cs ~fst:0 with
          | Error (`Msg e) -> log_err " Could not set palette colors: %s" e
          | Ok () ->
              begin match Sdl.set_pixel_format_palette pf p with
              | Error (`Msg e) ->
                  log_err " Could not set pixel format palette: %s" e
              | Ok () -> ();
              end;
              Sdl.free_palette p;
              assert (Sdl.Pixel.eq
                        (Sdl.get_pixel_format_format pf)
                        (Sdl.Pixel.format_index8));
              assert (Sdl.get_pixel_format_bits_pp pf = 8);
              assert (Sdl.get_pixel_format_bytes_pp pf = 1);
              assert (Sdl.get_rgb pf 0l = (0xFF, 0x00, 0x00));
              assert (Sdl.get_rgba pf 0l = (0xFF, 0x00, 0x00, 0xFF));
              assert (Sdl.map_rgb pf 0x00 0xFF 0x00 = 1l);
              assert (Sdl.map_rgba pf 0x00 0xFF 0x00 0xFF = 1l);
          end;
          Sdl.free_format pf;
      end;
  end;
  ()

let test_surfaces () =
  log "Testing surfaces";
  begin match Sdl.create_rgb_surface ~w:256 ~h:256 ~depth:32
                0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
  with
  | Error (`Msg e) -> log_err " Could not create surface: %s" e
  | Ok s0 ->
      assert (Sdl.rect_equals (Sdl.get_clip_rect s0)
                (Sdl.Rect.create 0 0 256 256));
      let r = Sdl.Rect.create 0 0 10 10 in
      assert (Sdl.set_clip_rect s0 r);
      assert (Sdl.rect_equals (Sdl.get_clip_rect s0) r);
      assert (match Sdl.get_color_key s0 with Error _ -> true | _ -> false);
      assert (Sdl.set_color_key s0 true 0xFF000000l = Ok ());
      assert (Sdl.get_color_key s0 = Ok 0xFF000000l);
      assert (Sdl.get_surface_alpha_mod s0 = Ok 0xFF);
      assert (Sdl.set_surface_alpha_mod s0 0x7F = Ok ());
      assert (Sdl.get_surface_alpha_mod s0 = Ok 0x7F);
      assert (Sdl.get_surface_blend_mode s0 = Ok Sdl.Blend.mode_blend);
      assert (Sdl.set_surface_blend_mode s0 Sdl.Blend.mode_add = Ok ());
      assert (Sdl.get_surface_blend_mode s0 = Ok Sdl.Blend.mode_add);
      assert (Sdl.get_surface_color_mod s0 = Ok (0xFF, 0xFF, 0xFF));
      assert (Sdl.set_surface_color_mod s0 0xAA 0xBB 0xCC = Ok ());
      assert (Sdl.get_surface_color_mod s0 = Ok (0xAA, 0xBB, 0xCC));
      ignore (Sdl.get_surface_pitch s0); (* value may dependent on platform *)
      assert (Sdl.get_surface_size s0 = (256, 256));
      assert (Sdl.get_surface_format_enum s0 = Sdl.Pixel.format_rgba8888);
      assert (Sdl.set_surface_rle s0 true = Ok ());
      begin match Sdl.convert_surface_format s0 Sdl.Pixel.format_argb8888 with
      | Error (`Msg e) -> log_err " Could not convert surface: %s" e
      | Ok s1 ->
          let r0 = Sdl.Rect.create 0 0 10 10 in
          let r1 = Sdl.Rect.create 2 3 5 5 in
          let ba = create_bigarray Bigarray.int32 8 in
          assert (Sdl.fill_rect s0 (Some r0) 0xFF000000l = Ok ());
          assert (Sdl.fill_rects s0 [r0; r1] 0xFF000000l = Ok ());
          assert (Sdl.fill_rects s0 [] 0xFF000000l = Ok ());
          ba.{0} <- 5l; ba.{1} <- 6l; ba.{2} <- 3l; ba.{3} <- 4l;
          ba.{4} <- 10l; ba.{5} <- 10l; ba.{6} <- 6l; ba.{7} <- 7l;
          assert (Sdl.fill_rects_ba s0 ba 0xFF000000l = Ok ());
          assert (Sdl.blit_scaled ~src:s0 r0 ~dst:s1 (Some r1) = Ok ());
          assert (Sdl.blit_scaled ~src:s0 r0 ~dst:s1 None = Ok ());
          assert (Sdl.blit_surface ~src:s0 (Some r0) ~dst:s1 r1 = Ok ());
          assert (Sdl.lower_blit ~src:s0 r0 ~dst:s1 r1 = Ok ());
          assert (Sdl.lower_blit_scaled ~src:s0 r0 ~dst:s1 r1 = Ok ());
          Sdl.free_surface s1
      end;
      begin match Sdl.alloc_format Sdl.Pixel.format_index8 with
      | Error (`Msg e) -> log_err " Could not alloc format: %s" e
      | Ok pf ->
          begin match Sdl.convert_surface s0 pf with
          | Error (`Msg e) -> log_err " Could not convert surface: %s" e
          | Ok s1 ->
              begin match Sdl.alloc_palette 256 with
              | Error (`Msg e) -> log_err " Could not alloc palette: %s" e
              | Ok p ->
                  assert (Sdl.set_surface_palette s1 p = Ok ());
                  Sdl.free_palette p;
                  Sdl.free_surface s1
              end;
          end;
      end;
      assert (Sdl.lock_surface s0 = Ok ());
      let ba = Sdl.get_surface_pixels s0 Bigarray.int32 in
      assert (Bigarray.Array1.dim ba = 256 * 256);
      ba.{0} <- 0xFF0000FFl;
      Sdl.unlock_surface s0;
      begin match Sdl.save_bmp s0 "/tmp/bla.bmp" with
      | Error (`Msg e) -> log_err " Could not save bmp: %s" e
      | Ok () ->
          begin match Sdl.load_bmp "/tmp/bla.bmp" with
          | Error (`Msg e) -> log_err " Could not load bmp: %s" e
          | Ok s ->
              let ba = Sdl.get_surface_pixels s Bigarray.int8_unsigned in
              assert (Sdl.get_surface_format_enum s =
                      Sdl.Pixel.format_argb8888);
              let hi, lo = if Sys.big_endian then 0xFF, 0x00 else 0x00, 0xFF in
              assert (ba.{0} = hi);
              assert (ba.{1} = hi);
              assert (ba.{2} = lo);
              assert (ba.{3} = lo);
          end;
      end;
      Sdl.free_surface s0;
  end;
  let pixels = create_bigarray Bigarray.int32 (256 * 256) in
  begin match Sdl.create_rgb_surface_from pixels ~w:256 ~h:256 ~depth:32
                ~pitch:256 0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
  with
  | Error (`Msg e) -> log_err " Could not create surface: %s" e
  | Ok s ->
      assert (Sdl.get_surface_format_enum s = Sdl.Pixel.format_rgba8888);
      assert (Sdl.get_surface_pitch s = 1024);
      Sdl.free_surface s
  end;
  let pixels_16 = create_bigarray Bigarray.int16_unsigned (256 * 256) in
  begin match
    Sdl.convert_pixels ~w:256 ~h:256
      ~src:Sdl.Pixel.format_rgba8888 pixels 256
      ~dst:Sdl.Pixel.format_rgba5551 pixels_16 256
  with
  | Error (`Msg e) -> log_err " Could not convert pixels: %s" e
  | Ok () -> ()
  end;
  ()

let test_renderers () =
  log "Testing renderers";
  begin match Sdl.get_num_render_drivers () with
  | Error (`Msg e) -> log_err " Could not get number of render drivers: %s" e
  | Ok count ->
      for n = 0 to count - 1 do
        begin match Sdl.get_render_driver_info n with
        | Error (`Msg e) ->
            log_err " Could not driver info for driver %d: %s" n e
        | Ok i ->
            log " Driver %d @[%a@]" n Fmts.pp_render_info i
        end
      done
  end;
  let w_props = Sdl.Window.opengl in
  begin match Sdl.create_window "Renderer test" ~w:640 ~h:480 w_props with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      assert (match Sdl.get_renderer w with Error _ -> true | _ -> false);
      begin match Sdl.create_renderer w with
      | Error (`Msg e) -> log_err " Could not create renderer: %s" e
      | Ok r ->
          assert (match Sdl.get_renderer w with Error _ -> false | _ -> true);
          begin match Sdl.get_renderer_info r with
          | Error (`Msg e) -> log_err " Could not get renderer info: %s" e
          | Ok i -> log " Renderer @[%a@]" Fmts.pp_render_info i
          end;
          begin match Sdl.get_renderer_output_size r with
          | Error (`Msg e) ->
              log_err " Could not get renderer output size: %s" e
          | Ok (w,h) -> log " Renderer output size: %dx%d" w h
          end;
          log " Render target supported: %b" (Sdl.render_target_supported r);
          let rect = Sdl.Rect.create 20 30 20 20 in
          assert (Sdl.render_set_clip_rect r (Some rect) = Ok ());
          assert (Sdl.rect_equals (Sdl.render_get_clip_rect r) rect);
          assert (Sdl.render_set_clip_rect r None = Ok ());
          assert (Sdl.render_set_logical_size r 320 240 = Ok ());
          assert (Sdl.render_get_logical_size r = (320, 240));
          assert (Sdl.render_get_scale r = (2., 2.));
          assert (Sdl.render_set_scale r 1. 1. = Ok ());
          let vp = Sdl.Rect.create 0 0 320 240 in
          assert (Sdl.render_set_viewport r (Some vp) = Ok ());
          assert (Sdl.rect_equals (Sdl.render_get_viewport r)
                    (Sdl.Rect.create 0 0 320 240));
          assert (Sdl.render_set_viewport r None = Ok ());
          assert (Sdl.set_render_draw_blend_mode r Sdl.Blend.mode_add = Ok ());
          assert (Sdl.get_render_draw_blend_mode r = Ok Sdl.Blend.mode_add);
          assert (Sdl.set_render_draw_blend_mode r Sdl.Blend.mode_blend =Ok());
          assert (Sdl.set_render_draw_color r 0x50 0xC8 0x78 0xFF = Ok ());
          assert (Sdl.get_render_draw_color r = Ok (0x50, 0xC8, 0x78, 0xFF));
          assert (Sdl.render_clear r = Ok ());
          assert (Sdl.set_render_draw_color r 0x00 0x00 0x00 0xFF = Ok ());
          assert (Sdl.render_draw_point r 5 5 = Ok ());
          assert (Sdl.render_draw_line r 10 10 100 100 = Ok ());
          assert (Sdl.render_fill_rect r (Some rect) = Ok ());
          assert (Sdl.set_render_draw_color r 0xFF 0xFF 0xFF 0xFF = Ok ());
          assert (Sdl.render_draw_rect r (Some rect) = Ok ());
          let pts = [Sdl.Point.create 100 100; Sdl.Point.create 100 200;
                     Sdl.Point.create 200 200; ]
          in
          let pts_ba = create_bigarray Bigarray.int32 6 in
          pts_ba.{0} <- 20l; pts_ba.{1} <- 20l;
          pts_ba.{2} <- 30l; pts_ba.{3} <- 20l;
          pts_ba.{4} <- 30l; pts_ba.{5} <- 30l;
          assert (Sdl.render_draw_lines r pts =Ok ());
          assert (Sdl.render_draw_lines_ba r pts_ba =Ok ());
          assert (Sdl.set_render_draw_color r 0xFF 0x00 0x00 0xFF = Ok ());
          assert (Sdl.render_draw_points r pts =Ok ());
          assert (Sdl.render_draw_points_ba r pts_ba =Ok ());
          let rects = [Sdl.Rect.create 120 30 45 60;
                       Sdl.Rect.create 150 40 56 57]
          in
          let rects_ba = create_bigarray Bigarray.int32 8 in
          rects_ba.{0} <- 200l; rects_ba.{1} <- 30l;
          rects_ba.{2} <- 45l; rects_ba.{3} <- 60l;
          rects_ba.{4} <- 230l; rects_ba.{5} <- 40l;
          rects_ba.{6} <- 56l; rects_ba.{7} <- 57l;
          assert (Sdl.render_fill_rects r rects = Ok ());
          assert (Sdl.render_fill_rects_ba r rects_ba = Ok ());
          assert (Sdl.set_render_draw_color r 0xFF 0xFF 0xFF 0xFF = Ok ());
          assert (Sdl.render_draw_rects r rects = Ok ());
          assert (Sdl.render_draw_rects_ba r rects_ba = Ok ());
          Sdl.render_present r;
          assert (Sdl.get_render_target r = None);
          assert (Sdl.set_render_target r None = Ok ());
          Sdl.destroy_renderer r
      end;
      Sdl.destroy_window w
  end;
  ()

let test_textures () =
  log "Testing textures";
  let w_props = Sdl.Window.(opengl + shown) in
  begin match Sdl.create_window_and_renderer ~w:640 ~h:480 w_props with
  | Error (`Msg e) -> log_err " Could not create window and renderer: %s" e
  | Ok (w, r) ->
      begin match Sdl.create_texture r Sdl.Pixel.format_iyuv ~w:256 ~h:256
                    Sdl.Texture.access_streaming
      with
      | Error (`Msg e) -> log_err " Could not create texture: %s" e
      | Ok t ->
          assert (Sdl.query_texture t =
                  Ok (Sdl.Pixel.format_iyuv, Sdl.Texture.access_streaming,
                       (256, 256)));
          assert (Sdl.get_texture_alpha_mod t = Ok 0xFF);
          assert (Sdl.set_texture_alpha_mod t 0x7F = Ok ());
          assert (Sdl.get_texture_alpha_mod t = Ok 0x7F);
          assert (Sdl.get_texture_blend_mode t = Ok Sdl.Blend.mode_none);
          assert (Sdl.set_texture_blend_mode t Sdl.Blend.mode_add = Ok ());
          assert (Sdl.get_texture_blend_mode t = Ok Sdl.Blend.mode_add);
          assert (Sdl.get_texture_color_mod t = Ok (0xFF, 0xFF, 0xFF));
          assert (Sdl.set_texture_color_mod t 0xAA 0xBB 0xCC = Ok ());
          assert (Sdl.get_texture_color_mod t = Ok (0xAA, 0xBB, 0xCC));
          let p = create_bigarray Bigarray.int8_unsigned (256 * 256) in
          assert (Sdl.update_yuv_texture t None ~y:p 256 ~u:p 256 ~v:p 256 =
                  Ok ());
          assert (Sdl.render_clear r = Ok ());
          assert (Sdl.render_copy ~dst:(Sdl.Rect.create 10 10 256 256) r t =
                  Ok ());
          assert (Sdl.render_copy_ex ~dst:(Sdl.Rect.create 150 150 256 256) r t
                    45. None Sdl.Flip.none = Ok ());
          begin match Sdl.lock_texture t None Bigarray.int8_unsigned with
          | Error (`Msg e) -> log_err " Could not lock texture: %s" e
          | Ok (ba, pitch) ->
              assert (Bigarray.Array1.dim ba = pitch * 256);
              assert (ba.{0} = 0x00);
              Sdl.unlock_texture t
          end;
          Sdl.render_present r;
          Sdl.destroy_texture t
      end;
      begin match Sdl.create_rgb_surface 50 50 32
                    0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
      with
      | Error (`Msg e) -> log_err " Could not create surface: %s" e
      | Ok s ->
          assert(Sdl.fill_rect s None 0xFF0000FFl = Ok ());
          begin match Sdl.create_texture_from_surface r s with
          | Error (`Msg e) -> log_err " Could not create texture: %s" e
          | Ok t ->
          assert (Sdl.query_texture t =
                  Ok (Sdl.Pixel.format_argb8888, Sdl.Texture.access_static,
                       (50, 50)));
          assert (Sdl.render_clear r = Ok ());
          assert (Sdl.render_copy ~dst:(Sdl.Rect.create 100 100 50 50) r t =
                  Ok ());
          Sdl.render_present r;
          let ba = create_bigarray Bigarray.int32 (50 * 50) in
          for i = 0 to 50 * 50 - 1 do ba.{i} <- 0xFF00FF00l done;
          begin match Sdl.update_texture t None ba 50 with
          | Error (`Msg e) -> log_err " Could not update texture: %s" e
          | Ok () ->
              assert (Sdl.render_clear r = Ok ());
              assert (Sdl.render_copy ~dst:(Sdl.Rect.create 200 200 50 50) r t
                      = Ok ());
              Sdl.render_present r;
          end;
          Sdl.destroy_texture t;
          end;
          Sdl.free_surface s;
      end;
      begin match Sdl.create_texture r Sdl.Pixel.format_rgba8888 ~w:50 ~h:50
                    Sdl.Texture.access_target
      with
      | Error (`Msg e) -> log_err " Could not create texture: %s" e
      | Ok t ->
          begin match Sdl.set_render_target r (Some t) with
          | Error (`Msg e) -> log_err "Could not set render target: %s" e
          | Ok () ->
              assert (Sdl.set_render_draw_color r 0x50 0xC8 0x78 0xFF = Ok ());
              assert (Sdl.render_clear r = Ok ());
              Sdl.render_present r;
              let pixels = create_bigarray Bigarray.int32 (50 * 50) in
              begin match Sdl.render_read_pixels r None
                            (Some Sdl.Pixel.format_rgba8888) pixels (50 * 4)
              with
              | Error (`Msg e) -> log_err " Could not read pixels: %s" e
              | Ok () -> assert (pixels.{0} = 0x50C878FFl);
              end;
          end;
          Sdl.destroy_texture t
      end;
      Sdl.destroy_renderer r;
      Sdl.destroy_window w;
  end;
  ()

let test_video_drivers () =
  log "Testing video drivers";
  let driver = Sdl.get_current_video_driver () in
  log " Current video driver: %a" (Fmts.pp_opt Fmts.pp_str) driver;
  begin match Sdl.get_num_video_drivers () with
  | Error (`Msg e) -> log_err " Could not get number of video drivers: %s" e
  | Ok count ->
      log " Number of video drivers: %d" count;
      for d = 0 to count - 1 do
        begin match Sdl.get_video_driver d with
        | Error (`Msg e) -> log_err " Driver %d: error: %s" d e
        | Ok name -> log " Driver %d: %s" d name
        end;
      done
  end;
  log " Quit video";
  Sdl.video_quit ();
  log " Init video with: %a" (Fmts.pp_opt Fmts.pp_str) driver;
  begin match Sdl.video_init None with
  | Error (`Msg e) -> log_err " Could not init video: %s" e
  | Ok () -> ()
  end;
  ()

let test_displays () =
  log "Testing displays";
  begin match Sdl.get_num_video_displays () with
  | Error (`Msg e) -> log_err " Could not get number of video displays: %s" e
  | Ok count ->
      log " Number of displays: %d" count;
      for d = 0 to count - 1 do
        log " Display %d" d;
        begin match Sdl.get_display_name d with
        | Error (`Msg e) -> log_err "  Could not get display name: %s" e
        | Ok n -> log "  Name: %s" n
        end;
        begin match Sdl.get_display_bounds d with
        | Error (`Msg e) -> log_err "  Could not get display bounds: %s" e
        | Ok r -> log "  Bounds: @[%a@]" Fmts.pp_rect r
        end;
        begin match Sdl.get_current_display_mode d with
        | Error (`Msg e) -> log_err "  Could not get display mode: %s" e
        | Ok m -> log "  Current mode: @[%a@]" Fmts.pp_display_mode m;
        end;
        begin match Sdl.get_desktop_display_mode d with
        | Error (`Msg e) -> log_err " Could not get desktop display mode: %s" e
        | Ok m -> log "  Desktop mode: @[%a@]" Fmts.pp_display_mode m;
        end;
        begin match Sdl.get_current_display_mode d with
        | Error (`Msg e) -> log_err "  Could not get display mode: %s" e
        | Ok m ->
            let m' = { m with dm_w = m.Sdl.dm_w / 2;
                              dm_h = m.Sdl.dm_h / 2;
                              dm_refresh_rate = Some 60;
                              Sdl.dm_driverdata = None }
            in
            begin match Sdl.get_closest_display_mode d m' with
            | None ->
                log "  @[No closest display mode of %a found@]"
                  Fmts.pp_display_mode m'
            | Some m ->
                log "  @[<1>Closest display mode of %a:@ %a@]"
                  Fmts.pp_display_mode m'  Fmts.pp_display_mode m
            end
        end;
        begin match Sdl.get_num_display_modes d with
        | Error (`Msg e) ->
            log_err " Could not get number of display modes: %s" e
        | Ok count ->
            begin match Sdl.get_num_display_modes d with
            | Error (`Msg e) ->
                log_err " Could not get number of display modes: %s" e
            | Ok count ->
                log "  %d display modes:" count;
                for i = 0 to (count - 1) do
                  match Sdl.get_display_mode d i with
                  | Error (`Msg e) ->
                      log_err "   Could not get display mode: %s" e
                  | Ok m -> log "   @[%a@]" Fmts.pp_display_mode m
                done
            end
        end;
      done
  end;
  ()

let test_windows () =
  log "Testing windows";
  let w_title = "A windowé" in
  let w_props = Sdl.Window.(shown + resizable) in
  begin match Sdl.create_window w_title ~w:640 ~h:480 w_props with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      begin match Sdl.set_window_brightness w 0.5 with
      | Error (`Msg e) -> log_err " Could not set window brightness: %s" e
      | Ok () -> assert (Sdl.get_window_brightness w = 0.5)
      end;
      begin match Sdl.get_window_display_index w with
      | Error (`Msg e) -> log_err " Could not get display index: %s" e
      | Ok d -> log " Window display index: %d" d;
      end;
      begin match Sdl.get_window_display_mode w with
      | Error (`Msg e) -> log_err " Could not get display mode: %s" e
      | Ok m ->
          log " Window display mode: %a" Fmts.pp_display_mode m;
          let m' = { m with dm_w = m.Sdl.dm_w / 2;
                            dm_h = m.Sdl.dm_h / 2;
                            dm_refresh_rate = Some 60;
                            Sdl.dm_driverdata = None }
          in
          begin match Sdl.set_window_fullscreen w Sdl.Window.fullscreen_desktop
          with
          | Error (`Msg e) ->
              log_err " Failed to set window to full screen mode: %s" e
          | Ok () -> ()
          end;
          begin match Sdl.set_window_fullscreen w Sdl.Window.windowed with
          | Error (`Msg e) ->
              log_err " Failed to set window to windowed mode: %s" e
          | Ok () -> ()
          end;
          begin match Sdl.set_window_display_mode w m' with
          | Error (`Msg e) -> log_err " Could not set window display mode: %s" e
          | Ok () -> ()
          end
      end;
      assert (Sdl.Window.(test (Sdl.get_window_flags w) resizable));
      let id = Sdl.get_window_id w in
      begin match Sdl.get_window_from_id id with
      | Error (`Msg e) -> log_err " Could not get window from id: %s" e
      | Ok w' -> assert (id = Sdl.get_window_id w')
      end;
      begin match Sdl.get_window_gamma_ramp w with
      | Error (`Msg e) -> log_err " Could get gamma ramps: %s" e
      | Ok (r, g, b) ->
          begin match Sdl.set_window_gamma_ramp w r g b with
          | Error (`Msg e) -> log_err " Could not set gamma ramp: %s" e
          | Ok () -> ()
          end
      end;
      Sdl.set_window_grab w true;
      assert(Sdl.get_window_grab w);
      Sdl.set_window_maximum_size w ~w:700 ~h:600;
      assert (Sdl.get_window_maximum_size w = (700, 600));
      Sdl.set_window_minimum_size w ~w:10 ~h:20;
      assert (Sdl.get_window_minimum_size w = (10, 20));
      log " Window pixel format: %s"
        (Sdl.get_pixel_format_name (Sdl.get_window_pixel_format w));
      let (x, y as pos) = Sdl.get_window_position w in
      Sdl.set_window_position w ~x:(x + 10) ~y:(y + 25);
      assert (Sdl.get_window_position w = (x + 10, y + 25));
      Sdl.set_window_size w ~w:100 ~h:200;
      assert (Sdl.get_window_size w = (100, 200));
      begin match Sdl.get_window_surface w with
      | Error (`Msg e) -> log_err " Could not get window surface: %s" e
      | Ok s ->
          assert (Sdl.fill_rect s None 0xFF0000FFl = Ok ());
          assert (Sdl.update_window_surface w = Ok ());
          let rs = [ Sdl.Rect.create 0 0 20 20; Sdl.Rect.create 20 20 20 20 ] in
          assert (Sdl.fill_rects s rs 0x00FF00FFl = Ok ());
          assert (Sdl.update_window_surface_rects w rs = Ok ());
          let rs = create_bigarray Bigarray.int32 8 in
          rs.{0} <- 40l; rs.{1} <- 40l; rs.{2} <- 20l; rs.{3} <- 20l;
          rs.{4} <- 60l; rs.{5} <- 60l; rs.{6} <- 20l; rs.{7} <- 20l;
          assert (Sdl.fill_rects_ba s rs 0x0000FFFFl = Ok ());
          assert (Sdl.update_window_surface_rects_ba w rs = Ok ());
      end;
      Sdl.set_window_title w "hop";
      assert (Sdl.get_window_title w = "hop");
      Sdl.hide_window w;
      Sdl.show_window w;
      Sdl.maximize_window w;
      Sdl.minimize_window w;
      Sdl.restore_window w;
      Sdl.raise_window w;
      Sdl.set_window_bordered w true;
      begin match Sdl.create_rgb_surface ~w:16 ~h:16 ~depth:32
                    0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
      with
      | Error (`Msg e) -> log_err " Could not create surface: %s" e
      | Ok icon ->
          assert (Sdl.fill_rect icon None 0x00FF007Fl = Ok ());
          Sdl.set_window_icon w icon;
          Sdl.free_surface icon
      end;
      Sdl.destroy_window w;
  end;
  ()

let test_opengl_contexts () =
  log "Testing OpenGL contexts";
  Sdl.gl_reset_attributes ();
  assert (Sdl.gl_set_attribute Sdl.Gl.doublebuffer 1 = Ok ());
  let flags = Sdl.Window.(opengl) in
  match Sdl.create_window "OpenGL" ~w:640 ~h:480 flags with
  | Error (`Msg e) -> log_err " Could not create OpenGL window: %s" e
  | Ok w ->
      begin match Sdl.gl_create_context w with
      | Error (`Msg e) -> log_err " Could not create OpenGL context: %s" e
      | Ok ctx ->
          begin match Sdl.gl_get_attribute Sdl.Gl.context_major_version with
          | Error (`Msg e) ->
              log_err " Could not get context major version: %s" e
          | Ok maj ->
              match Sdl.gl_get_attribute Sdl.Gl.context_minor_version with
              | Error (`Msg e) ->
                  log_err " Could not get context minor version: %s" e
              | Ok min -> log " Context version: %d.%d" maj min
          end;
          begin match Sdl.gl_get_attribute Sdl.Gl.context_egl with
          | Error (`Msg e) -> log_err " Could not get context EGL: %s" e
          | Ok egl -> log " Context is EGL: %b" (egl = 1)
          end;
          begin match Sdl.gl_get_attribute Sdl.Gl.context_profile_mask with
          | Error (`Msg e) ->
              log_err " Could not get context profile mask: %s" e
          | Ok m ->
              log " Context core: %b compatibility: %b ES: %b"
                (m land Sdl.Gl.context_profile_core <> 0)
                (m land Sdl.Gl.context_profile_compatibility <> 0)
                (m land Sdl.Gl.context_profile_es <> 0)
          end;
          assert (Sdl.gl_extension_supported "BLA234241" = false);
          begin match (Sdl.gl_set_swap_interval 1) with
          | Error (`Msg e) -> log_err " Could not set swap interval: %s" e
          | Ok () -> assert (Sdl.gl_get_swap_interval () = Ok 1)
          end;
          assert (Sdl.gl_make_current w ctx = Ok ());
          begin match Sdl.gl_get_current_context () with
          | Error (`Msg e) -> log_err " Could not get current context: %s" e
          | Ok _ -> ();
          end;
          assert (Sdl.gl_get_drawable_size w = (640, 480));
          begin match Sdl.create_renderer w with
          | Error (`Msg e) -> log_err " Could not create renderer: %s" e
          | Ok r ->
              begin match Sdl.create_texture r Sdl.Pixel.format_rgba8888
                            Sdl.Texture.access_static ~w:256 ~h:256
              with
              | Error (`Msg e) -> log_err " Could not create texture: %s" e
              | Ok t ->
                  begin match Sdl.gl_bind_texture t with
                  | Error (`Msg e) -> log " Could not bind texture: %s" e
                  | Ok (w, h) ->
                      (* FIXME: this segfaults in 2.0.1
                         see https://bugzilla.libsdl.org/show_bug.cgi?id=2296 *)
                      (* assert (Sdl.gl_unbind_texture t = Ok ()); *)
                      ()
                  end;
                  Sdl.destroy_texture t
              end;
              Sdl.destroy_renderer r
          end;
          Sdl.gl_swap_window w;
          Sdl.gl_delete_context ctx
      end;
      Sdl.destroy_window w

let test_screen_saver () =
  log "Testing screen saver functions";
  let e = Sdl.is_screen_saver_enabled () in
  if e then begin
    Sdl.disable_screen_saver ();
    assert (not (Sdl.is_screen_saver_enabled ()));
    Sdl.enable_screen_saver ();
  end else begin
    Sdl.enable_screen_saver ();
    assert (Sdl.is_screen_saver_enabled ());
    Sdl.disable_screen_saver ();
  end;
  ()

let test_message_boxes human =
  log "Testing message boxes";
  if not human then log " not tested, needs a human (invoke with -h)" else
  let show typ title msg =
    match Sdl.show_simple_message_box typ ~title msg None with
    | Error (`Msg e) -> log_err " Could not show message box %s: %s" title e
    | Ok () -> ()
  in
  show Sdl.Message_box.error "Error" "This is an error";
  show Sdl.Message_box.warning "Warning" "This is a warning";
  show Sdl.Message_box.information "Information" "This is an information";
  let d =
    let open Sdl.Message_box in
    let undo = { button_flags = button_escapekey_default;
                 button_id = 1;
                 button_text = "Undo"; }
    in
    let action = { button_flags = button_returnkey_default;
               button_id = 2;
               button_text = "Action" }
    in
    let color_scheme = { color_background = (255, 255, 255);
                         color_text = (255, 0, 0);
                         color_button_border = (0, 255, 0);
                         color_button_background = (0, 0, 255);
                         color_button_selected = (0, 255, 255); }
    in
    { flags = warning; window = None; title = "Action";
      message = "Do you want to action ?"; buttons = [undo; action];
      color_scheme = Some color_scheme }
  in
  begin match Sdl.show_message_box d with
  | Error (`Msg e) -> log_err " Could not show message box: %s" e
  | Ok 1 | Ok 2 -> ()
  | Ok _ -> assert false
  end;
  ()

let test_clipboard () =
  log "Testing clipboard";
  match Sdl.create_window "Clipboard" ~w:640 ~h:480 Sdl.Window.shown with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      (* N.B. we need a window on Linux otherwise we get an
         odd stack overflow. *)
      let saved =
        if not (Sdl.has_clipboard_text ()) then None else
        begin match Sdl.get_clipboard_text () with
        | Error (`Msg e) -> log_err " Could not get clipboard text %s" e; None
        | Ok text -> Some text
        end;
      in
      begin match Sdl.set_clipboard_text "öpooo" with
      | Error (`Msg e) -> log_err " Could not set clipboard text: %s" e
      | Ok () ->
          assert (Sdl.has_clipboard_text ());
          assert (Sdl.get_clipboard_text () = Ok "öpooo");
      end;
      begin match saved with
      | None -> () | Some t -> assert (Sdl.set_clipboard_text t = Ok ())
      end;
      Sdl.destroy_window w;
      ()

let test_keyboard () =
  log "Testing keyboard";
  match Sdl.create_window "Keyboard" ~w:640 ~h:480 Sdl.Window.input_focus with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      begin match Sdl.get_keyboard_focus () with
      | None -> log " No keyboard focus"
      | Some w' -> assert (Sdl.get_window_id w = Sdl.get_window_id w')
      end;
      ignore (Sdl.get_keyboard_state ());
      assert (Sdl.get_key_from_name "K" = Sdl.K.k);
      assert (Sdl.get_key_from_scancode Sdl.Scancode.k = Sdl.K.k);
      assert (Sdl.get_key_name Sdl.K.k = "K");
      assert (Sdl.get_scancode_from_key Sdl.K.k = Sdl.Scancode.k);
      assert (Sdl.get_scancode_from_name "K" = Sdl.Scancode.k);
      assert (Sdl.get_scancode_name Sdl.Scancode.k = "K");
      assert (Sdl.Scancode.(enum unknown) = `Unknown);
      assert (Sdl.Scancode.(enum a) = `A);
      assert (Sdl.Scancode.(enum app2) = `App2);
      log " screen keyboard: %b shown: %b"
        (Sdl.has_screen_keyboard_support ())
        (Sdl.is_screen_keyboard_shown w);
      let m = Sdl.get_mod_state () in
      Sdl.set_mod_state m;
      Sdl.set_text_input_rect None;
      Sdl.start_text_input ();
      assert ((Sdl.is_text_input_active ()));
      Sdl.stop_text_input ();
      assert (not (Sdl.is_text_input_active ()));
      Sdl.destroy_window w;
      ()

let test_mouse () =
  log "Testing mouse";
  match Sdl.create_window "Mouse" ~w:640 ~h:480 Sdl.Window.mouse_focus with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      ignore (Sdl.show_cursor true);
      ignore (Sdl.get_cursor_shown ());
      Sdl.pump_events ();
      Sdl.warp_mouse_in_window None 50 50;
      let current_cursor = Sdl.get_cursor () in
      let default_cursor = Sdl.get_default_cursor () in
      let cd = create_bigarray Bigarray.int8_unsigned (2 * 16) in
      let cm = create_bigarray Bigarray.int8_unsigned (2 * 16) in
      for i = 0 to 2 * 16 - 1 do cd.{i} <- 0x00; cm.{i} <- 0xFF done;
      begin match Sdl.create_cursor cd cm 16 16 7 7 with
      | Error (`Msg e) -> log_err " Could not create cursor: %s" e
      | Ok c ->
          Sdl.set_cursor (Some c);
          Sdl.pump_events (); Sdl.delay 300l;
          Sdl.free_cursor c
      end;
      begin match Sdl.create_rgb_surface ~w:16 ~h:16 ~depth:32
                    0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
      with
      | Error (`Msg e) -> log_err " Could not create surface: %s" e
      | Ok s ->
          assert (Sdl.fill_rect s None 0x0000FF7Fl = Ok ());
          begin match Sdl.create_color_cursor s 7 7 with
          | Error (`Msg e) -> log_err " Could not create color cursor: %s" e
          | Ok c ->
              Sdl.set_cursor (Some c);
              Sdl.pump_events (); Sdl.delay 300l;
              Sdl.free_cursor c
          end;
          Sdl.free_surface s
      end;
      begin match Sdl.create_system_cursor Sdl.System_cursor.hand with
      | Error (`Msg e) -> log_err " Could not create hand cursor: %s" e
      | Ok c ->
          Sdl.set_cursor (Some c);
          Sdl.pump_events (); Sdl.delay 300l;
          Sdl.free_cursor c
      end;
      Sdl.set_cursor default_cursor;
      Sdl.set_cursor current_cursor;
      begin match Sdl.get_mouse_focus () with
      | None -> ()
      | Some w' -> assert (Sdl.get_window_id w = Sdl.get_window_id w')
      end;
      ignore (Sdl.get_mouse_state ());
      ignore (Sdl.get_relative_mouse_state ());
      let rm = Sdl.get_relative_mouse_mode () in
      begin match Sdl.set_relative_mouse_mode (not rm) with
      | Error (`Msg e) -> log_err " Could not set relative mouse mode: %s" e
      | Ok () -> ()
      end;
      Sdl.destroy_window w;
      ()

let test_touch () =
  log "Testing touch";
  match Sdl.create_window "Touch" ~w:640 ~h:480 Sdl.Window.mouse_focus with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      let count = Sdl.get_num_touch_devices () in
      for i = 0 to count - 1 do
        begin match Sdl.get_touch_device i with
        | Error (`Msg e) -> log_err " Could not get touch device: %s" e
        | Ok id ->
            let fingers = Sdl.get_num_touch_fingers id in
            for i = 0 to fingers - 1 do
              begin match Sdl.get_touch_finger id i with
              | None -> ()
              | Some f ->
                  Sdl.Finger.(log "%f %f %f" (x f) (y f) (pressure f));
              end;
            done;
            begin match Sdl.record_gesture id with
            | Error (`Msg e) -> log_err " Could not record gesture: %s" e
            | Ok () -> ()
            end;
        end;
      done;
      Sdl.destroy_window w;
      ()

let test_joysticks () =
  log "Testing joysticks";
  match Sdl.create_window "Joystick" ~w:640 ~h:480 Sdl.Window.shown with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      ignore (Sdl.joystick_set_event_state Sdl.enable);
      assert (Sdl.joystick_get_event_state () = Ok (Sdl.enable));
      Sdl.joystick_update ();
      begin match Sdl.num_joysticks () with
      | Error (`Msg e) -> log_err " Could not get number of joysticks: %s" e
      | Ok count ->
          for i = 0 to count - 1 do match Sdl.joystick_open i with
          | Error (`Msg e) -> log_err " Could not open joystick: %s" e
          | Ok j ->
              let guid = Sdl.joystick_get_guid j in
              let guid_str = Sdl.joystick_get_guid_string guid in
              let name = match Sdl.joystick_name j with
              | Error (`Msg e) ->
                  log_err " Could not get joystick name: %s" e; "unknown"
              | Ok n -> n
              in
              log "Joystick %d %s %s" i name guid_str;
              ignore (Sdl.joystick_get_guid_from_string guid_str);
              assert (Sdl.joystick_name j = Sdl.joystick_name_for_index i);
              ignore (Sdl.joystick_instance_id j);
              ignore (Sdl.joystick_get_attached j);
              begin match Sdl.joystick_num_axes j with
              | Error (`Msg e) -> log_err " Could not get num axes: %s" e
              | Ok acount ->
                  for i = 0 to acount - 1 do
                    ignore (Sdl.joystick_get_axis j i);
                  done;
              end;
              begin match Sdl.joystick_num_balls j with
              | Error (`Msg e) -> log_err " Could not get num balls: %s" e
              | Ok bcount ->
                  for i = 0 to bcount - 1 do
                    ignore (Sdl.joystick_get_ball j i);
                  done;
              end;
              begin match Sdl.joystick_num_buttons j with
              | Error (`Msg e) -> log_err " Could not get num buttons: %s" e
              | Ok bcount ->
                  for i = 0 to bcount - 1 do ignore
                      (Sdl.joystick_get_button j i);
                  done;
              end;
              begin match Sdl.joystick_num_hats j with
              | Error (`Msg e) -> log_err " Could not get num hats: %s" e
              | Ok bcount ->
                  for i = 0 to bcount - 1 do
                    ignore (Sdl.joystick_get_hat j i);
                  done;
              end;
              Sdl.joystick_close j
          done;
      end;
      Sdl.destroy_window w;
      ()

let test_game_controllers () =
  log "Testing game controllers";
  match Sdl.create_window "Controllers" ~w:640 ~h:480 Sdl.Window.shown with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      ignore (Sdl.game_controller_set_event_state Sdl.enable);
      assert (Sdl.game_controller_get_event_state () = Ok (Sdl.enable));
      ignore (Sdl.game_controller_get_string_for_axis
                Sdl.Controller.axis_left_x);
      ignore (Sdl.game_controller_get_string_for_button
                Sdl.Controller.button_a);
      Sdl.game_controller_update ();
      begin match Sdl.num_joysticks () with
      | Error (`Msg e) -> log_err " Could not get number of joysticks: %s" e
      | Ok count ->
          for i = 0 to count - 1 do
            if not (Sdl.is_game_controller i) then () else
            match Sdl.game_controller_open i with
            | Error (`Msg e) -> log_err " Could not open game controller: %s" e
            | Ok c ->
                let name = match Sdl.game_controller_name c with
                | Error (`Msg e) ->
                    log_err "Could not get controller name: %s" e; "unknown"
                | Ok n -> n
                in
                log " Controller %d %s" i name;
                ignore (Sdl.game_controller_mapping c);
                ignore (Sdl.game_controller_get_joystick c);
                ignore (Sdl.game_controller_get_button c
                          Sdl.Controller.button_a);
                ignore (Sdl.game_controller_get_bind_for_button c
                          Sdl.Controller.button_a);
                ignore (Sdl.game_controller_get_axis c
                          Sdl.Controller.axis_left_x);
                ignore (Sdl.game_controller_get_bind_for_axis c
                          Sdl.Controller.axis_left_x);
                ignore (Sdl.game_controller_get_attached c);
                assert (Sdl.game_controller_name c =
                        Sdl.game_controller_name_for_index i);
                Sdl.game_controller_close c
          done;
      end;
      Sdl.destroy_window w;
      ()

let test_events () =
  log "Testing events";
  match Sdl.create_window "Events" ~w:640 ~h:480 Sdl.Window.resizable with
  | Error (`Msg e) -> log_err " Could not create window: %s" e
  | Ok w ->
      assert (Sdl.Event.(enum first_event) = `Unknown);
      assert (Sdl.Event.(enum quit) = `Quit);
      assert (Sdl.Event.(enum window_event) = `Window_event);
      let e = Sdl.Event.create () in
      Sdl.pump_events ();
      ignore (Sdl.has_events Sdl.Event.first_event Sdl.Event.last_event);
      ignore (Sdl.has_event Sdl.Event.window_event);
      ignore (Sdl.poll_event None);
      ignore (Sdl.poll_event (Some e));
      Sdl.set_event_state Sdl.Event.window_event Sdl.disable;
      assert (Sdl.get_event_state Sdl.Event.window_event = Sdl.disable);
      Sdl.set_event_state Sdl.Event.window_event Sdl.enable;
      while (Sdl.wait_event_timeout (Some e) 500) do () done;
      begin match Sdl.register_event () with
      | None -> log_err " Could not register event"
      | Some id ->
          Sdl.Event.set e Sdl.Event.typ id;
          begin match Sdl.push_event e with
          | Error (`Msg e) -> log_err " Could not push event: %s" e
          | Ok false -> log " Pushed event filtered"
          | Ok true ->
              assert (Sdl.has_event id);
              Sdl.flush_event id;
              assert (not (Sdl.has_event id));
              Sdl.flush_events Sdl.Event.first_event Sdl.Event.last_event;
          end;
          begin match Sdl.push_event e with
          | Error (`Msg e) -> log_err " Could not push event: %s" e
          | Ok false -> log " Event pushed filtered"
          | Ok true ->
              assert (Sdl.has_event id);
              begin match Sdl.wait_event None with
              | Error (`Msg e) -> log_err " Could not wait event: %s" e
              | Ok () -> ()
              end;
              begin match Sdl.wait_event (Some e) with
              | Error (`Msg e) -> log_err " Could not wait event: %s" e
              | Ok () -> assert (Sdl.Event.(get e typ) = id)
              end;
          end;
      end;
      Sdl.destroy_window w;
      ()

let test_haptic () =
  log "Testing haptic";
  begin match Sdl.mouse_is_haptic () with
  | Error (`Msg e) -> log_err " Could not determine if mouse is haptic: %s" e
  | Ok mouse ->
      if not mouse then () else
      begin match Sdl.haptic_open_from_mouse () with
      | Error (`Msg e) -> log_err " Could not open haptic from the mouse: %s" e
      | Ok h -> Sdl.haptic_close h;
      end;
  end;
  begin match Sdl.num_haptics () with
  | Error (`Msg e) ->
      log_err " Could not get the number of haptic devices: %s" e
  | Ok count ->
      for d = 0 to count - 1 do
        let name = match Sdl.haptic_name d with
        | Error (`Msg e) ->
            log_err " Could not get haptic name: %s" e; "unknown"
        | Ok name -> name
        in
        log " Haptic device %d: %s" d name;
        begin match Sdl.haptic_open d with
        | Error (`Msg e) -> log_err " Could not open device: %s" e
        | Ok h ->
            assert (Sdl.haptic_opened d);
            assert (Sdl.haptic_index h = Ok d);
            ignore (Sdl.haptic_num_axes h);
            ignore (Sdl.haptic_num_effects h);
            ignore (Sdl.haptic_num_effects_playing h);
            ignore (Sdl.haptic_query h);
            begin match Sdl.haptic_rumble_supported h with
            | Error _ | Ok false -> ()
            | Ok true ->
                ignore (Sdl.haptic_rumble_init h);
                ignore (Sdl.haptic_rumble_play h 0.5 3000l);
                ignore (Sdl.haptic_rumble_stop h);
            end;
            (* untested TODO
              Sdl.haptic_run_effect
              Sdl.haptic_destroy_effect
              Sdl.haptic_effect_supported
              Sdl.haptic_get_effect_status
              Sdl.haptic_new_effect
              Sdl.haptic_open_from_joystick
              Sdl.haptic_run_effect
              Sdl.haptic_set_autocenter
              Sdl.haptic_set_gain
              Sdl.haptic_stop_effect
              Sdl.haptic_update_effect
              Sdl.joystick_is_haptic *)
            ignore (Sdl.haptic_pause h);
            ignore (Sdl.haptic_unpause h);
            ignore (Sdl.haptic_stop_all h);
            Sdl.haptic_close h;

        end;
      done;
  end;
  ()

let test_audio_drivers () =
  log "Testing audio drivers";
  let driver = Sdl.get_current_audio_driver () in
  log " Current audio driver: %a" (Fmts.pp_opt Fmts.pp_str) driver;
  begin match Sdl.get_num_audio_drivers () with
  | Error (`Msg e) -> log_err " Could not get number of audio drivers: %s" e
  | Ok count ->
      log " Number of audio drivers: %d" count;
      for d = 0 to count - 1 do
        begin match Sdl.get_audio_driver d with
        | Error (`Msg e) -> log_err " Driver %d: error: %s" d e
        | Ok name -> log " Driver %d: %s" d name
        end;
      done
  end;
  log " Quit audio";
  Sdl.audio_quit ();
  log " Init audio with: %a" (Fmts.pp_opt Fmts.pp_str) driver;
  begin match Sdl.audio_init driver with
  | Error (`Msg e) -> log_err " Could not init audio: %s" e
  | Ok () -> ()
  end;
  ()

let test_audio_devices () =
  log "Testing audio devices";
  begin match Sdl.get_num_audio_devices false with
  | Error (`Msg e) -> log_err " Could not get number of audio devices: %s" e
  | Ok count ->
      log " Number of audio devices: %d" count;
      for i = 0 to count - 1 do
        log " Audio device %d %s" i
          begin match Sdl.get_audio_device_name i false with
          | Error (`Msg e) ->
              log " Could not get audio device name: %s" e; "unknown"
          | Ok name -> name
          end;
      done;
(*      let a440 =
        let t = ref 0. in
        fun a ->
          let c = (6.2831853 *. 440.) /. 44100. in
          for i = 0 to Bigarray.Array1.dim a - 1 do
            a.{i} <- (int_of_float (10000. *. (sin (c *. !t))));
            t := !t +. 1.0;
          done
      in
*)
      let spec = { Sdl.as_freq = 44100;
                   as_format = Sdl.Audio.s16_sys;
                   as_channels = 1;
                   as_samples = 4096;
                   as_ba_kind = Bigarray.int16_unsigned;
(*                   as_callback = Some a440; *)
                   as_size = 0l;
                   as_silence = 0; }
      in
      begin match Sdl.open_audio_device None false spec
                    Sdl.Audio.allow_any_change with
      | Error (`Msg e) -> log_err " Could not open audio device: %s" e
      | Ok (dev, obtained) ->
          if not (obtained.Sdl.as_channels = 1 && obtained.Sdl.as_freq = 44100)
          then log " Did not obtain expected audio device, will not play"
          else begin
            assert (Sdl.get_audio_device_status dev = Sdl.Audio.paused);
            Sdl.pause_audio_device dev false;
            Sdl.delay 2000l;
          end;
          Sdl.close_audio_device dev;
          assert (Sdl.get_audio_device_status dev = Sdl.Audio.stopped);
      end
  end;
  ()

let test_time d =
  log "Testing time functions";
  begin
    log " Delay: %lums" d;
    let t = Sdl.get_ticks () in
    Sdl.delay d;
    log " Elapsed: %lums (ticks)" (Int32.sub (Sdl.get_ticks ()) t)
  end;
  begin
    log " Delay: %lums" d;
    let c = Sdl.get_performance_counter () in
    Sdl.delay d;
    let c_dt = Int64.sub (Sdl.get_performance_counter ()) c in
    let freq = Sdl.get_performance_frequency () in
    let c_dt = ((Int64.to_float c_dt) /. ((Int64.to_float freq) /. 1000.)) in
    log " Elapsed: %gms (performance counter)" c_dt
  end;
  ()

let test_platform_cpu_info () =
  log "Testing platform and CPU information functions";
  log " Platform: %s" (Sdl.get_platform ());
  let cache_line_size = match Sdl.get_cpu_cache_line_size () with
  | Error (`Msg e) -> e
  | Ok s -> Printf.sprintf "%d" s
  in
  log " CPU: @[count:%d@ RAM:%d@ cache-line-size:%s@ 3DNow:%b@ AltiVec:%b@ \
       MMX:%b@ RDTSC:%b@ SSE:%b@ SSE2:%b@ SSE3:%b@ SSE41:%b@ SSE42:%b@]"
    (Sdl.get_cpu_count ())
    (Sdl.get_system_ram ())
    cache_line_size
    (Sdl.has_3d_now ())
    (Sdl.has_altivec ())
    (Sdl.has_mmx ())
    (Sdl.has_rdtsc ())
    (Sdl.has_sse ())
    (Sdl.has_sse2 ())
    (Sdl.has_sse3 ())
    (Sdl.has_sse41 ())
    (Sdl.has_sse42 ())

let test_power_info () =
  log "Testing power information";
  let p = Sdl.get_power_info () in
  let state = match p.Sdl.pi_state with
  | `Unknown -> "unknown"
  | `On_battery -> "on battery"
  | `No_battery -> "plugged, no battery"
  | `Charging -> "plugged, charging"
  | `Charged -> "plugged, charged"
  in
  let dur = match p.Sdl.pi_secs with
  | None -> "" | Some secs -> Printf.sprintf " remaining: %dmin" (secs / 60)
  in
  let pct = match p.Sdl.pi_pct with
  | None -> "" | Some pct -> Printf.sprintf " (%d%%)" pct
  in
  log " Power: %s%s%s" state dur pct

let tests human = match Sdl.init Sdl.Init.everything with
| Error (`Msg e) -> log_err " Could not initialize SDL: %s" e; exit 1
| Ok () ->
    test_init ();
    test_hints ();
    test_error ();
    test_log ();
    test_version ();
    test_rw_ops ();
    test_file_system_paths ();
    test_colors ();
    test_points ();
    test_rectangles ();
    test_palettes ();
    test_pixel_formats ();
    test_surfaces ();
    test_renderers ();
    test_textures ();
    test_video_drivers ();
    test_displays ();
    test_windows ();
    test_opengl_contexts ();
    test_screen_saver ();
    test_message_boxes human;
    test_clipboard ();
    test_keyboard ();
    test_mouse ();
    test_touch ();
    test_joysticks ();
    test_game_controllers ();
    test_events ();
    test_haptic ();
    test_audio_drivers ();
    test_audio_devices ();
    test_time 150l;
    test_power_info ();
    test_platform_cpu_info ();
    Sdl.quit ();
    Gc.compact ()

let main () =
  let usage =
    Printf.sprintf "Usage: %s [OPTION]\n Tests Tsdl.\nOptions:"
      (Filename.basename Sys.executable_name)
  in
  let human = ref false in
  let options =
    [ "-h", Arg.Set human, " Also make tests that need human interaction"; ]
  in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;
  tests !human

let () = main ()

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
