(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let unsafe_get = Array.unsafe_get

open Ctypes
module C = struct
  module Types = Types_generated
  module Async_functions = Async_function_description.Functions (Async_functions_generated)
  module Functions = Function_description.Functions (Functions_generated)
end

module Sdl = struct

(* Enum cases and #ifdef'd constants, see support/ in the distribution *)

(* Formatting with continuation. *)

let kpp k fmt =
  let k _fmt = k (Format.flush_str_formatter ()) in
  Format.kfprintf k Format.str_formatter fmt

(* Invalid_argument strings *)

let str = Printf.sprintf
let err_length_mul l mul = str "invalid length: %d not a multiple of %d" l mul
let err_read_field = "cannot read field"
let err_bigarray_pitch pitch ba_el_size =
  str "invalid bigarray kind: pitch (%d bytes) not a multiple of bigarray \
       element byte size (%d)" pitch ba_el_size

let err_bigarray_data len ba_el_size =
  str "invalid bigarray kind: data (%d bytes) not a multiple of bigarray \
       element byte size (%d)" len ba_el_size

let err_array_to_short ~exp ~fnd =
  str "array too short exp:%d bytes found:%d bytes" exp fnd

(* ctypes views *)

let char_array_as_string a =
  Ctypes.(string_from_ptr (CArray.start a) ~length:(CArray.length a))

(* SDL results *)

type nonrec 'a result = ( 'a, [ `Msg of string ] ) result

let get_error = C.Functions.get_error

let error () = Error (`Msg (get_error ()))

let zero_to_ok = function 0 -> Ok () | _ -> error ()

let one_to_ok = function 1 -> Ok () | _ -> error ()

let bool_to_ok = function 0 -> Ok false | 1 -> Ok true | _ -> error ()

let nat_to_ok = function n when n < 0 -> error () | n -> Ok n

let some_to_ok = function Some v -> Ok v | None -> error ()

let sdl_free = C.Functions.sdl_free

(* Since we never let SDL redefine our main make sure this is always
   called. *)

let () =
  C.Functions.set_main_ready ()

(* Integer types and maps *)

type uint8 = int
type uint16 = int
type int16 = int
type uint32 = int32
type uint64 = int64

module Int = struct type t = int let compare : int -> int -> int = compare end
module Imap = Map.Make(Int)

(* Bigarrays *)

type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

let ba_create k len = Bigarray.Array1.create k Bigarray.c_layout len
let ba_kind_byte_size :  ('a, 'b) Bigarray.kind -> int = fun k ->
  let open Bigarray in
  (* FIXME: see http://caml.inria.fr/mantis/view.php?id=6263 *)
  match Obj.magic k with
  | k when k = char || k = int8_signed || k = int8_unsigned -> 1
  | k when k = int16_signed || k = int16_unsigned -> 2
  | k when k = int32 || k = float32 -> 4
  | k when k = float64 || k = int64 || k = complex32 -> 8
  | k when k = complex64 -> 16
  | k when k = int || k = nativeint -> Sys.word_size / 8
  | _ -> assert false

let access_ptr_typ_of_ba_kind : ('a, 'b) Bigarray.kind -> 'a ptr typ = fun k ->
  let open Bigarray in
  (* FIXME: use typ_of_bigarray_kind when ctypes support it. *)
  match Obj.magic k with
  | k when k = float32 -> Obj.magic (ptr Ctypes.float)
  | k when k = float64 -> Obj.magic (ptr Ctypes.double)
  | k when k = complex32 -> Obj.magic (ptr Ctypes.complex32)
  | k when k = complex64 -> Obj.magic (ptr Ctypes.complex64)
  | k when k = int8_signed -> Obj.magic (ptr Ctypes.int8_t)
  | k when k = int8_unsigned -> Obj.magic (ptr Ctypes.uint8_t)
  | k when k = int16_signed -> Obj.magic (ptr Ctypes.int16_t)
  | k when k = int16_unsigned -> Obj.magic (ptr Ctypes.uint16_t)
  | k when k = int -> Obj.magic (ptr Ctypes.camlint)
  | k when k = int32 -> Obj.magic (ptr Ctypes.int32_t)
  | k when k = int64 -> Obj.magic (ptr Ctypes.int64_t)
  | k when k = nativeint -> Obj.magic (ptr Ctypes.nativeint)
  | k when k = char -> Obj.magic (ptr Ctypes.char)
  | _ -> assert false

(* Basics *)

(* Initialization and shutdown *)

module Init = struct
  type t = Unsigned.uint32
  let ( + ) = Unsigned.UInt32.logor
  let ( - ) f f' = Unsigned.UInt32.(logand f (lognot f'))
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let nothing = Unsigned.UInt32.zero
  include C.Types.Init
end

let init n = zero_to_ok (C.Functions.init n)

let init_sub_system n = zero_to_ok (C.Functions.init_sub_system n)

let quit = C.Functions.quit

let quit_sub_system = C.Functions.quit_sub_system

let was_init = function
| None -> C.Functions.was_init (Unsigned.UInt32.of_int 0)
| Some m -> C.Functions.was_init m

(* Hints *)

module Hint = struct
  type t = string
  let audio_resampling_mode =
    char_array_as_string (!@ C.Functions.Hint.audio_resampling_mode)
  let framebuffer_acceleration =
    char_array_as_string (!@ C.Functions.Hint.framebuffer_acceleration)
  let idle_timer_disabled =
    char_array_as_string (!@ C.Functions.Hint.idle_timer_disabled)
  let orientations = char_array_as_string (!@ C.Functions.Hint.orientations)
  let mouse_focus_clickthrough =
    char_array_as_string (!@ C.Functions.Hint.mouse_focus_clickthrough)
  let mouse_normal_speed_scale =
    char_array_as_string (!@ C.Functions.Hint.mouse_normal_speed_scale)
  let mouse_relative_speed_scale =
    char_array_as_string (!@ C.Functions.Hint.mouse_relative_speed_scale)
  let render_driver = char_array_as_string (!@ C.Functions.Hint.render_driver)
  let render_logical_size_mode =
    char_array_as_string (!@ C.Functions.Hint.render_logical_size_mode)
  let render_opengl_shaders =
    char_array_as_string (!@ C.Functions.Hint.render_opengl_shaders)
  let render_scale_quality =
    char_array_as_string (!@ C.Functions.Hint.render_scale_quality)
  let render_vsync = char_array_as_string (!@ C.Functions.Hint.render_vsync)
  let no_signal_handlers =
    char_array_as_string (!@ C.Functions.Hint.no_signal_handlers)
  let thread_stack_size =
    char_array_as_string (!@ C.Functions.Hint.thread_stack_size)
  let touch_mouse_events =
    char_array_as_string (!@ C.Functions.Hint.touch_mouse_events)
  let mouse_touch_events =
    char_array_as_string (!@ C.Functions.Hint.mouse_touch_events)
  let window_frame_usable_while_cursor_hidden =
    char_array_as_string
      (!@ C.Functions.Hint.window_frame_usable_while_cursor_hidden)

  type priority = int
  include C.Types.Hint
end

let clear_hints = C.Functions.clear_hints

let get_hint x = C.Functions.get_hint x

let get_hint_boolean = C.Functions.get_hint_boolean

let set_hint = C.Functions.set_hint

let set_hint_with_priority = C.Functions.set_hint_with_priority

(* Errors *)

let clear_error = C.Functions.clear_error

let set_error fmt =
  kpp (fun s -> ignore (C.Functions.set_error s)) fmt

(* Log *)

module Log = struct
  type category = int

  type priority = int
  let priority_compare : int -> int -> int = compare

  include C.Types.Log
end

external log_message : int -> int -> string -> unit = "ocaml_tsdl_log_message"
let log_message c p fmt = kpp (fun s -> log_message c p s) fmt

let log fmt = log_message Log.category_application Log.priority_info fmt
let log_critical c fmt = log_message c Log.priority_critical fmt
let log_debug c fmt = log_message c Log.priority_debug fmt
let log_info c fmt = log_message c Log.priority_info fmt
let log_error c fmt = log_message c Log.priority_error fmt
let log_verbose c fmt = log_message c Log.priority_verbose fmt
let log_warn c fmt = log_message c Log.priority_warn fmt

let log_get_priority = C.Functions.log_get_priority

let log_reset_priorities = C.Functions.log_reset_priorities

let log_set_all_priority = C.Functions.log_set_all_priority

let log_set_priority = C.Functions.log_set_priority

(* Version *)
let get_version () =
  let get v f = Unsigned.UInt8.to_int (getf v f) in
  let v = make C.Types.version in
  C.Functions.get_version (addr v);
  C.Types.((get v version_major), (get v version_minor), (get v version_patch))

let get_revision = C.Functions.get_revision

type rw_ops = C.Types.rw_ops

let load_file_rw rw_ops close =
  some_to_ok (C.Functions.load_file_rw
                rw_ops (coerce (ptr void) (ptr size_t) null) close)

let rw_from_file x y = some_to_ok (C.Functions.rw_from_file x y)

let rw_from_const_mem str =
  some_to_ok (C.Functions.rw_from_const_mem
                (ocaml_string_start str) (String.length str))

let rw_from_mem b =
  some_to_ok (C.Functions.rw_from_mem (ocaml_bytes_start b) (Bytes.length b))

let load_file filename = (* defined as a macro in SDL_rwops.h *)
  match rw_from_file filename "rb" with
  | Error _ as e -> e
  | Ok rw -> load_file_rw rw true

let rw_close ops =
  if C.Functions.rw_close ops = 0 then Ok () else (error ())

let unsafe_rw_ops_of_ptr addr : C.Types.rw_ops =
  from_voidp C.Types.rw_ops_struct (ptr_of_raw_address addr)
let unsafe_ptr_of_rw_ops rw_ops =
  raw_address_of_ptr (to_voidp rw_ops)

(* File system paths *)

let get_base_path () =
  let p = C.Functions.get_base_path () in
  let path = coerce (ptr char) string_opt p in
  sdl_free (coerce (ptr char) (ptr void) p);
  some_to_ok path

let get_pref_path ~org ~app =
  let p = C.Functions.get_pref_path org app in
  let path = coerce (ptr char) string_opt p in
  sdl_free (coerce (ptr char) (ptr void) p);
  some_to_ok path

(* Colors *)

module Color = struct
  let create ~r ~g ~b ~a =
    let c = make C.Types.Color.t in
    setf c C.Types.Color.r (Unsigned.UInt8.of_int r);
    setf c C.Types.Color.g (Unsigned.UInt8.of_int g);
    setf c C.Types.Color.b (Unsigned.UInt8.of_int b);
    setf c C.Types.Color.a (Unsigned.UInt8.of_int a);
    c

  let r c = Unsigned.UInt8.to_int (getf c C.Types.Color.r)
  let g c = Unsigned.UInt8.to_int (getf c C.Types.Color.g)
  let b c = Unsigned.UInt8.to_int (getf c C.Types.Color.b)
  let a c = Unsigned.UInt8.to_int (getf c C.Types.Color.a)

  let set_r c r = setf c C.Types.Color.r (Unsigned.UInt8.of_int r)
  let set_g c g = setf c C.Types.Color.g (Unsigned.UInt8.of_int g)
  let set_b c b = setf c C.Types.Color.b (Unsigned.UInt8.of_int b)
  let set_a c a = setf c C.Types.Color.a (Unsigned.UInt8.of_int a)
end

type color = C.Types.Color.t
let color = C.Types.Color.t

(* Points *)
type point = C.Types.Point.t

module Point = struct
  let create ~x ~y =
    let p = make C.Types.Point.t in
    setf p C.Types.Point.x x;
    setf p C.Types.Point.y y;
    p

  let x p = getf p C.Types.Point.x
  let y p = getf p C.Types.Point.y

  let set_x p x = setf p C.Types.Point.x x
  let set_y p y = setf p C.Types.Point.y y

  let opt_addr = function
  | None -> coerce (ptr void) (ptr C.Types.Point.t) null
  | Some v -> addr v
end

(* Float Points *)
type fpoint = C.Types.Fpoint.t

module Fpoint = struct
  let create ~x ~y =
    let p = make C.Types.Fpoint.t in
    setf p C.Types.Fpoint.x x;
    setf p C.Types.Fpoint.y y;
    p

  let x p = getf p C.Types.Fpoint.x
  let y p = getf p C.Types.Fpoint.y

  let set_x p x = setf p C.Types.Fpoint.x x
  let set_y p y = setf p C.Types.Fpoint.y y
end

(* Vertices *)

type vertex = C.Types.Vertex.t

module Vertex = struct
  open C.Types

  let create ~position ~color ~tex_coord =
    let v = make Vertex.t in
    setf v Vertex.position position;
    setf v Vertex.color color;
    setf v Vertex.tex_coord tex_coord;
    v

  let position v = getf v Vertex.position
  let color v = getf v Vertex.color
  let tex_coord v = getf v Vertex.tex_coord

  let set_position v position = setf v Vertex.position position
  let set_color v color = setf v Vertex.color color
  let set_tex_coord v tex_coord = setf v Vertex.tex_coord tex_coord
end

(* Rectangle *)

type rect = C.Types.Rect.t

module Rect = struct
  let create ~x ~y ~w ~h =
    let r = make C.Types.Rect.t in
    setf r C.Types.Rect.x x;
    setf r C.Types.Rect.y y;
    setf r C.Types.Rect.w w;
    setf r C.Types.Rect.h h;
    r

  let x r = getf r C.Types.Rect.x
  let y r = getf r C.Types.Rect.y
  let w r = getf r C.Types.Rect.w
  let h r = getf r C.Types.Rect.h

  let set_x r x = setf r C.Types.Rect.x x
  let set_y r y = setf r C.Types.Rect.y y
  let set_w r w = setf r C.Types.Rect.w w
  let set_h r h = setf r C.Types.Rect.h h

  let opt_addr = function
  | None -> coerce (ptr void) (ptr C.Types.Rect.t) null
  | Some v -> addr v
end

(* Float Rectangle *)

type frect = C.Types.Frect.t

module Frect = struct
  let create ~x ~y ~w ~h =
    let r = make C.Types.Frect.t in
    setf r C.Types.Frect.x x;
    setf r C.Types.Frect.y y;
    setf r C.Types.Frect.w w;
    setf r C.Types.Frect.h h;
    r

  let x r = getf r C.Types.Frect.x
  let y r = getf r C.Types.Frect.y
  let w r = getf r C.Types.Frect.w
  let h r = getf r C.Types.Frect.h

  let set_x r x = setf r C.Types.Frect.x x
  let set_y r y = setf r C.Types.Frect.y y
  let set_w r w = setf r C.Types.Frect.w w
  let set_h r h = setf r C.Types.Frect.h h
end

let enclose_points_ba ?clip ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  let res = make C.Types.Rect.t in
  if C.Functions.enclose_points ps count (Rect.opt_addr clip) (addr res)
  then Some res
  else None

let enclose_points ?clip ps =
  let a = CArray.of_list C.Types.Point.t ps in
  let ps = to_voidp (CArray.start a) in
  let res = make C.Types.Rect.t in
  if C.Functions.enclose_points ps (CArray.length a) (Rect.opt_addr clip) (addr res)
  then Some res
  else None

let has_intersection a b =
  C.Functions.has_intersection (addr a) (addr b)

let intersect_rect a b =
  let res = make C.Types.Rect.t in
  if C.Functions.intersect_rect (addr a) (addr b) (addr res)
  then Some res
  else None

let intersect_rect_and_line r x1 y1 x2 y2 =
  let alloc v = allocate int v in
  let x1, y1 = alloc x1, alloc y1 in
  let x2, y2 = alloc x2, alloc y2 in
  if C.Functions.intersect_rect_and_line (addr r) x1 y1 x2 y2
  then Some ((!@x1, !@y1), (!@x2, !@y2))
  else None

let point_in_rect p r = C.Functions.point_in_rect (addr p) (addr r)

let rect_empty r = C.Functions.rect_empty (addr r)

let rect_equals a b = C.Functions.rect_equals (addr a) (addr b)

let union_rect a b =
  let res = make C.Types.Rect.t in
  C.Functions.union_rect (addr a) (addr b) (addr res);
  res

(* Palettes *)

type palette = C.Types.palette ptr

let unsafe_palette_of_ptr addr : palette =
  from_voidp C.Types.palette (ptr_of_raw_address addr)
let unsafe_ptr_of_palette palette =
  raw_address_of_ptr (to_voidp palette)

let alloc_palette x = some_to_ok (C.Functions.alloc_palette x)

let free_palette = C.Functions.free_palette

let get_palette_ncolors p =
  getf (!@ p) C.Types.palette_ncolors

let get_palette_colors p =
  let ps = !@ p in
  CArray.to_list
    (CArray.from_ptr
       (getf ps C.Types.palette_colors)
       (getf ps C.Types.palette_ncolors))

let get_palette_colors_ba p =
  let ps = !@ p in
  (* FIXME: ctypes should have a CArray.copy function *)
  let n = getf ps C.Types.palette_ncolors in
  let ba = Bigarray.(Array1.create int8_unsigned c_layout (n * 4)) in
  let ba_ptr =
    CArray.from_ptr (coerce (ptr int) (ptr color) (bigarray_start array1 ba)) n
  in
  let ca = CArray.from_ptr (getf ps C.Types.palette_colors) n in
  for i = 0 to n - 1 do CArray.set ba_ptr i (CArray.get ca i) done;
  ba

let set_palette_colors x y z t =
  zero_to_ok (C.Functions.set_palette_colors x y z t)

let set_palette_colors_ba p cs ~fst =
  let len = Bigarray.Array1.dim cs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let cs = to_voidp (bigarray_start array1 cs) in
  set_palette_colors p cs fst count

let set_palette_colors p cs ~fst =
  let a = CArray.of_list color cs in
  set_palette_colors p (to_voidp (CArray.start a)) fst (CArray.length a)

(* Pixel formats *)

type gamma_ramp = (int, Bigarray.int16_unsigned_elt) bigarray

let calculate_gamma_ramp g =
  let ba = Bigarray.(Array1.create int16_unsigned c_layout 256) in
  C.Functions.calculate_gamma_ramp g (bigarray_start array1 ba);
  ba

module Blend = struct
  type operation = int
  type factor = int
  include C.Types.Blend
end

let compose_custom_blend_mode = C.Functions.compose_custom_blend_mode

module Pixel = struct
  type format_enum = Unsigned.UInt32.t
  let to_uint32 = Unsigned.UInt32.to_int32
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  include C.Types.Pixel
end

(* Note. Giving direct access to the palette field of SDL_PixelFormat
   is problematic. We can't ensure the pointer won't become invalid at
   a certain point. *)


type pixel_format = C.Types.pixel_format ptr
let unsafe_pixel_format_of_ptr addr : pixel_format =
  from_voidp C.Types.pixel_format (ptr_of_raw_address addr)
let unsafe_ptr_of_pixel_format pixel_format =
  raw_address_of_ptr (to_voidp pixel_format)

let alloc_format x = some_to_ok (C.Functions.alloc_format x)

let free_format = C.Functions.free_format

let get_pixel_format_name = C.Functions.get_pixel_format_name

let get_pixel_format_format pf =
  getf (!@ pf) C.Types.pf_format

let get_pixel_format_bits_pp pf =
  Unsigned.UInt8.to_int (getf (!@ pf) C.Types.pf_bits_per_pixel)

let get_pixel_format_bytes_pp pf =
  Unsigned.UInt8.to_int (getf (!@ pf) C.Types.pf_bytes_per_pixel)

let get_rgb pf p =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let to_int = Unsigned.UInt8.to_int in
  let r, g, b = alloc (), alloc (), alloc () in
  C.Functions.get_rgb (Unsigned.UInt32.of_int32 p) pf r g b;
   to_int (!@ r), to_int (!@ g), to_int (!@ b)

let get_rgba pf p =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let to_int = Unsigned.UInt8.to_int in
  let r, g, b, a = alloc (), alloc (), alloc (), alloc () in
  C.Functions.get_rgba (Unsigned.UInt32.of_int32 p) pf r g b a;
   to_int (!@ r), to_int (!@ g), to_int (!@ b), to_int (!@ a)

let pixel_format_enum_to_masks pf =
  let ui () = allocate uint32_t (Unsigned.UInt32.of_int 0) in
  let get iptr = Unsigned.UInt32.to_int32 (!@ iptr) in
  let bpp = allocate int 0 in
  let rm, gm, bm, am = ui (), ui (), ui (), ui () in
  if not (C.Functions.pixel_format_enum_to_masks pf bpp rm gm bm am)
  then error ()
  else Ok (!@ bpp, get rm, get gm, get bm, get am)

let map_rgb pf r g b =
  Unsigned.UInt32.to_int32
    (C.Functions.map_rgb
       pf
       (Unsigned.UInt8.of_int r)
       (Unsigned.UInt8.of_int g)
       (Unsigned.UInt8.of_int b))

let map_rgba pf r g b a =
  Unsigned.UInt32.to_int32
    (C.Functions.map_rgba
       pf
       (Unsigned.UInt8.of_int r)
       (Unsigned.UInt8.of_int g)
       (Unsigned.UInt8.of_int b)
       (Unsigned.UInt8.of_int a))

let masks_to_pixel_format_enum bpp rm gm bm am =
  C.Functions.masks_to_pixel_format_enum
    bpp
    (Unsigned.UInt32.of_int32 rm)
    (Unsigned.UInt32.of_int32 gm)
    (Unsigned.UInt32.of_int32 bm)
    (Unsigned.UInt32.of_int32 am)

let set_pixel_format_palette x y =
  zero_to_ok (C.Functions.set_pixel_format_palette x y)

(* Surface *)

type surface = C.Types.surface ptr

let unsafe_surface_of_ptr addr : surface =
  from_voidp C.Types.surface (ptr_of_raw_address addr)
let unsafe_ptr_of_surface surface =
  raw_address_of_ptr (to_voidp surface)

let blit_scaled ~src sr ~dst dr =
  zero_to_ok
    (C.Functions.blit_scaled src (Rect.opt_addr sr) dst (Rect.opt_addr dr))

let blit_surface ~src sr ~dst dr =
  zero_to_ok
    (C.Functions.blit_surface src (Rect.opt_addr sr) dst (Rect.opt_addr dr))

let convert_pixels ~w ~h ~src sp spitch ~dst dp dpitch =
  (* FIXME: we could try check bounds. *)
  let spitch = ba_kind_byte_size (Bigarray.Array1.kind sp) * spitch in
  let dpitch = ba_kind_byte_size (Bigarray.Array1.kind dp) * dpitch in
  let sp = to_voidp (bigarray_start array1 sp) in
  let dp = to_voidp (bigarray_start array1 dp) in
  zero_to_ok (C.Functions.convert_pixels w h src sp spitch dst dp dpitch)

let convert_surface s pf =
  some_to_ok (C.Functions.convert_surface s pf Unsigned.UInt32.zero)

let convert_surface_format s pf =
  some_to_ok (C.Functions.convert_surface_format s pf Unsigned.UInt32.zero)

let create_rgb_surface ~w ~h ~depth rmask gmask bmask amask =
  some_to_ok
    (C.Functions.create_rgb_surface
       Unsigned.UInt32.zero w h depth
       (Unsigned.UInt32.of_int32 rmask)
       (Unsigned.UInt32.of_int32 gmask)
       (Unsigned.UInt32.of_int32 bmask)
       (Unsigned.UInt32.of_int32 amask))

let create_rgb_surface_from p ~w ~h ~depth ~pitch rmask gmask bmask amask =
  (* FIXME: we could try check bounds. *)
  let pitch = ba_kind_byte_size (Bigarray.Array1.kind p) * pitch in
  let p = to_voidp (bigarray_start array1 p) in
  some_to_ok
    (C.Functions.create_rgb_surface_from
       p w h depth pitch
       (Unsigned.UInt32.of_int32 rmask)
       (Unsigned.UInt32.of_int32 gmask)
       (Unsigned.UInt32.of_int32 bmask)
       (Unsigned.UInt32.of_int32 amask))

let create_rgb_surface_with_format ~w ~h ~depth format =
  some_to_ok
    (C.Functions.create_rgb_surface_with_format
       Unsigned.UInt32.zero w h depth format)

let create_rgb_surface_with_format_from p ~w ~h ~depth ~pitch format =
  (* FIXME: check bounds? *)
  let pitch = ba_kind_byte_size (Bigarray.Array1.kind p) * pitch in
  let p = to_voidp (bigarray_start array1 p) in
  some_to_ok
    (C.Functions.create_rgb_surface_with_format_from p w h depth pitch format)

let duplicate_surface = C.Functions.duplicate_surface

let fill_rect s r c =
  zero_to_ok
    (C.Functions.fill_rect s (Rect.opt_addr r) (Unsigned.UInt32.of_int32 c))

let fill_rects_ba s rs col =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  zero_to_ok (C.Functions.fill_rects s rs count (Unsigned.UInt32.of_int32 col))

let fill_rects s rs col =
  let a = CArray.of_list C.Types.Rect.t rs in
  let col = Unsigned.UInt32.of_int32 col in
  zero_to_ok
    (C.Functions.fill_rects s (to_voidp (CArray.start a)) (CArray.length a) col)

let free_surface = C.Functions.free_surface

let get_clip_rect s =
  let r = make C.Types.Rect.t in
  (C.Functions.get_clip_rect s (addr r); r)

let get_color_key s =
  let key = allocate uint32_t Unsigned.UInt32.zero in
  match C.Functions.get_color_key s key with
  | 0 -> Ok (Unsigned.UInt32.to_int32 (!@ key)) | _ -> error ()

let get_surface_alpha_mod s =
  let alpha = allocate uint8_t Unsigned.UInt8.zero in
  match C.Functions.get_surface_alpha_mod s alpha with
  | 0 -> Ok (Unsigned.UInt8.to_int (!@ alpha)) | _ -> error ()

let get_surface_blend_mode s =
  let mode = allocate Blend.mode Blend.mode_invalid in
  match C.Functions.get_surface_blend_mode s mode with
  0 -> Ok (!@ mode) | _ -> error ()

let get_surface_color_mod s =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let get v = Unsigned.UInt8.to_int (!@ v) in
  let r, g, b = alloc (), alloc (), alloc () in
  match C.Functions.get_surface_color_mod s r g b with
  | 0 -> Ok (get r, get g, get b) | _ -> error ()

let get_surface_format_enum s =
  (* We don't give direct access to the format field. This prevents
     memory ownership problems. *)
  get_pixel_format_format (getf (!@ s) C.Types.surface_format)

let get_surface_pitch s =
  getf (!@ s) C.Types.surface_pitch

let get_surface_pixels s kind =
  let pitch = get_surface_pitch s in
  let kind_size = ba_kind_byte_size kind in
  if pitch mod kind_size <> 0
  then invalid_arg (err_bigarray_pitch pitch kind_size)
  else
  let h = getf (!@ s) C.Types.surface_h in
  let ba_size = (pitch * h) / kind_size in
  let pixels = getf (!@ s) C.Types.surface_pixels in
  let pixels = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) pixels in
  bigarray_of_ptr array1 ba_size kind pixels

let get_surface_size s =
  getf (!@ s) C.Types.surface_w, getf (!@ s) C.Types.surface_h

let load_bmp_rw rw ~close =
  some_to_ok (C.Functions.load_bmp_rw rw close)

let load_bmp file =
  (* SDL_LoadBMP is cpp based *)
  match rw_from_file file "rb" with
  | Error _ as e -> e
  | Ok rw -> load_bmp_rw rw ~close:true

let lock_surface x = zero_to_ok (C.Functions.lock_surface x)

let lower_blit ~src sr ~dst dr =
  zero_to_ok (C.Functions.lower_blit src (addr sr) dst (addr dr))

let lower_blit_scaled ~src sr ~dst dr =
  zero_to_ok
    (C.Functions.lower_blit_scaled src (addr sr) dst (addr dr))

let save_bmp_rw s rw ~close =
  zero_to_ok (C.Functions.save_bmp_rw s rw close)

let save_bmp s file =
  (* SDL_SaveBMP is cpp based *)
  match rw_from_file file "wb" with
  | Error _ as e -> e
  | Ok rw -> save_bmp_rw s rw ~close:true

let set_clip_rect s r =
  C.Functions.set_clip_rect s (addr r)

let set_color_key s b x =
  zero_to_ok (C.Functions.set_color_key s b (Unsigned.UInt32.of_int32 x))

let set_surface_alpha_mod s x =
  zero_to_ok (C.Functions.set_surface_alpha_mod s (Unsigned.UInt8.of_int x))

let set_surface_blend_mode s x =
  zero_to_ok (C.Functions.set_surface_blend_mode s x)

let set_surface_color_mod s x y z =
  zero_to_ok
    (C.Functions.set_surface_color_mod
       s
       (Unsigned.UInt8.of_int x)
       (Unsigned.UInt8.of_int y)
       (Unsigned.UInt8.of_int z))

let set_surface_palette s p =
  zero_to_ok (C.Functions.set_surface_palette s p)

let set_surface_rle s b =
  zero_to_ok (C.Functions.set_surface_rle s b)

let unlock_surface = C.Functions.unlock_surface

(* Renderers *)

type flip = int
module Flip = struct
  let ( + ) = ( lor )
  include C.Types.Flip
end

type texture = C.Types.Texture.t ptr

let unsafe_texture_of_ptr addr : texture =
  from_voidp C.Types.Texture.t (ptr_of_raw_address addr)
let unsafe_ptr_of_texture texture =
  raw_address_of_ptr (to_voidp texture)

module Renderer = struct
  type flags = Unsigned.uint32
  let ( + ) = Unsigned.UInt32.logor
  let ( - ) f f' = Unsigned.UInt32.(logand f (lognot f'))
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  include C.Types.Renderer
end

type renderer = Renderer.t ptr

let unsafe_renderer_of_ptr addr : renderer =
  from_voidp Renderer.t (Ctypes.ptr_of_raw_address addr)
let unsafe_ptr_of_renderer renderer =
  raw_address_of_ptr (to_voidp renderer)

type renderer_info =
  { ri_name : string;
    ri_flags : Renderer.flags;
    ri_texture_formats : Pixel.format_enum list;
    ri_max_texture_width : int;
    ri_max_texture_height : int; }

let renderer_info_of_c c =
  let ri_name = getf c C.Types.ri_name in
  let ri_flags = getf c C.Types.ri_flags in
  let num_tf = Unsigned.UInt32.to_int (getf c C.Types.ri_num_tf) in
  let tfs = getf c C.Types.ri_tfs in
  let ri_texture_formats =
    let acc = ref [] in
    for i = 0 to num_tf - 1 do acc := (CArray.get tfs i) :: !acc done;
    List.rev !acc
  in
  let ri_max_texture_width = getf c C.Types.ri_max_texture_width in
  let ri_max_texture_height = getf c C.Types.ri_max_texture_height in
  { ri_name; ri_flags; ri_texture_formats; ri_max_texture_width;
    ri_max_texture_height }

let create_renderer ?(index = -1) ?(flags = Unsigned.UInt32.zero) w =
  some_to_ok (C.Functions.create_renderer w index flags)

let create_software_renderer s =
  some_to_ok (C.Functions.create_software_renderer s)

let destroy_renderer = C.Functions.destroy_renderer

let get_num_render_drivers () =
  nat_to_ok (C.Functions.get_num_render_drivers ())

let get_render_draw_blend_mode r =
  let m = allocate Blend.mode Blend.mode_invalid in
  match C.Functions.get_render_draw_blend_mode r m with
  | 0 -> Ok !@m | _ -> error ()

let get_render_draw_color rend =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let get v = Unsigned.UInt8.to_int (!@ v) in
  let r, g, b, a = alloc (), alloc (), alloc (), alloc () in
  match C.Functions.get_render_draw_color rend r g b a with
  | 0 -> Ok (get r, get g, get b, get a) | _ -> error ()

let get_render_driver_info i =
  let info = make C.Types.renderer_info in
  match C.Functions.get_render_driver_info i (addr info) with
  | 0 -> Ok (renderer_info_of_c info) | _ -> error ()

let get_render_target = C.Functions.get_render_target

let get_renderer w =
  some_to_ok (C.Functions.get_renderer w)

let get_renderer_info r =
  let info = make C.Types.renderer_info in
  match C.Functions.get_renderer_info r (addr info) with
  | 0 -> Ok (renderer_info_of_c info) | _ -> error ()

let get_renderer_output_size r =
  let w = allocate int 0 in
  let h = allocate int 0 in
  match C.Functions.get_renderer_output_size r w h with
  | 0 -> Ok (!@ w, !@ h) | _ -> error ()

let render_clear r =
  zero_to_ok (C.Functions.render_clear r)

let render_copy ?src ?dst r t =
  zero_to_ok
    (C.Functions.render_copy r t (Rect.opt_addr src) (Rect.opt_addr dst))

let render_copy_ex ?src ?dst r t angle c flip =
  zero_to_ok
    (C.Functions.render_copy_ex r t (Rect.opt_addr src) (Rect.opt_addr dst)
       angle (Point.opt_addr c) flip)

let render_draw_line r a b c d =
  zero_to_ok (C.Functions.render_draw_line r a b c d)

let render_draw_line_f r a b c d =
  zero_to_ok (C.Functions.render_draw_line_f r a b c d)

let render_draw_lines_ba r ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  zero_to_ok (C.Functions.render_draw_lines r ps count)

let render_draw_lines r ps =
  let a = CArray.of_list C.Types.Point.t ps in
  zero_to_ok (C.Functions.render_draw_lines
                r (to_voidp (CArray.start a)) (CArray.length a))

let render_draw_point r a b =
  zero_to_ok (C.Functions.render_draw_point r a b)

let render_draw_points_ba r ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  zero_to_ok (C.Functions.render_draw_points r ps count)

let render_draw_points r ps =
  let a = CArray.of_list C.Types.Point.t ps in
  zero_to_ok (C.Functions.render_draw_points
                r (to_voidp (CArray.start a)) (CArray.length a))

let render_draw_point_f r a b =
  zero_to_ok (C.Functions.render_draw_point_f r a b)

let render_draw_points_f_ba r ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  zero_to_ok (C.Functions.render_draw_points_f r ps count)

let render_draw_points_f r ps =
  let a = CArray.of_list C.Types.Fpoint.t ps in
  zero_to_ok (C.Functions.render_draw_points_f
                r (to_voidp (CArray.start a)) (CArray.length a))

let render_draw_rect rend r =
  zero_to_ok (C.Functions.render_draw_rect rend (Rect.opt_addr r))

let render_draw_rects_ba r rs =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  zero_to_ok (C.Functions.render_draw_rects r rs count)

let render_draw_rects r rs =
  let a = CArray.of_list C.Types.Rect.t rs in
  zero_to_ok (C.Functions.render_draw_rects
                r (to_voidp (CArray.start a)) (CArray.length a))

let render_fill_rect rend r =
  zero_to_ok (C.Functions.render_fill_rect rend (Rect.opt_addr r))

let render_fill_rects_ba r rs =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  zero_to_ok (C.Functions.render_fill_rects r rs count)

let render_fill_rects r rs =
  let a = CArray.of_list C.Types.Rect.t rs in
  zero_to_ok (C.Functions.render_fill_rects
                r (to_voidp (CArray.start a)) (CArray.length a))

let render_geometry ?indices ?texture r vertices =
  let a1 = CArray.of_list C.Types.Vertex.t vertices in
  let a2_ptr, a2_len = match indices with
  | None -> (None, 0)
  | Some is ->
      let a2 = CArray.of_list int is in
      (Some (CArray.start a2), CArray.length a2)
  in
  zero_to_ok
    (C.Functions.render_geometry
       r texture (CArray.start a1) (CArray.length a1) a2_ptr a2_len)

let render_geometry_raw
    ?indices ?texture r ~xy ?(xy_stride = 8) ~color ?(color_stride = 4)
    ~uv ?(uv_stride = 8) ~num_vertices ()
  =
  let i_ptr, i_len = match indices with
  | None -> null, 0
  | Some is -> to_voidp (bigarray_start array1 is), Bigarray.Array1.dim is
  in
  let i_stride = 4 in (* indices are assumed to be 4-byte integers *)
  let xy_ptr = bigarray_start array1 xy in
  let xy_len_bytes = Bigarray.Array1.dim xy * 4 in
  let xy_exp_bytes = num_vertices * xy_stride - (xy_stride - 8) in
  if xy_len_bytes < xy_exp_bytes then begin
    let msg = "xy " ^ err_array_to_short ~exp:xy_exp_bytes ~fnd:xy_len_bytes in
    invalid_arg msg
  end;
  let color_ptr = to_voidp (bigarray_start array1 color) in
  let color_len_bytes = Bigarray.Array1.dim color in
  let color_exp_bytes = num_vertices * color_stride - (color_stride - 4) in
  if color_len_bytes < color_exp_bytes then begin
    let msg =
      "color " ^ err_array_to_short ~exp:color_exp_bytes ~fnd:color_len_bytes
    in
    invalid_arg msg
  end;
  let uv_ptr = bigarray_start array1 uv in
  let uv_len_bytes = Bigarray.Array1.dim uv * 4 in
  let uv_exp_bytes = num_vertices * uv_stride - (uv_stride - 8) in
  if uv_len_bytes < uv_exp_bytes then begin
    let msg =
      "uv " ^ err_array_to_short ~exp:uv_exp_bytes ~fnd:uv_len_bytes
    in
    invalid_arg msg
  end;
  zero_to_ok
    (C.Functions.render_geometry_raw
       r texture xy_ptr xy_stride color_ptr color_stride uv_ptr uv_stride
       num_vertices i_ptr i_len i_stride)

let render_get_clip_rect rend =
  let r = make C.Types.Rect.t in
  C.Functions.render_get_clip_rect rend (addr r);
  r

let render_is_clip_enabled = C.Functions.render_is_clip_enabled

let render_get_integer_scale = C.Functions.render_get_integer_scale

let render_get_logical_size r =
  let w = allocate int 0 in
  let h = allocate int 0 in
  C.Functions.render_get_logical_size r w h;
  !@ w, !@ h

let render_get_scale r =
  let x = allocate float 0. in
  let y = allocate float 0. in
  C.Functions.render_get_scale r x y;
  !@ x, !@ y

let render_get_viewport rend =
  let r = make C.Types.Rect.t in
  C.Functions.render_get_viewport rend (addr r);
  r

let render_present = C.Async_functions.render_present

let render_read_pixels r rect format pixels pitch =
  let format = match format with None -> Unsigned.UInt32.zero | Some f -> f in
  let pixels = to_voidp (bigarray_start array1 pixels) in
  zero_to_ok (C.Functions.render_read_pixels
                r (Rect.opt_addr rect) format pixels pitch)

let render_set_clip_rect rend r =
  zero_to_ok (C.Functions.render_set_clip_rect rend (Rect.opt_addr r))

let render_set_integer_scale r b =
  zero_to_ok (C.Functions.render_set_integer_scale r b)

let render_set_logical_size r x y =
  zero_to_ok (C.Functions.render_set_logical_size r x y)

let render_set_scale r x y =
  zero_to_ok (C.Functions.render_set_scale r x y)

let render_set_viewport rend r =
  zero_to_ok (C.Functions.render_set_viewport rend (Rect.opt_addr r))

let render_target_supported = C.Functions.render_target_supported

let set_render_draw_blend_mode r x =
  zero_to_ok (C.Functions.set_render_draw_blend_mode r x)

let set_render_draw_color r a b c d =
  zero_to_ok (C.Functions.set_render_draw_color r
                (Unsigned.UInt8.of_int a)
                (Unsigned.UInt8.of_int b)
                (Unsigned.UInt8.of_int c)
                (Unsigned.UInt8.of_int d))

let set_render_target r t =
  zero_to_ok (C.Functions.set_render_target r t)

(* Textures *)

module Texture = struct
  type access = int
  type modulate = Unsigned.uint32
  include C.Types.Texture
end

let create_texture r pf access ~w ~h =
  some_to_ok (C.Functions.create_texture r pf access w h)

let create_texture_from_surface r s =
  some_to_ok (C.Functions.create_texture_from_surface r s)

let destroy_texture = C.Functions.destroy_texture

let get_texture_alpha_mod t =
  let alpha = allocate uint8_t Unsigned.UInt8.zero in
  match C.Functions.get_texture_alpha_mod t alpha with
  | 0 -> Ok (Unsigned.UInt8.to_int (!@ alpha)) | _ -> error ()

let get_texture_blend_mode t =
  let m = allocate Blend.mode Blend.mode_invalid in
  match C.Functions.get_texture_blend_mode t m with
  | 0 -> Ok (!@ m) | _ -> error ()

let get_texture_color_mod t =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let get v = Unsigned.UInt8.to_int (!@ v) in
  let r, g, b = alloc (), alloc (), alloc () in
  match C.Functions.get_texture_color_mod t r g b with
  | 0 -> Ok (get r, get g, get b) | _ -> error ()

let _texture_height t =
  let h = allocate int 0 in
  let unull = coerce (ptr void) (ptr uint32_t) null in
  let inull = coerce (ptr void) (ptr int) null in
  match C.Functions.query_texture t unull inull inull h with
  | 0 -> Ok (!@ h) | _ -> error ()

let lock_texture t r kind =
  match (match r with None -> _texture_height t | Some r -> Ok (Rect.h r)) with
  | Error _ as e -> e
  | Ok h ->
      let pitch = allocate int 0 in
      let p = allocate (ptr void) null in
      match C.Functions.lock_texture t (Rect.opt_addr r) p pitch with
      | 0 ->
          let p = !@ p in
          let pitch = !@ pitch in
          let kind_size = ba_kind_byte_size kind in
          if pitch mod kind_size <> 0
          then invalid_arg (err_bigarray_pitch pitch kind_size)
          else
          let ba_size = (pitch * h) / kind_size in
          let pixels = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
          Ok (bigarray_of_ptr array1 ba_size kind pixels, pitch / kind_size)
      | _ -> error ()

let query_texture t =
  let pf = allocate uint32_t Unsigned.UInt32.zero in
  let access = allocate int 0 in
  let w = allocate int 0 in
  let h = allocate int 0 in
  match C.Functions.query_texture t pf access w h with
  | 0 -> Ok (!@ pf, !@ access, (!@ w, !@ h)) | _ -> error ()

let set_texture_alpha_mod t a =
  zero_to_ok (C.Functions.set_texture_alpha_mod t (Unsigned.UInt8.of_int a))

let set_texture_blend_mode t b =
  zero_to_ok (C.Functions.set_texture_blend_mode t b)

let set_texture_color_mod t a b c =
  zero_to_ok (C.Functions.set_texture_color_mod
                t
                (Unsigned.UInt8.of_int a)
                (Unsigned.UInt8.of_int b)
                (Unsigned.UInt8.of_int c))

let unlock_texture = C.Functions.unlock_texture

let update_texture t rect pixels pitch =
  let pitch = pitch * (ba_kind_byte_size (Bigarray.Array1.kind pixels)) in
  let pixels = to_voidp (bigarray_start array1 pixels) in
  zero_to_ok (C.Functions.update_texture t (Rect.opt_addr rect) pixels pitch)

let update_yuv_texture r rect ~y ypitch ~u upitch ~v vpitch =
  let yp = bigarray_start array1 y in
  let up = bigarray_start array1 u in
  let vp = bigarray_start array1 v in
  zero_to_ok (C.Functions.update_yuv_texture
                r (Rect.opt_addr rect) yp ypitch up upitch vp vpitch)

(* Video drivers *)

let get_current_video_driver = C.Functions.get_current_video_driver

let get_num_video_drivers () =
  nat_to_ok (C.Functions.get_num_video_drivers ())

let get_video_driver x = some_to_ok (C.Functions.get_video_driver x)

let video_init s = zero_to_ok (C.Functions.video_init s)

let video_quit = C.Functions.video_quit

(* Displays *)

type driverdata = unit ptr

type display_mode =
  { dm_format : Pixel.format_enum;
    dm_w : int;
    dm_h : int;
    dm_refresh_rate : int option;
    dm_driverdata : driverdata option }

let display_mode_to_c o =
  let c = make C.Types.display_mode in
  let rate = match o.dm_refresh_rate with None -> 0 | Some r -> r in
  setf c C.Types.dm_format o.dm_format;
  setf c C.Types.dm_w o.dm_w;
  setf c C.Types.dm_h o.dm_h;
  setf c C.Types.dm_refresh_rate rate;
  setf c C.Types.dm_driverdata o.dm_driverdata;
  c

let display_mode_of_c c =
  let dm_format = getf c C.Types.dm_format in
  let dm_w = getf c C.Types.dm_w in
  let dm_h = getf c C.Types.dm_h in
  let dm_refresh_rate = match getf c C.Types.dm_refresh_rate with
  | 0 -> None | r -> Some r
  in
  let dm_driverdata = getf c C.Types.dm_driverdata in
  { dm_format; dm_w; dm_h; dm_refresh_rate; dm_driverdata }

let get_closest_display_mode i m =
  let mode = display_mode_to_c m in
  let closest = make C.Types.display_mode in
  match C.Functions.get_closest_display_mode i (addr mode) (addr closest) with
  | None -> None
  | Some _ -> Some (display_mode_of_c closest)

let get_current_display_mode i =
  let mode = make C.Types.display_mode in
  match C.Functions.get_current_display_mode i (addr mode) with
  | 0 -> Ok (display_mode_of_c mode) | _ -> error ()

let get_desktop_display_mode i =
  let mode = make C.Types.display_mode in
  match C.Functions.get_desktop_display_mode i (addr mode) with
  | 0 -> Ok (display_mode_of_c mode) | _ -> error ()

let get_display_bounds i =
  let r = make C.Types.Rect.t in
  match C.Functions.get_display_bounds i (addr r) with
  | 0 -> Ok r | _ -> error ()

let get_display_dpi display =
  let diagonal = allocate float 0. in
  let horizontal = allocate float 0. in
  let vertical = allocate float 0. in
  match C.Functions.get_display_dpi display diagonal horizontal vertical with
  | 0 -> Ok (!@diagonal,!@horizontal,!@vertical)
  | _ -> error ()

let get_display_mode d i =
  let mode = make C.Types.display_mode in
  match C.Functions.get_display_mode d i (addr mode) with
  | 0 -> Ok (display_mode_of_c mode) | _ -> error ()

let get_display_usable_bounds i =
  let r = make C.Types.Rect.t in
  match C.Functions.get_display_usable_bounds i (addr r) with
  | 0 -> Ok r | _ -> error ()

let get_num_display_modes x = nat_to_ok (C.Functions.get_num_display_modes x)

let get_display_name x = some_to_ok (C.Functions.get_display_name x)

let get_num_video_displays () =
  nat_to_ok (C.Functions.get_num_video_displays ())

(* Windows *)

module Window = struct
  type flags = Unsigned.uint32
  let ( + ) = Unsigned.UInt32.logor
  let ( - ) f f' = Unsigned.UInt32.(logand f (lognot f'))
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let windowed = Unsigned.UInt32.zero
  include C.Types.Window
end

(* Video *)

type window = Window.t

let unsafe_window_of_ptr addr : Window.t =
  from_voidp Window.raw (ptr_of_raw_address addr)
let unsafe_ptr_of_window window =
  raw_address_of_ptr (to_voidp window)

let create_window t ?(x = Window.pos_undefined) ?(y = Window.pos_undefined)
    ~w ~h flags = some_to_ok (C.Functions.create_window t x y w h flags)

let create_window_and_renderer ~w ~h flags =
  let win = allocate Window.t (from_voidp Window.raw null) in
  let r = allocate (ptr Renderer.t) (from_voidp Renderer.t null) in
  match C.Functions.create_window_and_renderer w h flags win r with
  | 0 -> Ok (!@ win, !@ r) | _ -> error ()

let destroy_window = C.Functions.destroy_window

let get_window_brightness = C.Functions.get_window_brightness

let get_window_borders_size w =
  let top = allocate int 0 in
  let left = allocate int 0 in
  let bottom = allocate int 0 in
  let right = allocate int 0 in
  match C.Functions.get_window_borders_size w top bottom left right with
  | 0 -> Ok (!@ top, !@ left, !@ bottom, !@ right)
  | _ -> error ()

let get_window_display_index w =
  nat_to_ok (C.Functions.get_window_display_index w)

let get_window_display_mode w =
  let mode = make C.Types.display_mode in
  match C.Functions.get_window_display_mode w (addr mode) with
  | 0 -> Ok (display_mode_of_c mode) | _err -> error ()

let get_window_flags = C.Functions.get_window_flags

let get_window_from_id x =
  some_to_ok (C.Functions.get_window_from_id (Unsigned.UInt32.of_int x))

let get_window_gamma_ramp w =
  let create_ramp () = ba_create Bigarray.int16_unsigned 256 in
  let r, g, b = create_ramp (), create_ramp (), create_ramp () in
  let ramp_ptr r = bigarray_start array1 r in
  match C.Functions.get_window_gamma_ramp
          w (ramp_ptr r) (ramp_ptr g) (ramp_ptr b) with
  | 0 -> Ok (r, g, b) | _ -> error ()

let get_window_grab = C.Functions.get_window_grab

let get_grabbed_window = C.Functions.get_grabbed_window

let get_window_id w = Unsigned.UInt32.to_int (C.Functions.get_window_id w)

let get_window_maximum_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  C.Functions.get_window_maximum_size win w h;
  !@ w, !@ h

let get_window_minimum_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  C.Functions.get_window_minimum_size win w h;
  !@ w, !@ h

let get_window_opacity win =
  let x = allocate float 0. in
  match C.Functions.get_window_opacity win x with
  | 0 -> Ok !@x
  | _ -> error ()

let get_window_pixel_format = C.Functions.get_window_pixel_format

let get_window_position win =
  let x = allocate int 0 in
  let y = allocate int 0 in
  C.Functions.get_window_position win x y;
  !@ x, !@ y

let get_window_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  C.Functions.get_window_size win w h;
  !@ w, !@ h

let get_window_surface w =
  some_to_ok (C.Functions.get_window_surface w)

let get_window_title = C.Functions.get_window_title

let hide_window = C.Functions.hide_window

let maximize_window = C.Functions.maximize_window

let minimize_window = C.Functions.minimize_window

let raise_window = C.Functions.raise_window

let restore_window = C.Functions.restore_window

let set_window_bordered = C.Functions.set_window_bordered

let set_window_brightness w x =
  zero_to_ok (C.Functions.set_window_brightness w x)

let set_window_display_mode w m =
  let mode = display_mode_to_c m in
  zero_to_ok (C.Functions.set_window_display_mode w (addr mode))

let set_window_fullscreen w x =
  zero_to_ok (C.Functions.set_window_fullscreen w x)

let set_window_gamma_ramp w r g b =
  let ramp_ptr r = bigarray_start array1 r in
  zero_to_ok (C.Functions.set_window_gamma_ramp
                w (ramp_ptr r) (ramp_ptr g) (ramp_ptr b))

let set_window_grab = C.Functions.set_window_grab

let set_window_icon = C.Functions.set_window_icon

let set_window_input_focus w =
  zero_to_ok (C.Functions.set_window_input_focus w)

let set_window_maximum_size win ~w ~h =
  C.Functions.set_window_maximum_size win w h

let set_window_minimum_size win ~w ~h =
  C.Functions.set_window_minimum_size win w h

let set_window_modal_for ~modal ~parent =
  zero_to_ok (C.Functions.set_window_modal_for modal parent)

let set_window_opacity w x =
  zero_to_ok (C.Functions.set_window_opacity w x)

let set_window_position win ~x ~y =
  C.Functions.set_window_position win x y

let set_window_resizable = C.Functions.set_window_resizable

let set_window_size win ~w ~h =
  C.Functions.set_window_size win w h

let set_window_title = C.Functions.set_window_title

let show_window = C.Functions.show_window

let update_window_surface w =
  zero_to_ok (C.Functions.update_window_surface w)

let update_window_surface_rects_ba w rs =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  zero_to_ok (C.Functions.update_window_surface_rects w rs count)

let update_window_surface_rects w rs =
  let a = CArray.of_list C.Types.Rect.t rs in
  let rs = to_voidp (CArray.start a) in
  zero_to_ok (C.Functions.update_window_surface_rects w rs (CArray.length a))

(* OpenGL contexts *)

module Gl = struct
  type context_flags = int
  type profile = int
  type attr = int

  include C.Types.Gl
end

type gl_context = C.Types.Gl.context ptr

let unsafe_gl_context_of_ptr addr : gl_context =
  from_voidp C.Types.Gl.context  (ptr_of_raw_address addr)
let unsafe_ptr_of_gl_context gl_context =
  raw_address_of_ptr (to_voidp gl_context)

let gl_bind_texture t =
  let w = allocate float 0. in
  let h = allocate float 0. in
  match C.Functions.gl_bind_texture t w h with
  | 0 -> Ok (!@ w, !@ h) | _ -> error ()

let gl_create_context w =
  some_to_ok (C.Functions.gl_create_context w)

let gl_delete_context = C.Functions.gl_delete_context

let gl_extension_supported = C.Functions.gl_extension_supported

let gl_get_attribute att =
  let value = allocate int 0 in
  match C.Functions.gl_get_attribute att value with
  | 0 -> Ok (!@ value) | _err -> error ()

let gl_get_current_context () =
  some_to_ok (C.Functions.gl_get_current_context ())

let gl_get_drawable_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  C.Functions.gl_get_drawable_size win w h;
  (!@ w, !@ h)

let gl_get_swap_interval () = Ok (C.Functions.gl_get_swap_interval ())

let gl_make_current w g =
  zero_to_ok (C.Functions.gl_make_current w g)

let gl_reset_attributes = C.Functions.gl_reset_attributes

let gl_set_attribute x y =
  zero_to_ok (C.Functions.gl_set_attribute x y)

let gl_set_swap_interval x =
  zero_to_ok (C.Functions.gl_set_swap_interval x)

let gl_swap_window = C.Functions.gl_swap_window

let gl_unbind_texture t =
  zero_to_ok (C.Functions.gl_unbind_texture t)

(* Vulkan *)

module Vulkan = struct

  type instance = unit ptr
  let unsafe_ptr_of_instance = raw_address_of_ptr
  let unsafe_instance_of_ptr x = ptr_of_raw_address x

  type surface = C.Types.Vulkan.surface
  let unsafe_uint64_of_surface x =
    Int64.of_nativeint (raw_address_of_ptr (to_voidp x))
  let unsafe_surface_of_uint64 x =
    from_voidp C.Types.Vulkan.raw_surface
      (ptr_of_raw_address (Int64.to_nativeint x))

  let load_library s =
    zero_to_ok (C.Functions.Vulkan.load_library s)

  let unload_library = C.Functions.Vulkan.unload_library

  let get_instance_extensions window =
    let n = allocate int 0 in
    match C.Functions.Vulkan.get_instance_extensions window n
            (Ctypes.coerce (ptr void) (ptr string) null) with
    | false -> None
    | true ->
        let exts = allocate_n string ~count:(!@n) in
        match C.Functions.Vulkan.get_instance_extensions window n exts with
        | false -> None
        | true -> Some CArray.(to_list @@ from_ptr exts (!@n))

  let create_surface window instance =
    let s = allocate_n C.Types.Vulkan.surface ~count:1 in
    if C.Functions.Vulkan.create_surface window instance s then
      Some !@s
    else
    None

  let get_drawable_size window =
    let w = allocate int 0 in
    let h = allocate int 0 in
    C.Functions.Vulkan.get_drawable_size window w h;
    !@w, !@h
end

(* Screen saver *)

let disable_screen_saver = C.Functions.disable_screen_saver

let enable_screen_saver = C.Functions.enable_screen_saver

let is_screen_saver_enabled = C.Functions.is_screen_saver_enabled

(* Message boxes *)

module Message_box = struct
  include C.Types.Message_box

  type button_flags = Unsigned.uint32
  let button_no_default = Unsigned.UInt32.zero

  type button_data =
    { button_flags : button_flags;
      button_id : int;
      button_text : string }

  type flags = Unsigned.uint32

  type color = int * int * int

  type color_scheme =
    { color_background : color;
      color_text : color;
      color_button_border : color;
      color_button_background : color;
      color_button_selected : color; }

  type data =
    { flags : flags;
      window : Window.t option;
      title : string;
      message : string;
      buttons : button_data list;
      color_scheme : color_scheme option }

  let buttons_to_c bl =
    let button_data_to_c b =
      let bt = make button_data in
      setf bt button_flags b.button_flags;
      setf bt button_buttonid b.button_id;
      setf bt button_text b.button_text;
      bt
    in
    CArray.start (CArray.of_list button_data (List.map button_data_to_c bl))

  let color_scheme_to_c s =
    let st = make color_scheme in
    let arr = getf st colors in
    let set i (rv, gv, bv) =
      let ct = CArray.get arr i in
      setf ct color_r (Unsigned.UInt8.of_int rv);
      setf ct color_g (Unsigned.UInt8.of_int gv);
      setf ct color_b (Unsigned.UInt8.of_int bv);
    in
    set color_background s.color_background;
    set color_text s.color_text;
    set color_button_border s.color_button_border;
    set color_button_background s.color_button_background;
    set color_button_selected s.color_button_selected;
    st

  let data_to_c d =
    let dt = make data in
    setf dt d_flags d.flags;
    setf dt d_window d.window;
    setf dt d_title d.title;
    setf dt d_message d.message;
    setf dt d_numbuttons (List.length d.buttons);
    setf dt d_buttons (buttons_to_c d.buttons);
    setf dt d_color_scheme
      (Option.map (fun s -> addr (color_scheme_to_c s)) d.color_scheme);
    dt
end

let show_message_box d =
  let d = addr (Message_box.data_to_c d) in
  let ret = allocate int 0 in
  match C.Functions.Message_box.show d ret with
  | 0 -> Ok (!@ ret) | _ -> error ()

let show_simple_message_box t ~title msg w =
  zero_to_ok (C.Functions.Message_box.show_simple t title msg w)

(* Clipboard *)

let get_clipboard_text () =
  let p = C.Functions.get_clipboard_text () in
  if (to_voidp p) = null then error () else
  let b = Buffer.create 255 in
  let ptr = ref p in
  while (!@ !ptr) <> '\000' do
    Buffer.add_char b (!@ !ptr);
    ptr := !ptr +@ 1;
  done;
  sdl_free (to_voidp p);
  Ok (Buffer.contents b)

let has_clipboard_text = C.Functions.has_clipboard_text

let set_clipboard_text s =
  zero_to_ok (C.Functions.set_clipboard_text s)

(* Input *)

type button_state = Unsigned.uint8
let pressed = C.Types.pressed
let released = C.Types.released

type toggle_state = Unsigned.uint8
let disable = C.Types.disable
let enable = C.Types.enable

(* Keyboard *)

type scancode = int

module Scancode = struct
  include C.Types.Scancode
  let enum_of_scancode = [|
    `Unknown; `Unknown; `Unknown; `Unknown; `A; `B; `C; `D; `E; `F;
    `G; `H; `I; `J; `K; `L; `M; `N; `O; `P; `Q; `R; `S; `T; `U; `V;
    `W; `X; `Y; `Z; `K1; `K2; `K3; `K4; `K5; `K6; `K7; `K8; `K9; `K0;
    `Return; `Escape; `Backspace; `Tab; `Space; `Minus; `Equals;
    `Leftbracket; `Rightbracket; `Backslash; `Nonushash; `Semicolon;
    `Apostrophe; `Grave; `Comma; `Period; `Slash; `Capslock; `F1; `F2;
    `F3; `F4; `F5; `F6; `F7; `F8; `F9; `F10; `F11; `F12; `Printscreen;
    `Scrolllock; `Pause; `Insert; `Home; `Pageup; `Delete; `End;
    `Pagedown; `Right; `Left; `Down; `Up; `Numlockclear; `Kp_divide;
    `Kp_multiply; `Kp_minus; `Kp_plus; `Kp_enter; `Kp_1; `Kp_2; `Kp_3;
    `Kp_4; `Kp_5; `Kp_6; `Kp_7; `Kp_8; `Kp_9; `Kp_0; `Kp_period;
    `Nonusbackslash; `Application; `Power; `Kp_equals; `F13; `F14;
    `F15; `F16; `F17; `F18; `F19; `F20; `F21; `F22; `F23; `F24;
    `Execute; `Help; `Menu; `Select; `Stop; `Again; `Undo; `Cut;
    `Copy; `Paste; `Find; `Mute; `Volumeup; `Volumedown; `Unknown;
    `Unknown; `Unknown; `Kp_comma; `Kp_equalsas400; `International1;
    `International2; `International3; `International4;
    `International5; `International6; `International7;
    `International8; `International9; `Lang1; `Lang2; `Lang3; `Lang4;
    `Lang5; `Lang6; `Lang7; `Lang8; `Lang9; `Alterase; `Sysreq;
    `Cancel; `Clear; `Prior; `Return2; `Separator; `Out; `Oper;
    `Clearagain; `Crsel; `Exsel; `Unknown; `Unknown; `Unknown;
    `Unknown; `Unknown; `Unknown; `Unknown; `Unknown; `Unknown;
    `Unknown; `Unknown; `Kp_00; `Kp_000; `Thousandsseparator;
    `Decimalseparator; `Currencyunit; `Currencysubunit; `Kp_leftparen;
    `Kp_rightparen; `Kp_leftbrace; `Kp_rightbrace; `Kp_tab;
    `Kp_backspace; `Kp_a; `Kp_b; `Kp_c; `Kp_d; `Kp_e; `Kp_f; `Kp_xor;
    `Kp_power; `Kp_percent; `Kp_less; `Kp_greater; `Kp_ampersand;
    `Kp_dblampersand; `Kp_verticalbar; `Kp_dblverticalbar; `Kp_colon;
    `Kp_hash; `Kp_space; `Kp_at; `Kp_exclam; `Kp_memstore;
    `Kp_memrecall; `Kp_memclear; `Kp_memadd; `Kp_memsubtract;
    `Kp_memmultiply; `Kp_memdivide; `Kp_plusminus; `Kp_clear;
    `Kp_clearentry; `Kp_binary; `Kp_octal; `Kp_decimal;
    `Kp_hexadecimal; `Unknown; `Unknown; `Lctrl; `Lshift; `Lalt;
    `Lgui; `Rctrl; `Rshift; `Ralt; `Rgui; `Unknown; `Unknown;
    `Unknown; `Unknown; `Unknown; `Unknown; `Unknown; `Unknown;
    `Unknown; `Unknown; `Unknown; `Unknown; `Unknown; `Unknown;
    `Unknown; `Unknown; `Unknown; `Unknown; `Unknown; `Unknown;
    `Unknown; `Unknown; `Unknown; `Unknown; `Unknown; `Mode;
    `Audionext; `Audioprev; `Audiostop; `Audioplay; `Audiomute;
    `Mediaselect; `Www; `Mail; `Calculator; `Computer; `Ac_search;
    `Ac_home; `Ac_back; `Ac_forward; `Ac_stop; `Ac_refresh;
    `Ac_bookmarks; `Brightnessdown; `Brightnessup; `Displayswitch;
    `Kbdillumtoggle; `Kbdillumdown; `Kbdillumup; `Eject; `Sleep;
    `App1; `App2; |]

  let enum s =
    if 0 <= s && s <= app2 then unsafe_get enum_of_scancode s else
    `Unknown
end

type keycode = int

module K = C.Types.K

type keymod = int

module Kmod = C.Types.Kmod

let get_keyboard_focus = C.Functions.get_keyboard_focus

let get_keyboard_state () =
  let count = allocate int 0 in
  let p = C.Functions.get_keyboard_state count in
  let p' = coerce (ptr uint8_t) (ptr int) p in
  let a  = CArray.from_ptr p' (!@ count) in
  bigarray_of_array array1 Bigarray.int8_unsigned a

let get_key_from_name = C.Functions.get_key_from_name

let get_key_from_scancode = C.Functions.get_key_from_scancode

let get_key_name = C.Functions.get_key_name

let get_mod_state () = Unsigned.UInt16.to_int (C.Functions.get_mod_state ())

let get_scancode_from_key = C.Functions.get_scancode_from_key

let get_scancode_from_name = C.Functions.get_scancode_from_name

let get_scancode_name = C.Functions. get_scancode_name

let has_screen_keyboard_support = C.Functions.has_screen_keyboard_support

let is_screen_keyboard_shown = C.Functions.is_screen_keyboard_shown

let is_text_input_active = C.Functions.is_text_input_active

let set_mod_state m = C.Functions.set_mod_state (Unsigned.UInt16.of_int m)

let set_text_input_rect r =
  C.Functions.set_text_input_rect (Rect.opt_addr r)

let start_text_input = C.Functions.start_text_input

let stop_text_input = C.Functions.stop_text_input

(* Mouse *)

type cursor = C.Types.cursor ptr

let unsafe_cursor_of_ptr addr : cursor =
  from_voidp C.Types.cursor (ptr_of_raw_address addr)
let unsafe_ptr_of_cursor cursor =
  raw_address_of_ptr (to_voidp cursor)

module System_cursor = struct
  type t = int
    include C.Types.System_cursor
end

module Button = C.Types.Button

let capture_mouse b = zero_to_ok (C.Functions.capture_mouse b)

let create_color_cursor s ~hot_x ~hot_y =
  some_to_ok (C.Functions.create_color_cursor s hot_x hot_y)

let create_cursor d m ~w ~h ~hot_x ~hot_y =
  (* FIXME: we could try to check bounds *)
  let d = bigarray_start array1 d in
  let m = bigarray_start array1 m in
  some_to_ok (C.Functions.create_cursor d m w h hot_x hot_y)

let create_system_cursor i =
  some_to_ok (C.Functions.create_system_cursor i)

let free_cursor = C.Functions.free_cursor

let get_cursor = C.Functions.get_cursor

let get_default_cursor = C.Functions.get_default_cursor

let get_global_mouse_state () =
  let x = allocate int 0 in
  let y = allocate int 0 in
  let s = C.Functions.get_global_mouse_state x y in
  Unsigned.UInt32.to_int32 s, (!@ x, !@ y)

let get_mouse_focus =
  C.Functions.get_mouse_focus

let get_mouse_state () =
  let x = allocate int 0 in
  let y = allocate int 0 in
  let s = C.Functions.get_mouse_state x y in
  Unsigned.UInt32.to_int32 s, (!@ x, !@ y)

let get_relative_mouse_mode = C.Functions.get_relative_mouse_mode

  let get_relative_mouse_state () =
  let x = allocate int 0 in
  let y = allocate int 0 in
  let s = C.Functions.get_relative_mouse_state x y in
  Unsigned.UInt32.to_int32 s, (!@ x, !@ y)

let get_cursor_shown () =
  bool_to_ok (C.Functions.show_cursor (-1))

let set_cursor = C.Functions.set_cursor

let set_relative_mouse_mode b =
  zero_to_ok (C.Functions.set_relative_mouse_mode b)

let show_cursor b =
  bool_to_ok (C.Functions.show_cursor (if b then 1 else 0))

let warp_mouse_in_window w ~x ~y =
  C.Functions.warp_mouse_in_window w x y

let warp_mouse_global ~x ~y =
  zero_to_ok (C.Functions.warp_mouse_global x y)

(* Touch *)

type touch_id = int64
let touch_mouse_id = C.Types.touch_mouseid

type gesture_id = int64

type finger_id = int64

module Finger = struct
  include C.Types.Finger

  let id f = getf f id
  let x f = getf f x
  let y f = getf f y
  let pressure f = getf f pressure
end
type finger = Finger.t

let get_num_touch_devices = C.Functions.get_num_touch_devices

let get_num_touch_fingers = C.Functions.get_num_touch_fingers

let get_touch_device i =
  match C.Functions.get_touch_device i with
  | 0L -> error () | id -> Ok id

let get_touch_finger id i =
  match C.Functions.get_touch_finger id i with
  | None -> None | Some p -> Some (!@ p)

let load_dollar_templates x y =
  zero_to_ok (C.Functions.load_dollar_templates x y)

let record_gesture i =
  one_to_ok (C.Functions.record_gesture i)

let save_dollar_template x y =
  zero_to_ok (C.Functions.save_dollar_template x y)

let save_all_dollar_templates o =
  zero_to_ok (C.Functions.save_all_dollar_templates o)

(* Joystick *)

type joystick_guid = C.Types.guid

type joystick_id = int32

type joystick = C.Types.joystick ptr

let unsafe_joystick_of_ptr addr : joystick =
  from_voidp C.Types.joystick (ptr_of_raw_address addr)
let unsafe_ptr_of_joystick joystick =
  raw_address_of_ptr (to_voidp joystick)

module Hat = C.Types.Hat

module Joystick_power_level = C.Types.Joystick_power_level

module Joystick_type = C.Types.Joystick_type

let joystick_close = C.Functions.joystick_close

let joystick_current_power_level = C.Functions.joystick_current_power_level

let joystick_event_state i =
  C.Functions.joystick_event_state i |> nat_to_ok |> Result.map Unsigned.UInt8.of_int

let joystick_from_instance_id = C.Functions.joystick_from_instance_id

let joystick_get_event_state () =
  joystick_event_state C.Types.sdl_query

let joystick_set_event_state s =
  joystick_event_state (Unsigned.UInt8.to_int s)

let joystick_get_attached = C.Functions.joystick_get_attached

let joystick_get_axis = C.Functions.joystick_get_axis

let joystick_get_axis_initial_state j i =
  let out = allocate int16_t 0 in
  (* FIXME: should probably be an option, no? *)
  if C.Functions.joystick_get_axis_initial_state j i out then !@ out else 0

let joystick_get_ball j i =
  let x = allocate int 0 in
  let y = allocate int 0 in
  match C.Functions.joystick_get_ball j i x y with
  | 0 -> Ok (!@ x, !@ y) | _ -> error ()

let joystick_get_button j i =
  Unsigned.UInt8.to_int (C.Functions.joystick_get_button j i)

let joystick_get_device_guid = C.Functions.joystick_get_device_guid

let joystick_get_device_product i =
  Unsigned.UInt16.to_int (C.Functions.joystick_get_device_product i)

let joystick_get_device_product_version i =
  Unsigned.UInt16.to_int (C.Functions.joystick_get_device_product_version i)

let joystick_get_device_type = C.Functions.joystick_get_device_type

let joystick_get_device_instance_id = C.Functions.joystick_get_device_instance_id

let joystick_get_device_vendor i =
  Unsigned.UInt16.to_int (C.Functions.joystick_get_device_vendor i)

let joystick_get_guid = C.Functions.joystick_get_guid

let joystick_get_guid_from_string = C.Functions.joystick_get_guid_from_string

let joystick_get_guid_string guid =
  let len = 33 in
  let s = CArray.start (CArray.make char 33) in
  C.Functions.joystick_get_guid_string guid s len;
  coerce (ptr char) string s

let joystick_get_hat j i =
    Unsigned.UInt8.to_int (C.Functions.joystick_get_hat j i)

let joystick_get_product j =
  Unsigned.UInt16.to_int (C.Functions.joystick_get_product j)

let joystick_get_product_version j =
  Unsigned.UInt16.to_int (C.Functions.joystick_get_product_version j)

let joystick_get_type = C.Functions.joystick_get_type

let joystick_get_vendor j =
  Unsigned.UInt16.to_int (C.Functions.joystick_get_vendor j)

let joystick_instance_id j =
  match C.Functions.joystick_instance_id j with
  | n when n < 0l -> error () | n -> Ok n

let joystick_name j =
  some_to_ok (C.Functions.joystick_name j)

let joystick_name_for_index i =
  some_to_ok (C.Functions.joystick_name_for_index i)

let joystick_num_axes j =
  nat_to_ok (C.Functions.joystick_num_axes j)

let joystick_num_balls j =
  nat_to_ok (C.Functions.joystick_num_balls j)

let joystick_num_buttons j =
  nat_to_ok (C.Functions.joystick_num_buttons j)

let joystick_num_hats j =
  nat_to_ok (C.Functions.joystick_num_hats j)

let joystick_open i =
  some_to_ok (C.Functions.joystick_open i)

let joystick_update = C.Functions.joystick_update

let num_joysticks () =
  nat_to_ok (C.Functions.num_joysticks ())

(* Game controller *)

type game_controller = C.Types._game_controller structure ptr

let unsafe_game_controller_of_ptr addr : game_controller =
  from_voidp C.Types.game_controller (ptr_of_raw_address addr)
let unsafe_ptr_of_game_controller game_controller =
  raw_address_of_ptr (to_voidp game_controller)

module Controller = struct
  include C.Types.Controller

  type button_bind = C.Functions._button_bind structure
  let bind_type v = getf v C.Functions.button_bind_bind_type
  let bind_button_value v = getf v C.Functions.button_bind_value1
  let bind_axis_value v = getf v C.Functions.button_bind_value1
  let bind_hat_value v =
    getf v C.Functions.button_bind_value1, getf v C.Functions.button_bind_value2
end

let game_controller_add_mapping s =
  bool_to_ok (C.Functions.game_controller_add_mapping s)

let game_controller_add_mapping_from_rw r b =
  nat_to_ok (C.Functions.game_controller_add_mapping_from_rw r b)

let game_controller_close = C.Functions.game_controller_close

let game_controller_event_state i =
  C.Functions.game_controller_event_state i |> nat_to_ok |> Result.map Unsigned.UInt8.of_int

let game_controller_from_instance_id = C.Functions.game_controller_from_instance_id

let game_controller_get_event_state () =
  game_controller_event_state C.Types.sdl_query

let game_controller_set_event_state t =
  game_controller_event_state (Unsigned.UInt8.to_int t)

let game_controller_get_attached = C.Functions.game_controller_get_attached

let game_controller_get_axis = C.Functions.game_controller_get_axis

let game_controller_get_axis_from_string =
  C.Functions.game_controller_get_axis_from_string

let game_controller_get_bind_for_axis =
  C.Functions.game_controller_get_bind_for_axis

let game_controller_get_bind_for_button =
  C.Functions.game_controller_get_bind_for_button

let game_controller_get_button c i =
  Unsigned.UInt8.to_int (C.Functions.game_controller_get_button c i)

let game_controller_get_button_from_string =
  C.Functions.game_controller_get_button_from_string

let game_controller_get_joystick c =
  some_to_ok (C.Functions.game_controller_get_joystick c)

let game_controller_get_product t =
  Unsigned.UInt16.to_int (C.Functions.game_controller_get_product t)

let game_controller_get_product_version t =
  Unsigned.UInt16.to_int (C.Functions.game_controller_get_product_version t)

let game_controller_get_string_for_axis =
  C.Functions.game_controller_get_string_for_axis

let game_controller_get_string_for_button =
  C.Functions.game_controller_get_string_for_button

let game_controller_get_vendor t =
  Unsigned.UInt16.to_int (C.Functions.game_controller_get_vendor t)

let game_controller_mapping c =
  some_to_ok (C.Functions.game_controller_mapping c)

let game_controller_mapping_for_index i =
  some_to_ok (C.Functions.game_controller_mapping_for_index i)

let game_controller_mapping_for_guid g =
  some_to_ok (C.Functions.game_controller_mapping_for_guid g)

let game_controller_name c =
  some_to_ok (C.Functions.game_controller_name c)

let game_controller_name_for_index i =
  some_to_ok (C.Functions.game_controller_name_for_index i)

let game_controller_num_mappings = C.Functions.game_controller_num_mappings

let game_controller_open i =
  some_to_ok (C.Functions.game_controller_open i)

let game_controller_update = C.Functions.game_controller_update

let is_game_controller = C.Functions.is_game_controller

(* Events *)

type event_type = int

module Event = struct
  include C.Types.Event
  (* Event structures *)

  let create () = make t
  let opt_addr = function
  | None -> coerce (ptr void) (ptr t) null
  | Some v -> addr v

  type _ field =
      F : (* existential to hide the 'a structure *)
        (('a structure, t union) Ctypes.field *
         ('b, 'a structure) Ctypes.field *
         ('b -> 'c) * ('b -> 'c -> 'b)) -> 'c field

  let get e (F (s, f, c, _)) = c (getf (getf e s) f)
  let set e (F (s, f, _, c)) v = let x = getf e s in setf x f (c (getf x f) v)

  (* Common *)

  let typ  =
    F (common, Common.typ,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let timestamp =
    F (common, Common.timestamp,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))

  (* Controller events *)

  let controller_axis_which =
    F (controller_axis_event, Controller_axis_event.which, Fun.id, (fun _ x -> x))
  let controller_axis_axis =
    F (controller_axis_event, Controller_axis_event.axis,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let controller_axis_value =
    F (controller_axis_event, Controller_axis_event.value, Fun.id, (fun _ x -> x))

  let controller_button_which =
    F (controller_button_event, Controller_button_event.which, Fun.id, (fun _ x -> x))
  let controller_button_button =
    F (controller_button_event, Controller_button_event.button,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let controller_button_state =
    F (controller_button_event, Controller_button_event.state, Fun.id, (fun _ x -> x))

  let controller_device_which =
    F (controller_device_event, Controller_device_event.which, Fun.id, (fun _ x -> x))

  let dollar_gesture_touch_id =
    F (dollar_gesture_event, Dollar_gesture_event.touch_id, Fun.id, (fun _ x -> x))
  let dollar_gesture_gesture_id =
    F (dollar_gesture_event, Dollar_gesture_event.gesture_id, Fun.id, (fun _ x -> x))
  let dollar_gesture_num_fingers =
    F (dollar_gesture_event, Dollar_gesture_event.num_fingers,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let dollar_gesture_error =
    F (dollar_gesture_event, Dollar_gesture_event.error, Fun.id, (fun _ x -> x))
  let dollar_gesture_x =
    F (dollar_gesture_event, Dollar_gesture_event.x, Fun.id, (fun _ x -> x))
  let dollar_gesture_y =
    F (dollar_gesture_event, Dollar_gesture_event.y, Fun.id, (fun _ x -> x))

  let drop_file_file = F (drop_event, Drop_event.file, Fun.id, (fun _ x -> x))
  let drop_window_id =
    F (drop_event, Drop_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))

  let drop_file_free e =
    let sp = to_voidp (get e drop_file_file) in
    if is_null sp then () else sdl_free sp

  let drop_file_file e =
    let sp = get e drop_file_file in
    if is_null sp then None else Some (coerce (ptr char) string sp)

  (* Touch events *)

  let touch_finger_touch_id =
    F (touch_finger_event,Touch_finger_event.touch_id, Fun.id, (fun _ x -> x))
  let touch_finger_finger_id =
    F (touch_finger_event, Touch_finger_event.finger_id, Fun.id, (fun _ x -> x))
  let touch_finger_x =
    F (touch_finger_event, Touch_finger_event.x, Fun.id, (fun _ x -> x))
  let touch_finger_y =
    F (touch_finger_event, Touch_finger_event.y, Fun.id, (fun _ x -> x))
  let touch_finger_dx =
    F (touch_finger_event, Touch_finger_event.dx, Fun.id, (fun _ x -> x))
  let touch_finger_dy =
    F (touch_finger_event, Touch_finger_event.dy, Fun.id, (fun _ x -> x))
  let touch_finger_pressure =
    F (touch_finger_event, Touch_finger_event.pressure, Fun.id, (fun _ x -> x))

  (* Joystick events. *)

  let joy_axis_which =
    F (joy_axis_event, Joy_axis_event.which, Fun.id, (fun _ x -> x))
  let joy_axis_axis =
    F (joy_axis_event, Joy_axis_event.axis,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let joy_axis_value =
    F (joy_axis_event, Joy_axis_event.value, Fun.id, (fun _ x -> x))

  let joy_ball_which =
    F (joy_ball_event, Joy_ball_event.which, Fun.id, (fun _ x -> x))
  let joy_ball_ball =
    F (joy_ball_event, Joy_ball_event.ball,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let joy_ball_xrel =
    F (joy_ball_event, Joy_ball_event.xrel, Fun.id, (fun _ x -> x))
  let joy_ball_yrel =
    F (joy_ball_event, Joy_ball_event.yrel, Fun.id, (fun _ x -> x))

  let joy_button_which =
    F (joy_button_event, Joy_button_event.which, Fun.id, (fun _ x -> x))
  let joy_button_button =
    F (joy_button_event, Joy_button_event.button,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let joy_button_state =
    F (joy_button_event, Joy_button_event.state, Fun.id, (fun _ x -> x))

  let joy_device_which =
    F (joy_device_event, Joy_device_event.which, Fun.id, (fun _ x -> x))

  let joy_hat_which =
    F (joy_hat_event, Joy_hat_event.which, Fun.id, (fun _ x -> x))
  let joy_hat_hat =
    F (joy_hat_event, Joy_hat_event.hat,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let joy_hat_value =
    F (joy_hat_event, Joy_hat_event.value,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))

  (* Keyboard events *)

  let keyboard_window_id =
    F (keyboard_event, Keyboard_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let keyboard_repeat =
    F (keyboard_event, Keyboard_event.repeat,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let keyboard_state =
    F (keyboard_event, Keyboard_event.state, Fun.id, (fun _ x -> x))
  let keyboard_scancode =
    F (keyboard_event, Keyboard_event.keysym,
       (fun k -> getf k Keyboard_event.scancode),
       (fun k v -> let () = setf k Keyboard_event.scancode v in k))
  let keyboard_keycode =
    F (keyboard_event, Keyboard_event.keysym,
       (fun k -> getf k Keyboard_event.keycode),
       (fun k v -> let () = setf k Keyboard_event.keycode v in k))
  let keyboard_keymod =
    F (keyboard_event, Keyboard_event.keysym,
       (fun k -> Unsigned.UInt16.to_int (getf k Keyboard_event.keymod)),
       (fun k v ->
         let () = setf k Keyboard_event.keymod (Unsigned.UInt16.of_int v) in k))

  (* Mouse events *)

  let mouse_button_window_id =
    F (mouse_button_event, Mouse_button_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let mouse_button_which =
    F (mouse_button_event, Mouse_button_event.which,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))
  let mouse_button_state =
    F (mouse_button_event, Mouse_button_event.state, Fun.id, (fun _ x -> x))
  let mouse_button_button =
    F (mouse_button_event, Mouse_button_event.button,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let mouse_button_clicks =
    F (mouse_button_event, Mouse_button_event.clicks,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let mouse_button_x =
    F (mouse_button_event, Mouse_button_event.x,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let mouse_button_y =
    F (mouse_button_event, Mouse_button_event.y,
       Int32.to_int, (fun _ x -> Int32.of_int x))

  let mouse_motion_window_id =
    F (mouse_motion_event, Mouse_motion_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let mouse_motion_which =
    F (mouse_motion_event, Mouse_motion_event.which,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))
  let mouse_motion_state =
    F (mouse_motion_event, Mouse_motion_event.state,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))
  let mouse_motion_x =
    F (mouse_motion_event, Mouse_motion_event.x,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let mouse_motion_y =
    F (mouse_motion_event, Mouse_motion_event.y,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let mouse_motion_xrel =
    F (mouse_motion_event, Mouse_motion_event.xrel,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let mouse_motion_yrel =
    F (mouse_motion_event, Mouse_motion_event.yrel,
       Int32.to_int, (fun _ x -> Int32.of_int x))

  let mouse_wheel_window_id =
    F (mouse_wheel_event, Mouse_wheel_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let mouse_wheel_which =
    F (mouse_wheel_event, Mouse_wheel_event.which,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))
  let mouse_wheel_x =
    F (mouse_wheel_event, Mouse_wheel_event.x,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let mouse_wheel_y =
    F (mouse_wheel_event, Mouse_wheel_event.y,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let mouse_wheel_direction =
    F(mouse_wheel_event, Mouse_wheel_event.direction,
      Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))

  (* Multi gesture events *)

  let multi_gesture_touch_id =
    F (multi_gesture_event, Multi_gesture_event.touch_id, Fun.id, (fun _ x -> x))
  let multi_gesture_dtheta =
    F (multi_gesture_event, Multi_gesture_event.dtheta, Fun.id, (fun _ x -> x))
  let multi_gesture_ddist =
    F (multi_gesture_event, Multi_gesture_event.ddist, Fun.id, (fun _ x -> x))
  let multi_gesture_x =
    F (multi_gesture_event, Multi_gesture_event.x, Fun.id, (fun _ x -> x))
  let multi_gesture_y =
    F (multi_gesture_event, Multi_gesture_event.y, Fun.id, (fun _ x -> x))
  let multi_gesture_num_fingers =
    F (multi_gesture_event, Multi_gesture_event.num_fingers,
       Unsigned.UInt16.to_int, (fun _ x -> Unsigned.UInt16.of_int x))

  let text_editing_window_id =
    F (text_editing_event, Text_editing_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let text_editing_text =
    F (text_editing_event, Text_editing_event.text,
       (fun p -> string_from_ptr (CArray.start p) ~length:texteditingevent_text_size),
       (fun _ x -> CArray.of_string x))
  let text_editing_start =
    F (text_editing_event, Text_editing_event.start,
       Int32.to_int, (fun _ x -> Int32.of_int x))
  let text_editing_length =
    F (text_editing_event, Text_editing_event.length,
       Int32.to_int, (fun _ x -> Int32.of_int x))

  let text_input_window_id =
    F (text_input_event, Text_input_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let text_input_text =
    F (text_input_event, Text_input_event.text,
       (fun p -> string_from_ptr (CArray.start p) ~length:textinputevent_text_size),
       (fun _ x -> CArray.of_string x))

  (* User events *)

  let user_window_id =
    F (_user_event, User_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let user_code =
    F (_user_event, User_event.code, Int32.to_int, (fun _ x -> Int32.of_int x))

  let window_window_id =
    F (_window_event, Window_event.window_id,
       Unsigned.UInt32.to_int, (fun _ x -> Unsigned.UInt32.of_int x))
  let window_event_id =
    F (_window_event, Window_event.event,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))
  let window_data1 =
    F (_window_event, Window_event.data1, Fun.id, (fun _ x -> x))
  let window_data2 =
    F (_window_event, Window_event.data2, Fun.id, (fun _ x -> x))

  (* Window event id enum *)

  type window_event_enum =
    [ `Close | `Enter | `Exposed | `Focus_gained | `Focus_lost | `Hidden
    | `Hit_test | `Leave | `Maximized | `Minimized | `Moved | `Resized
    | `Restored | `Shown | `Size_changed | `Take_focus
    | `Unknown of window_event_id ]

  let enum_of_window_event_id =
    (*let add acc (k, v) = Imap.add k v acc in*)
      let enums = [
      window_event_shown, `Shown;
      window_event_hidden, `Hidden;
      window_event_exposed, `Exposed;
      window_event_moved, `Moved;
      window_event_resized, `Resized;
      window_event_size_changed, `Size_changed;
      window_event_minimized, `Minimized;
      window_event_maximized, `Maximized;
      window_event_restored, `Restored;
      window_event_enter, `Enter;
      window_event_leave, `Leave;
      window_event_focus_gained, `Focus_gained;
      window_event_focus_lost, `Focus_lost;
      window_event_close, `Close;
      window_event_take_focus, `Take_focus;
      window_event_hit_test, `Hit_test; ]
    in
    (*List.fold_left add Imap.empty*) enums

  let window_event_enum id =
    try List.assoc id enum_of_window_event_id with Not_found -> `Unknown id

  (* Display event *)

  let display_display =
    F (_display_event, Display_event.display,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))

  let display_event_id =
    F (_display_event, Display_event.event,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))

  let display_data1 =
    F (_display_event, Display_event.data1, Fun.id, (fun _ x -> x))

  (* Sensor event *)

  let sensor_which =
    F (sensor_event, Sensor_event.which,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))

  let sensor_data0 =
    F (sensor_event, Sensor_event.data,
       (fun a -> CArray.get a 0),
       (fun a x -> let () = CArray.set a 0 x in a))

  let sensor_data1 =
    F (sensor_event, Sensor_event.data,
       (fun a -> CArray.get a 1),
       (fun a x -> let () = CArray.set a 1 x in a))

  let sensor_data2 =
    F (sensor_event, Sensor_event.data,
       (fun a -> CArray.get a 2),
       (fun a x -> let () = CArray.set a 2 x in a))

  let sensor_data3 =
    F (sensor_event, Sensor_event.data,
       (fun a -> CArray.get a 3),
       (fun a x -> let () = CArray.set a 3 x in a))

  let sensor_data4 =
    F (sensor_event, Sensor_event.data,
       (fun a -> CArray.get a 4),
       (fun a x -> let () = CArray.set a 4 x in a))

  let sensor_data5 =
    F (sensor_event, Sensor_event.data,
       (fun a -> CArray.get a 5),
       (fun a x -> let () = CArray.set a 5 x in a))

  (* Audio device event *)

  let audio_device_timestamp =
    F (audio_device_event, Audio_device_event.timestamp,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))

  let audio_device_which =
    F (audio_device_event, Audio_device_event.which,
       Unsigned.UInt32.to_int32, (fun _ x -> Unsigned.UInt32.of_int32 x))

  let audio_device_is_capture =
    F (audio_device_event, Audio_device_event.iscapture,
       Unsigned.UInt8.to_int, (fun _ x -> Unsigned.UInt8.of_int x))

  (* Event type enum *)

  type enum =
  [ `App_did_enter_background | `App_did_enter_foreground
  | `App_low_memory | `App_terminating | `App_will_enter_background
  | `App_will_enter_foreground
  | `Audio_device_added | `Audio_device_removed
  | `Clipboard_update | `Controller_axis_motion | `Controller_button_down
  | `Controller_button_up | `Controller_device_added
  | `Controller_device_remapped | `Controller_device_removed
  | `Dollar_gesture | `Dollar_record
  | `Drop_begin | `Drop_complete | `Drop_file | `Drop_text
  | `Finger_down | `Finger_motion | `Finger_up
  | `Keymap_changed
  | `Joy_axis_motion | `Joy_ball_motion
  | `Joy_button_down | `Joy_button_up | `Joy_device_added
  | `Joy_device_removed | `Joy_hat_motion | `Key_down | `Key_up
  | `Mouse_button_down | `Mouse_button_up | `Mouse_motion
  | `Mouse_wheel | `Multi_gesture | `Quit
  | `Render_targets_reset | `Render_device_reset
  | `Sys_wm_event
  | `Text_editing | `Text_input | `Unknown of int | `User_event
  | `Window_event | `Display_event | `Sensor_update ]

  let enum_of_event_type =
    let add acc (k, v) = Imap.add k v acc in
    let enums = [ app_terminating, `App_terminating;
                  app_low_memory, `App_low_memory;
                  app_will_enter_background, `App_will_enter_background;
                  app_did_enter_background, `App_did_enter_background;
                  app_will_enter_foreground, `App_will_enter_foreground;
                  app_did_enter_foreground, `App_did_enter_foreground;
                  audio_device_added, `Audio_device_added;
                  audio_device_removed, `Audio_device_removed;
                  clipboard_update, `Clipboard_update;
                  controller_axis_motion, `Controller_axis_motion;
                  controller_button_down, `Controller_button_down;
                  controller_button_up, `Controller_button_up;
                  controller_device_added, `Controller_device_added;
                  controller_device_remapped, `Controller_device_remapped;
                  controller_device_removed, `Controller_device_removed;
                  dollar_gesture, `Dollar_gesture;
                  dollar_record, `Dollar_record;
                  drop_begin, `Drop_begin;
                  drop_complete, `Drop_complete;
                  drop_file, `Drop_file;
                  drop_text, `Drop_text;
                  finger_down, `Finger_down;
                  finger_motion, `Finger_motion;
                  finger_up, `Finger_up;
                  keymap_changed, `Keymap_changed;
                  joy_axis_motion, `Joy_axis_motion;
                  joy_ball_motion, `Joy_ball_motion;
                  joy_button_down, `Joy_button_down;
                  joy_button_up, `Joy_button_up;
                  joy_device_added, `Joy_device_added;
                  joy_device_removed, `Joy_device_removed;
                  joy_hat_motion, `Joy_hat_motion;
                  key_down, `Key_down;
                  key_up, `Key_up;
                  mouse_button_down, `Mouse_button_down;
                  mouse_button_up, `Mouse_button_up;
                  mouse_motion, `Mouse_motion;
                  mouse_wheel, `Mouse_wheel;
                  multi_gesture, `Multi_gesture;
                  render_targets_reset, `Render_targets_reset;
                  render_device_reset, `Render_device_reset;
                  sys_wm_event, `Sys_wm_event;
                  text_editing, `Text_editing;
                  text_input, `Text_input;
                  C.Types.Event.user_event, `User_event;
                  quit, `Quit;
                  C.Types.Event.window_event, `Window_event;
                  C.Types.Event.display_event, `Display_event;
                  sensor_update, `Sensor_update; ]
    in
    List.fold_left add Imap.empty enums

  let enum t = try Imap.find t enum_of_event_type with Not_found -> `Unknown t

end

type event = Event.t union

let get_event_state e =
  C.Functions.event_state (Unsigned.UInt32.of_int e) C.Types.sdl_query

let set_event_state e s =
  ignore (C.Functions.event_state
            (Unsigned.UInt32.of_int e) (Unsigned.UInt8.to_int s))

let flush_event e = C.Functions.flush_event (Unsigned.UInt32.of_int e)

let flush_events f t =
  C.Functions.flush_events (Unsigned.UInt32.of_int f) (Unsigned.UInt32.of_int t)

let has_event e = C.Functions.has_event (Unsigned.UInt32.of_int e)

let has_events f t =
  C.Functions.has_events (Unsigned.UInt32.of_int f) (Unsigned.UInt32.of_int t)

let poll_event e =
  C.Functions.poll_event (Event.opt_addr e)

let pump_events = C.Functions.pump_events

let push_event e =
  bool_to_ok (C.Functions.push_event (addr e))

let register_event () =
  let out = C.Functions.register_events 1 in
  if Unsigned.UInt32.(equal out max_int) then None else Some (Unsigned.UInt32.to_int out)

let wait_event e = match C.Async_functions.wait_event (Event.opt_addr e) with
| 1 -> Ok () | _ -> error ()

let wait_event_timeout e t =
  C.Async_functions.wait_event_timeout (Event.opt_addr e) t

(* Force feedback *)

module Haptic = struct
  include C.Types.Haptic

  module Direction = struct
    include Direction

    let create typv d0 d1 d2 =
      let d = make t in
      setf d typ (Unsigned.UInt8.of_int typv);
      let dir = getf d dir in
      CArray.set dir 0 d0;
      CArray.set dir 1 d1;
      CArray.set dir 2 d2;
      d

    let typ d = Unsigned.UInt8.to_int (getf d typ)
    let dir_0 d = CArray.get (getf d dir) 0
    let dir_1 d = CArray.get (getf d dir) 1
    let dir_2 d = CArray.get (getf d dir) 2
  end

  type effect_type = int

  let create_effect () = make Effect.t

  type _ field =
      F : (* existential to hide the 'a structure *)
        (('a structure, Effect.t union) Ctypes.field *
         ('b, 'a structure) Ctypes.field *
        ('b -> 'c) * ('c -> 'b)) -> 'c field

  let get e (F (s, f, c, _)) = c (getf (getf e s) f)
  let set e (F (s, f, _, c)) v = setf (getf e s) f (c v)

  let typ = F (Effect.constant, Constant.typ,
               Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  (* same in each enum *)

  (* Constant *)

  let constant_type =
    F (Effect.constant, Constant.typ,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_direction =
    F (Effect.constant, Constant.direction, Fun.id, Fun.id)
  let constant_length =
    F (Effect.constant, Constant.length,
       Unsigned.UInt32.to_int32, Unsigned.UInt32.of_int32)
  let constant_delay =
    F (Effect.constant, Constant.delay,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_button =
    F (Effect.constant, Constant.button,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_interval =
    F (Effect.constant, Constant.interval,
      Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_level =
    F (Effect.constant, Constant.level, Fun.id, Fun.id)
  let constant_attack_length =
    F (Effect.constant, Constant.attack_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_attack_level =
    F (Effect.constant, Constant.attack_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_fade_length =
    F (Effect.constant, Constant.fade_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let constant_fade_level =
    F (Effect.constant, Constant.fade_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)

  (* Periodic *)

  let periodic_type =
    F (Effect.periodic, Periodic.typ,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_direction =
    F (Effect.periodic, Periodic.direction, Fun.id, Fun.id)
  let periodic_length =
    F (Effect.periodic, Periodic.length,
       Unsigned.UInt32.to_int32, Unsigned.UInt32.of_int32)
  let periodic_delay =
    F (Effect.periodic, Periodic.delay,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_button =
    F (Effect.periodic, Periodic.button,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_interval =
    F (Effect.periodic, Periodic.interval,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_period =
    F (Effect.periodic, Periodic.period,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_magnitude =
    F (Effect.periodic, Periodic.magnitude, Fun.id, Fun.id)
  let periodic_offset =
    F (Effect.periodic, Periodic.offset, Fun.id, Fun.id)
  let periodic_phase =
    F (Effect.periodic, Periodic.phase,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_attack_length =
    F (Effect.periodic, Periodic.attack_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_attack_level =
    F (Effect.periodic, Periodic.attack_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_fade_length =
    F (Effect.periodic, Periodic.fade_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let periodic_fade_level =
    F (Effect.periodic, Periodic.fade_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)

  (* Condition *)

  let condition_type =
    F (Effect.condition, Condition.typ,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_direction =
    F (Effect.condition, Condition.direction, Fun.id, Fun.id)
  let condition_length =
    F (Effect.condition, Condition.length,
       Unsigned.UInt32.to_int32, Unsigned.UInt32.of_int32)
  let condition_delay =
    F (Effect.condition, Condition.delay,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_button =
    F (Effect.condition, Condition.button,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_interval =
    F (Effect.condition, Condition.interval,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_right_sat_0 =
    F (Effect.condition, Condition.right_sat_0,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_right_sat_1 =
    F (Effect.condition, Condition.right_sat_1,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_right_sat_2 =
    F (Effect.condition, Condition.right_sat_2,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_left_sat_0 =
    F (Effect.condition, Condition.left_sat_0,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_left_sat_1 =
    F (Effect.condition, Condition.left_sat_1,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_left_sat_2 =
    F (Effect.condition, Condition.left_sat_2,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_right_coeff_0 =
    F (Effect.condition, Condition.right_coeff_0, Fun.id, Fun.id)
  let condition_right_coeff_1 =
    F (Effect.condition, Condition.right_coeff_1, Fun.id, Fun.id)
  let condition_right_coeff_2 =
    F (Effect.condition, Condition.right_coeff_2, Fun.id, Fun.id)
  let condition_left_coeff_0 =
    F (Effect.condition, Condition.left_coeff_0, Fun.id, Fun.id)
  let condition_left_coeff_1 =
    F (Effect.condition, Condition.left_coeff_1, Fun.id, Fun.id)
  let condition_left_coeff_2 =
    F (Effect.condition, Condition.left_coeff_2, Fun.id, Fun.id)
  let condition_deadband_0 =
    F (Effect.condition, Condition.deadband_0,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_deadband_1 =
    F (Effect.condition, Condition.deadband_1,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_deadband_2 =
    F (Effect.condition, Condition.deadband_2,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let condition_center_0 =
    F (Effect.condition, Condition.center_0, Fun.id, Fun.id)
  let condition_center_1 =
    F (Effect.condition, Condition.center_1, Fun.id, Fun.id)
  let condition_center_2 =
    F (Effect.condition, Condition.center_2, Fun.id, Fun.id)

  (* Ramp *)

  let ramp_type =
    F (Effect.ramp, Ramp.typ,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_direction =
    F (Effect.ramp, Ramp.direction, Fun.id, Fun.id)
  let ramp_length =
    F (Effect.ramp, Ramp.length,
       Unsigned.UInt32.to_int32, Unsigned.UInt32.of_int32)
  let ramp_delay =
    F (Effect.ramp, Ramp.delay,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_button =
    F (Effect.ramp, Ramp.button,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_interval =
    F (Effect.ramp, Ramp.interval,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_start =
    F (Effect.ramp, Ramp.start, Fun.id, Fun.id)
  let ramp_end =
    F (Effect.ramp, Ramp.end_, Fun.id, Fun.id)
  let ramp_attack_length =
    F (Effect.ramp, Ramp.attack_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_attack_level =
    F (Effect.ramp, Ramp.attack_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_fade_length =
    F (Effect.ramp, Ramp.fade_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let ramp_fade_level =
    F (Effect.ramp, Ramp.fade_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)

  (* Left right *)

  let left_right_type =
    F (Effect.left_right, Left_right.typ,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let left_right_length =
    F (Effect.left_right, Left_right.length,
       Unsigned.UInt32.to_int32, Unsigned.UInt32.of_int32)
  let left_right_large_magnitude =
    F (Effect.left_right, Left_right.large_magnitude,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let left_right_small_magnitude =
    F (Effect.left_right, Left_right.small_magnitude,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)

  (* Custom *)

  let custom_type =
    F (Effect.custom, Custom.typ,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_direction =
    F (Effect.custom, Custom.direction, Fun.id, Fun.id)
  let custom_length =
    F (Effect.custom, Custom.length,
       Unsigned.UInt32.to_int32, Unsigned.UInt32.of_int32)
  let custom_delay =
    F (Effect.custom, Custom.delay,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_button =
    F (Effect.custom, Custom.button,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_interval =
    F (Effect.custom, Custom.interval,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_channels =
    F (Effect.custom, Custom.channels,
       Unsigned.UInt8.to_int, Unsigned.UInt8.of_int)
  let custom_period =
    F (Effect.custom, Custom.period,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_samples =
    F (Effect.custom, Custom.samples,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_data =
    F (Effect.custom, Custom.data,
       (fun p -> invalid_arg err_read_field),
       (fun l ->
          let l = List.map Unsigned.UInt16.of_int l in
          let a = Ctypes.CArray.of_list Ctypes.uint16_t l in
          Ctypes.CArray.start a))
  let custom_attack_length =
    F (Effect.custom, Custom.attack_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_attack_level =
    F (Effect.custom, Custom.attack_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_fade_length =
    F (Effect.custom, Custom.fade_length,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
  let custom_fade_level =
    F (Effect.custom, Custom.fade_level,
       Unsigned.UInt16.to_int, Unsigned.UInt16.of_int)
end

type haptic = Haptic.t ptr

type haptic_effect = Haptic.Effect.t union

type haptic_effect_id = int

let haptic_close = C.Functions.haptic_close

let haptic_destroy_effect = C.Functions.haptic_destroy_effect

let haptic_effect_supported h e =
  bool_to_ok (C.Functions.haptic_effect_supported h (addr e))

let haptic_get_effect_status h i =
  bool_to_ok (C.Functions.haptic_get_effect_status h i)

let haptic_index h = nat_to_ok (C.Functions.haptic_index h)

let haptic_name i = some_to_ok (C.Functions.haptic_name i)

let haptic_new_effect h e =
  nat_to_ok (C.Functions.haptic_new_effect h (addr e))

let haptic_num_axes h =
  nat_to_ok (C.Functions.haptic_num_axes h)

let haptic_num_effects h =
  nat_to_ok (C.Functions.haptic_num_effects h)

let haptic_num_effects_playing h =
  nat_to_ok (C.Functions.haptic_num_effects_playing h)

let haptic_open i = some_to_ok (C.Functions.haptic_open i)

let haptic_open_from_joystick j =
  some_to_ok (C.Functions.haptic_open_from_joystick j)

let haptic_open_from_mouse () =
  some_to_ok (C.Functions.haptic_open_from_mouse ())

let haptic_opened i = match C.Functions.haptic_opened i with
| 0 -> false | 1 -> true | _ -> assert false

let haptic_pause h = zero_to_ok (C.Functions.haptic_pause h)

let haptic_query = C.Functions.haptic_query

let haptic_rumble_init h =
  zero_to_ok (C.Functions.haptic_rumble_init h)

let haptic_rumble_play h x y =
  zero_to_ok (C.Functions.haptic_rumble_play h x y)

let haptic_rumble_stop h =
  zero_to_ok (C.Functions.haptic_rumble_stop h)

let haptic_rumble_supported h =
  bool_to_ok (C.Functions.haptic_rumble_supported h)

let haptic_run_effect h i n =
  zero_to_ok (C.Functions.haptic_run_effect h i n)

let haptic_set_autocenter h n =
  zero_to_ok (C.Functions.haptic_set_autocenter h n)

let haptic_set_gain h n = zero_to_ok (C.Functions.haptic_set_gain h n)

let haptic_stop_all h = zero_to_ok (C.Functions.haptic_stop_all h)

let haptic_stop_effect h i = zero_to_ok (C.Functions.haptic_stop_effect h i)

let haptic_unpause h = zero_to_ok (C.Functions.haptic_unpause h)

let haptic_update_effect h id e =
  zero_to_ok (C.Functions.haptic_update_effect h id (addr e))

let joystick_is_haptic j = bool_to_ok (C.Functions.joystick_is_haptic j)

let mouse_is_haptic () = bool_to_ok (C.Functions.mouse_is_haptic ())

let num_haptics () = nat_to_ok (C.Functions.num_haptics ())

(* Audio *)

(* Audio drivers *)

let audio_init s = zero_to_ok (C.Functions.audio_init s)

let audio_quit = C.Functions.audio_quit

let get_audio_driver i = some_to_ok (C.Functions.get_audio_driver i)

let get_current_audio_driver = C.Functions.get_current_audio_driver

let get_num_audio_drivers () =
  nat_to_ok (C.Functions.get_num_audio_drivers ())

(* Audio devices *)

module Audio = C.Types.Audio

type audio_device_id = uint32

type audio_callback =
  unit Ctypes_static.ptr -> Unsigned.uint8 Ctypes_static.ptr -> int -> unit

type audio_spec =
  { as_freq : int;
    as_format : Audio.format;
    as_channels : uint8;
    as_silence : uint8;
    as_samples : uint16;
    as_size : uint32;
    as_callback : audio_callback option; }

let audio_callback kind f =
  let kind_bytes = ba_kind_byte_size kind in
  let ba_ptr_typ = access_ptr_typ_of_ba_kind kind in
  fun _ p len ->
    let p = coerce (ptr uint8_t) ba_ptr_typ p in
    let len = len / kind_bytes in
    f (bigarray_of_ptr array1 len kind p)

let as_callback =
  Foreign.funptr_opt ~thread_registration:true ~runtime_lock:true C.Types.as_callback_type

let audio_spec_of_c c =
  let as_freq = getf c C.Types.as_freq in
  let as_format = Unsigned.UInt16.to_int (getf c C.Types.as_format) in
  let as_channels = Unsigned.UInt8.to_int (getf c C.Types.as_channels) in
  let as_silence = Unsigned.UInt8.to_int (getf c C.Types.as_silence) in
  let as_samples = Unsigned.UInt16.to_int (getf c C.Types.as_samples) in
  let as_size = Unsigned.UInt32.to_int32 (getf c C.Types.as_size) in
  let as_callback = None in
  { as_freq; as_format; as_channels; as_silence; as_samples; as_size;
    as_callback; }

let audio_spec_to_c a =
  let c = make C.Types.audio_spec in
  setf c C.Types.as_freq a.as_freq;
  setf c C.Types.as_format (Unsigned.UInt16.of_int a.as_format);
  setf c C.Types.as_channels (Unsigned.UInt8.of_int a.as_channels);
  setf c C.Types.as_silence (Unsigned.UInt8.of_int a.as_silence); (* irrelevant *)
  setf c C.Types.as_samples (Unsigned.UInt16.of_int a.as_samples);
  setf c C.Types.as_size (Unsigned.UInt32.of_int32 a.as_size); (* irrelevant *)
  setf c C.Types.as_callback
    (coerce as_callback (static_funptr C.Types.as_callback_type) a.as_callback);
  setf c C.Types.as_userdata null;
  c

let close_audio_device d =
  C.Functions.close_audio_device (Unsigned.UInt32.of_int32 d)

let free_wav ba =
  C.Functions.free_wav (to_voidp (bigarray_start array1 ba))

let get_audio_device_name i b =
  some_to_ok (C.Functions.get_audio_device_name i b)

let get_audio_device_status d =
  C.Functions.get_audio_device_status (Unsigned.UInt32.of_int32 d)

let get_num_audio_devices b = nat_to_ok (C.Functions.get_num_audio_devices b)

let load_wav_rw ops spec kind =
  let d = allocate (ptr uint8_t) (from_voidp uint8_t null) in
  let len = allocate uint32_t Unsigned.UInt32.zero in
  match C.Async_functions.load_wav_rw
          ops 0 (addr (audio_spec_to_c spec)) d len with
  | None -> error ()
  | Some r ->
      let rspec = audio_spec_of_c (!@ r) in
      let kind_size = ba_kind_byte_size kind in
      let len = Unsigned.UInt32.to_int (!@ len) in
      if len mod kind_size <> 0
      then invalid_arg (err_bigarray_data len kind_size)
      else
      let ba_size = len / kind_size in
      let ba_ptr = access_ptr_typ_of_ba_kind kind in
      let d = coerce (ptr uint8_t)  ba_ptr (!@ d) in
      Ok (rspec, bigarray_of_ptr array1 ba_size kind d)

let lock_audio_device d =
  C.Functions.lock_audio_device (Unsigned.UInt32.of_int32 d)

let open_audio_device dev capture desired allow =
  let desiredc = audio_spec_to_c desired in
  let obtained = make C.Types.audio_spec in
  match C.Functions.open_audio_device
          dev capture (addr desiredc) (addr obtained) allow
  with
  | id when Unsigned.UInt32.(equal id zero) -> error ()
  | id -> Ok (Unsigned.UInt32.to_int32 id,  audio_spec_of_c obtained)

let pause_audio_device d =
  C.Functions.pause_audio_device (Unsigned.UInt32.of_int32 d)

let unlock_audio_device d =
  C.Functions.unlock_audio_device (Unsigned.UInt32.of_int32 d)

let queue_audio dev ba =
  let len = Bigarray.Array1.dim ba in
  let kind_size = ba_kind_byte_size (Bigarray.Array1.kind ba) in
  zero_to_ok (C.Functions.queue_audio
                (Unsigned.UInt32.of_int32 dev)
                (to_voidp (bigarray_start array1 ba))
                (Unsigned.UInt32.of_int (len * kind_size)))

let dequeue_audio dev ba =
  let len = Bigarray.Array1.dim ba in
  let kind_size = ba_kind_byte_size (Bigarray.Array1.kind ba) in
  Unsigned.UInt32.to_int
    (C.Functions.dequeue_audio
        (Unsigned.UInt32.of_int32 dev)
       (to_voidp (bigarray_start array1 ba)) (len * kind_size))

let get_queued_audio_size d =
  Unsigned.UInt32.to_int
    (C.Functions.get_queued_audio_size (Unsigned.UInt32.of_int32 d))

let clear_queued_audio d =
  C.Functions.clear_queued_audio (Unsigned.UInt32.of_int32 d)

(* Timer *)

let delay = C.Async_functions.delay

let get_ticks = C.Functions.get_ticks

let get_ticks64 = C.Functions.get_ticks64

let get_performance_counter = C.Functions.get_performance_counter

let get_performance_frequency = C.Functions.get_performance_frequency

(* Platform and CPU information *)

let get_platform = C.Functions.get_platform

let get_cpu_cache_line_size () =
  nat_to_ok (C.Functions.get_cpu_cache_line_size ())

let get_cpu_count = C.Functions.get_cpu_count

let get_system_ram = C.Functions.get_system_ram

let has_3d_now = C.Functions.has_3d_now

let has_altivec = C.Functions.has_altivec

let has_avx = C.Functions.has_avx

let has_avx2 = C.Functions.has_avx2

let has_mmx = C.Functions.has_mmx

let has_neon = C.Functions.has_neon

let has_rdtsc = C.Functions.has_rdtsc

let has_sse = C.Functions.has_sse

let has_sse2 = C.Functions.has_sse2

let has_sse3 = C.Functions.has_sse3

let has_sse41 = C.Functions.has_sse41

let has_sse42 = C.Functions.has_sse42

(* Power management *)

type power_state =
  [ `Unknown | `On_battery | `No_battery | `Charging | `Charged ]

let power_state =
  C.Types.Powerstate.[ unknown, `Unknown;
                       on_battery, `On_battery;
                       no_battery, `No_battery;
                       charging, `Charging;
                       charged, `Charged; ]

type power_info =
  { pi_state : power_state;
    pi_secs : int option;
    pi_pct : int option; }

let get_power_info () =
  let secs = allocate int 0 in
  let pct = allocate int 0 in
  let s = C.Functions.get_power_info secs pct in
  let pi_state = try List.assoc s power_state with Not_found -> assert false in
  let pi_secs = match !@ secs with -1 -> None | secs -> Some secs in
  let pi_pct = match !@ pct with -1 -> None | pct -> Some pct in
  { pi_state; pi_secs; pi_pct }
end
