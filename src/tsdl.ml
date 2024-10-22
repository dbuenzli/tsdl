(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let unsafe_get = Array.unsafe_get

open Ctypes
open Foreign

module Sdl = struct

(* Enum cases and #ifdef'd constants, see support/ in the distribution *)

open Tsdl_consts

(* Formatting with continuation. *)

let kpp k fmt =
  let k fmt = k (Format.flush_str_formatter ()) in
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

let write_never _ = assert false

let bool =
  view ~read:((<>)0) ~write:(fun b -> compare b false) int;;

let int_as_uint8_t =
  view ~read:Unsigned.UInt8.to_int ~write:Unsigned.UInt8.of_int uint8_t

let int_as_uint16_t =
  view ~read:Unsigned.UInt16.to_int ~write:Unsigned.UInt16.of_int uint16_t

let int_as_uint32_t =
  view ~read:Unsigned.UInt32.to_int ~write:Unsigned.UInt32.of_int uint32_t

let int_as_int32_t =
  view ~read:Signed.Int32.to_int ~write:Signed.Int32.of_int int32_t

let int32_as_uint32_t =
  view ~read:Unsigned.UInt32.to_int32 ~write:Unsigned.UInt32.of_int32 uint32_t

let string_as_char_array n = (* FIXME: drop this if ctypes proposes better *)
  let n_array = array n char in
  let read a =
    let len = CArray.length a in
    let b = Buffer.create len in
    try
      for i = 0 to len - 1 do
        let c = CArray.get a i in
        if c = '\000' then raise Exit else Buffer.add_char b c
      done;
      Buffer.contents b
    with Exit -> Buffer.contents b
  in
  let write s =
    let a = CArray.make char n in
    let len = min (CArray.length a) (String.length s) in
    for i = 0 to len - 1 do CArray.set a i (s.[i]) done;
    a
  in
  view ~read ~write n_array

let get_error =
  foreign "SDL_GetError" (void @-> returning string)

(* SDL results *)

type nonrec 'a result = ( 'a, [ `Msg of string ] ) result

let error () = Error (`Msg (get_error ()))

let zero_to_ok =
  let read = function 0 -> Ok () | err -> error () in
  view ~read ~write:write_never int

let one_to_ok =
  let read = function 1 -> Ok () | err -> error () in
  view ~read ~write:write_never int

let bool_to_ok =
  let read = function 0 -> Ok false | 1 -> Ok true | _ -> error () in
  view ~read ~write:write_never int

let nat_to_ok =
  let read = function n when n < 0 -> error () | n -> Ok n in
  view ~read ~write:write_never int

let some_to_ok t =
  let read = function Some v -> Ok v | None -> error () in
  view ~read ~write:write_never t

let sdl_free = foreign "SDL_free" (ptr void @-> returning void)

(* Since we never let SDL redefine our main make sure this is always
   called. *)

let () =
  let set_main_ready = foreign "SDL_SetMainReady" (void @-> returning void) in
  set_main_ready ()

let stub = true


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
  | k -> assert false

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
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let ( - ) f f' = Unsigned.UInt32.(logand f (lognot f'))
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let nothing = i 0
  let timer = i sdl_init_timer
  let audio = i sdl_init_audio
  let video = i sdl_init_video
  let joystick = i sdl_init_joystick
  let haptic = i sdl_init_haptic
  let gamecontroller = i sdl_init_gamecontroller
  let events = i sdl_init_events
  let everything = i sdl_init_everything
  let noparachute = i sdl_init_noparachute
end

let init =
  foreign "SDL_Init" (uint32_t @-> returning zero_to_ok)

let init_sub_system =
  foreign "SDL_InitSubSystem" (uint32_t @-> returning zero_to_ok)

let quit =
  foreign "SDL_Quit" (void @-> returning void)

let quit_sub_system =
  foreign "SDL_QuitSubSystem" (uint32_t @-> returning void)

let was_init =
  foreign "SDL_WasInit" (uint32_t @-> returning uint32_t)

let was_init = function
| None -> was_init (Unsigned.UInt32.of_int 0)
| Some m -> was_init m

(* Hints *)

module Hint = struct
  type t = string
  let audio_resampling_mode = sdl_hint_audio_resampling_mode
  let framebuffer_acceleration = sdl_hint_framebuffer_acceleration
  let idle_timer_disabled = sdl_hint_idle_timer_disabled
  let orientations = sdl_hint_orientations
  let mouse_focus_clickthrough = sdl_hint_mouse_focus_clickthrough
  let mouse_normal_speed_scale = sdl_hint_mouse_normal_speed_scale
  let mouse_relative_speed_scale = sdl_hint_mouse_relative_speed_scale
  let render_driver = sdl_hint_render_driver
  let render_logical_size_mode = sdl_hint_render_logical_size_mode
  let render_opengl_shaders = sdl_hint_render_opengl_shaders
  let render_scale_quality = sdl_hint_render_scale_quality
  let render_vsync = sdl_hint_render_vsync
  let no_signal_handlers = sdl_hint_no_signal_handlers
  let thread_stack_size = sdl_hint_thread_stack_size
  let touch_mouse_events = sdl_hint_touch_mouse_events
  let mouse_touch_events = sdl_hint_mouse_touch_events
  let window_frame_usable_while_cursor_hidden =
    sdl_hint_window_frame_usable_while_cursor_hidden

  type priority = int
  let default = sdl_hint_default
  let normal = sdl_hint_normal
  let override = sdl_hint_override
end

let clear_hints =
  foreign "SDL_ClearHints" (void @-> returning void)

let get_hint =
  foreign "SDL_GetHint" (string @-> returning string_opt)

let get_hint_boolean =
  foreign "SDL_GetHintBoolean" (string @-> bool @-> returning bool)

let set_hint =
  foreign "SDL_SetHint" (string @-> string @-> returning bool)

let set_hint_with_priority =
  foreign "SDL_SetHintWithPriority"
    (string @-> string @-> int @-> returning bool)

(* Errors *)

let clear_error =
  foreign "SDL_ClearError" (void @-> returning void)

let set_error =
  foreign "SDL_SetError" (string @-> returning int)

let set_error fmt =
  kpp (fun s -> ignore (set_error s)) fmt

(* Log *)

module Log = struct
  type category = int
  let category_application = sdl_log_category_application
  let category_error = sdl_log_category_error
  let category_system = sdl_log_category_system
  let category_audio = sdl_log_category_audio
  let category_video = sdl_log_category_video
  let category_render = sdl_log_category_render
  let category_input = sdl_log_category_input
  let category_custom = sdl_log_category_custom

  type priority = int
  let priority_compare : int -> int -> int = compare
  let priority_verbose = sdl_log_priority_verbose
  let priority_debug = sdl_log_priority_debug
  let priority_info = sdl_log_priority_info
  let priority_warn = sdl_log_priority_warn
  let priority_error = sdl_log_priority_error
  let priority_critical = sdl_log_priority_critical
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

let log_get_priority =
  foreign "SDL_LogGetPriority" (int @-> returning int)

let log_reset_priorities =
  foreign "SDL_LogResetPriorities" (void @-> returning void)

let log_set_all_priority =
  foreign "SDL_LogSetAllPriority" (int @-> returning void)

let log_set_priority =
  foreign "SDL_LogSetPriority" (int @-> int @-> returning void)

(* Version *)

let version = structure "SDL_version"
let version_major = field version "major" uint8_t
let version_minor = field version "minor" uint8_t
let version_patch = field version "patch" uint8_t
let () = seal version

let get_version =
  foreign "SDL_GetVersion" (ptr version @-> returning void)

let get_version () =
  let get v f = Unsigned.UInt8.to_int (getf v f) in
  let v = make version in
  get_version (addr v);
  (get v version_major), (get v version_minor), (get v version_patch)

let get_revision =
  foreign "SDL_GetRevision" (void @-> returning string)

let get_revision_number =
  foreign "SDL_GetRevisionNumber" (void @-> returning int)

(* IO absraction *)

type _rw_ops
let rw_ops_struct : _rw_ops structure typ = structure "SDL_RWops"
let rw_ops : _rw_ops structure ptr typ = ptr rw_ops_struct
let rw_ops_opt : _rw_ops structure ptr option typ = ptr_opt rw_ops_struct

let rw_ops_size = field rw_ops_struct "size"
    (funptr (rw_ops @-> returning int64_t))
let rw_ops_seek = field rw_ops_struct "seek"
    (funptr (rw_ops @-> int64_t @-> int @-> returning int64_t))
let rw_ops_read = field rw_ops_struct "read"
    (funptr (rw_ops @-> ptr void @-> size_t @-> size_t @-> returning size_t))
let rw_ops_write = field rw_ops_struct "write"
    (funptr (rw_ops @-> ptr void @-> size_t @-> size_t @-> returning size_t))
let rw_ops_close = field rw_ops_struct "close"
    (funptr (rw_ops @-> returning int))
let _ = field rw_ops_struct "type" uint32_t
(* ... #ifdef'd union follows, we don't care we don't use Ctypes.make *)
let () = seal rw_ops_struct

type rw_ops = _rw_ops structure ptr

let load_file_rw =
  foreign "SDL_LoadFile_RW"
    (rw_ops @-> ptr int @-> bool @-> returning (some_to_ok string_opt))

let load_file_rw rw_ops close =
  load_file_rw rw_ops (coerce (ptr void) (ptr int) null) close

let rw_from_file =
  foreign "SDL_RWFromFile"
    (string @-> string @-> returning (some_to_ok rw_ops_opt))

let rw_from_const_mem =
  foreign "SDL_RWFromConstMem"
    (ocaml_string @-> int @-> returning (some_to_ok rw_ops_opt))

let rw_from_const_mem str = rw_from_const_mem
  (ocaml_string_start str) (String.length str)

let rw_from_mem =
  foreign "SDL_RWFromMem"
    (ocaml_bytes @-> int @-> returning (some_to_ok rw_ops_opt))

let rw_from_mem b = rw_from_mem (ocaml_bytes_start b) (Bytes.length b)

let load_file filename = (* defined as a macro in SDL_rwops.h *)
  match rw_from_file filename "rb" with
  | Error _ as e -> e
  | Ok rw -> load_file_rw rw true

let rw_close =
  foreign "SDL_RWclose" (rw_ops @-> returning int)

let rw_close ops =
  if rw_close ops = 0 then Ok () else (error ())

let unsafe_rw_ops_of_ptr addr : rw_ops =
  from_voidp rw_ops_struct (ptr_of_raw_address addr)
let unsafe_ptr_of_rw_ops rw_ops =
  raw_address_of_ptr (to_voidp rw_ops)

(* File system paths *)

let get_base_path =
  foreign "SDL_GetBasePath" (void @-> returning (ptr char))

let get_base_path () =
  let p = get_base_path () in
  let path = coerce (ptr char) (some_to_ok string_opt) p in
  sdl_free (coerce (ptr char) (ptr void) p);
  path

let get_pref_path =
  foreign "SDL_GetPrefPath" (string @-> string @-> returning (ptr char))

let get_pref_path ~org ~app =
  let p = get_pref_path org app in
  let path = coerce (ptr char) (some_to_ok string_opt) p in
  sdl_free (coerce (ptr char) (ptr void) p);
  path

(* Video *)

type window = unit ptr
let window : window typ = ptr void
let window_opt : window option typ = ptr_opt void

let unsafe_window_of_ptr addr : window =
  ptr_of_raw_address addr
let unsafe_ptr_of_window window =
  raw_address_of_ptr (to_voidp window)

(* Colors *)

type _color
type color = _color structure
let color : color typ = structure "SDL_Color"
let color_r = field color "r" uint8_t
let color_g = field color "g" uint8_t
let color_b = field color "b" uint8_t
let color_a = field color "a" uint8_t
let () = seal color

module Color = struct
  let create ~r ~g ~b ~a =
    let c = make color in
    setf c color_r (Unsigned.UInt8.of_int r);
    setf c color_g (Unsigned.UInt8.of_int g);
    setf c color_b (Unsigned.UInt8.of_int b);
    setf c color_a (Unsigned.UInt8.of_int a);
    c

  let r c = Unsigned.UInt8.to_int (getf c color_r)
  let g c = Unsigned.UInt8.to_int (getf c color_g)
  let b c = Unsigned.UInt8.to_int (getf c color_b)
  let a c = Unsigned.UInt8.to_int (getf c color_a)

  let set_r c r = setf c color_r (Unsigned.UInt8.of_int r)
  let set_g c g = setf c color_g (Unsigned.UInt8.of_int g)
  let set_b c b = setf c color_b (Unsigned.UInt8.of_int b)
  let set_a c a = setf c color_a (Unsigned.UInt8.of_int a)
end

(* Points *)

type _point
type point = _point structure
let point : point typ = structure "SDL_Point"
let point_x = field point "x" int
let point_y = field point "y" int
let () = seal point

module Point = struct
  let create ~x ~y =
    let p = make point in
    setf p point_x x;
    setf p point_y y;
    p

  let x p = getf p point_x
  let y p = getf p point_y

  let set_x p x = setf p point_x x
  let set_y p y = setf p point_y y

  let opt_addr = function
  | None -> coerce (ptr void) (ptr point) null
  | Some v -> addr v
end

(* Float Points *)

type _fpoint
type fpoint = _fpoint structure
let fpoint : fpoint typ = structure "SDL_FPoint"
let fpoint_x = field fpoint "x" float
let fpoint_y = field fpoint "y" float
let () = seal fpoint

module Fpoint = struct
  let create ~x ~y =
    let p = make fpoint in
    setf p fpoint_x x;
    setf p fpoint_y y;
    p

  let x p = getf p fpoint_x
  let y p = getf p fpoint_y

  let set_x p x = setf p fpoint_x x
  let set_y p y = setf p fpoint_y y
end

(* Vertices *)

type _vertex
type vertex = _vertex structure
let vertex : vertex typ = structure "SDL_Vertex"
let vertex_position = field vertex "position" fpoint
let vertex_color = field vertex "color" color
let vertex_tex_coord = field vertex "tex_coord" fpoint
let () = seal vertex

module Vertex = struct
  let create ~position ~color ~tex_coord =
    let v = make vertex in
    setf v vertex_position position;
    setf v vertex_color color;
    setf v vertex_tex_coord tex_coord;
    v

  let position v = getf v vertex_position
  let color v = getf v vertex_color
  let tex_coord v = getf v vertex_tex_coord

  let set_position v position = setf v vertex_position position
  let set_color v color = setf v vertex_color color
  let set_tex_coord v tex_coord = setf v vertex_tex_coord tex_coord

  let opt_addr = function
  | None -> coerce (ptr void) (ptr vertex) null
  | Some v -> addr v
end

(* Rectangle *)

type _rect
type rect = _rect structure
let rect : rect typ = structure "SDL_Rect"
let rect_x = field rect "x" int
let rect_y = field rect "y" int
let rect_w = field rect "w" int
let rect_h = field rect "h" int
let () = seal rect

module Rect = struct
  let create ~x ~y ~w ~h =
    let r = make rect in
    setf r rect_x x;
    setf r rect_y y;
    setf r rect_w w;
    setf r rect_h h;
    r

  let x r = getf r rect_x
  let y r = getf r rect_y
  let w r = getf r rect_w
  let h r = getf r rect_h

  let set_x r x = setf r rect_x x
  let set_y r y = setf r rect_y y
  let set_w r w = setf r rect_w w
  let set_h r h = setf r rect_h h

  let opt_addr = function
  | None -> coerce (ptr void) (ptr rect) null
  | Some v -> addr v
end

(* Float Rectangle *)

type _frect
type frect = _frect structure
let frect : frect typ = structure "SDL_FRect"
let frect_x = field frect "x" float
let frect_y = field frect "y" float
let frect_w = field frect "w" float
let frect_h = field frect "h" float
let () = seal frect

module Frect = struct
  let create ~x ~y ~w ~h =
    let r = make frect in
    setf r frect_x x;
    setf r frect_y y;
    setf r frect_w w;
    setf r frect_h h;
    r

  let x r = getf r frect_x
  let y r = getf r frect_y
  let w r = getf r frect_w
  let h r = getf r frect_h

  let set_x r x = setf r frect_x x
  let set_y r y = setf r frect_y y
  let set_w r w = setf r frect_w w
  let set_h r h = setf r frect_h h
end

let enclose_points =
  foreign "SDL_EnclosePoints"
    (ptr void @-> int @-> ptr rect @-> ptr rect @-> returning bool)

let enclose_points_ba ?clip ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  let res = make rect in
  if enclose_points ps count (Rect.opt_addr clip) (addr res)
  then Some res
  else None

let enclose_points ?clip ps =
  let a = CArray.of_list point ps in
  let ps = to_voidp (CArray.start a) in
  let res = make rect in
  if enclose_points ps (CArray.length a) (Rect.opt_addr clip) (addr res)
  then Some res
  else None

let has_intersection =
  foreign "SDL_HasIntersection"
    (ptr rect @-> ptr rect @-> returning bool)

let has_intersection a b =
  has_intersection (addr a) (addr b)

let intersect_rect =
  foreign "SDL_IntersectRect"
    (ptr rect @-> ptr rect @-> ptr rect @-> returning bool)

let intersect_rect a b =
  let res = make rect in
  if intersect_rect (addr a) (addr b) (addr res) then Some res else None

let intersect_rect_and_line =
  foreign "SDL_IntersectRectAndLine"
    (ptr rect @-> ptr int @-> ptr int @-> ptr int @-> ptr int @->
     returning bool)

let intersect_rect_and_line r x1 y1 x2 y2 =
  let alloc v = allocate int v in
  let x1, y1 = alloc x1, alloc y1 in
  let x2, y2 = alloc x2, alloc y2 in
  if intersect_rect_and_line (addr r) x1 y1 x2 y2
  then Some ((!@x1, !@y1), (!@x2, !@y2))
  else None

let point_in_rect p r =
  (* SDL_FORCE_INLINE *)
  let px = Point.x p in
  let py = Point.y p in
  let rx = Rect.x r in
  let ry = Rect.y r in
  px >= rx && px < rx + Rect.w r && py >= ry && py < ry + Rect.h r

let rect_empty r =
  (* symbol doesn't exist: SDL_FORCE_INLINE directive
     foreign "SDL_RectEmpty" (ptr rect @-> returning bool) *)
  Rect.w r <= 0 || Rect.h r <= 0

let rect_equals a b =
  (* symbol doesn't exist: SDL_FORCE_INLINE directive
    foreign "SDL_RectEquals" (ptr rect @-> ptr rect @-> returning bool) *)
  (Rect.x a = Rect.x b) && (Rect.y a = Rect.y b) &&
  (Rect.w a = Rect.w b) && (Rect.h a = Rect.h b)

let union_rect =
  foreign "SDL_UnionRect"
    (ptr rect @-> ptr rect @-> ptr rect @-> returning void)

let union_rect a b =
  let res = make rect in
  union_rect (addr a) (addr b) (addr res);
  res

(* Palettes *)

type _palette
type palette_struct = _palette structure
let palette_struct : palette_struct typ = structure "SDL_Palette"
let palette_ncolors = field palette_struct "ncolors" int
let palette_colors = field palette_struct "colors" (ptr color)
let _ = field palette_struct "version" uint32_t
let _ = field palette_struct "refcount" int
let () = seal palette_struct

type palette = palette_struct ptr
let palette : palette typ = ptr palette_struct
let palette_opt : palette option typ = ptr_opt palette_struct

let unsafe_palette_of_ptr addr : palette =
  from_voidp palette_struct (ptr_of_raw_address addr)
let unsafe_ptr_of_palette palette =
  raw_address_of_ptr (to_voidp palette)

let alloc_palette =
  foreign "SDL_AllocPalette"
    (int @-> returning (some_to_ok palette_opt))

let free_palette =
  foreign "SDL_FreePalette" (palette @-> returning void)

let get_palette_ncolors p =
  getf (!@ p) palette_ncolors

let get_palette_colors p =
  let ps = !@ p in
  CArray.to_list
    (CArray.from_ptr (getf ps palette_colors) (getf ps palette_ncolors))

let get_palette_colors_ba p =
  let ps = !@ p in
  (* FIXME: ctypes should have a CArray.copy function *)
  let n = getf ps palette_ncolors in
  let ba = Bigarray.(Array1.create int8_unsigned c_layout (n * 4)) in
  let ba_ptr =
    CArray.from_ptr (coerce (ptr int) (ptr color) (bigarray_start array1 ba)) n
  in
  let ca = CArray.from_ptr (getf ps palette_colors) n in
  for i = 0 to n - 1 do CArray.set ba_ptr i (CArray.get ca i) done;
  ba

let set_palette_colors =
  foreign "SDL_SetPaletteColors"
    (palette @-> ptr void @-> int @-> int @-> returning zero_to_ok)

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

let calculate_gamma_ramp =
  foreign "SDL_CalculateGammaRamp"
    (float @-> ptr void @-> returning void)

let calculate_gamma_ramp g =
  let ba = Bigarray.(Array1.create int16_unsigned c_layout 256) in
  calculate_gamma_ramp g (to_voidp (bigarray_start array1 ba));
  ba

module Blend = struct
  type mode = int
  let mode_none = sdl_blendmode_none
  let mode_blend = sdl_blendmode_blend
  let mode_add = sdl_blendmode_add
  let mode_mod = sdl_blendmode_mod

  type operation = int
  let add = sdl_blendoperation_add
  let subtract = sdl_blendoperation_subtract
  let rev_subtract = sdl_blendoperation_rev_subtract
  let minimum = sdl_blendoperation_minimum
  let maximum = sdl_blendoperation_maximum

  type factor = int
  let zero = sdl_blendfactor_zero
  let one = sdl_blendfactor_one
  let src_color = sdl_blendfactor_src_color
  let one_minus_src_color = sdl_blendfactor_one_minus_src_color
  let src_alpha = sdl_blendfactor_src_alpha
  let one_minus_src_alpha = sdl_blendfactor_one_minus_src_alpha
  let dst_color = sdl_blendfactor_dst_color
  let one_minus_dst_color = sdl_blendfactor_one_minus_dst_color
  let dst_alpha = sdl_blendfactor_dst_alpha
  let one_minus_dst_alpha = sdl_blendfactor_one_minus_dst_alpha

end

let compose_custom_blend_mode =
  foreign "SDL_ComposeCustomBlendMode"
    (int @-> int @-> int @-> int @-> int @-> int @-> returning int)

module Pixel = struct
  type format_enum = Unsigned.UInt32.t
  let i = Unsigned.UInt32.of_int32
  let to_uint32 = Unsigned.UInt32.to_int32
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let format_unknown = i sdl_pixelformat_unknown
  let format_index1lsb = i sdl_pixelformat_index1lsb
  let format_index1msb = i sdl_pixelformat_index1msb
  let format_index4lsb = i sdl_pixelformat_index4lsb
  let format_index4msb = i sdl_pixelformat_index4msb
  let format_index8 = i sdl_pixelformat_index8
  let format_rgb332 = i sdl_pixelformat_rgb332
  let format_rgb444 = i sdl_pixelformat_rgb444
  let format_rgb555 = i sdl_pixelformat_rgb555
  let format_bgr555 = i sdl_pixelformat_bgr555
  let format_argb4444 = i sdl_pixelformat_argb4444
  let format_rgba4444 = i sdl_pixelformat_rgba4444
  let format_abgr4444 = i sdl_pixelformat_abgr4444
  let format_bgra4444 = i sdl_pixelformat_bgra4444
  let format_argb1555 = i sdl_pixelformat_argb1555
  let format_rgba5551 = i sdl_pixelformat_rgba5551
  let format_abgr1555 = i sdl_pixelformat_abgr1555
  let format_bgra5551 = i sdl_pixelformat_bgra5551
  let format_rgb565 = i sdl_pixelformat_rgb565
  let format_bgr565 = i sdl_pixelformat_bgr565
  let format_rgb24 = i sdl_pixelformat_rgb24
  let format_bgr24 = i sdl_pixelformat_bgr24
  let format_rgb888 = i sdl_pixelformat_rgb888
  let format_rgbx8888 = i sdl_pixelformat_rgbx8888
  let format_bgr888 = i sdl_pixelformat_bgr888
  let format_bgrx8888 = i sdl_pixelformat_bgrx8888
  let format_argb8888 = i sdl_pixelformat_argb8888
  let format_rgba8888 = i sdl_pixelformat_rgba8888
  let format_abgr8888 = i sdl_pixelformat_abgr8888
  let format_bgra8888 = i sdl_pixelformat_bgra8888
  let format_argb2101010 = i sdl_pixelformat_argb2101010
  let format_yv12 = i sdl_pixelformat_yv12
  let format_iyuv = i sdl_pixelformat_iyuv
  let format_yuy2 = i sdl_pixelformat_yuy2
  let format_uyvy = i sdl_pixelformat_uyvy
  let format_yvyu = i sdl_pixelformat_yvyu
end

(* Note. Giving direct access to the palette field of SDL_PixelFormat
   is problematic. We can't ensure the pointer won't become invalid at
   a certain point. *)

type _pixel_format
type pixel_format_struct = _pixel_format structure
let pixel_format_struct : pixel_format_struct typ = structure "SDL_PixelFormat"
let pf_format = field pixel_format_struct "format" uint32_t
let pf_palette = field pixel_format_struct "palette" palette
let pf_bits_per_pixel = field pixel_format_struct "BitsPerPixel" uint8_t
let pf_bytes_per_pixel = field pixel_format_struct "BytesPerPixel" uint8_t
let _ = field pixel_format_struct "padding" uint16_t
let _ = field pixel_format_struct "Rmask" uint32_t
let _ = field pixel_format_struct "Gmask" uint32_t
let _ = field pixel_format_struct "Bmask" uint32_t
let _ = field pixel_format_struct "Amask" uint32_t
let _ = field pixel_format_struct "Rloss" uint8_t
let _ = field pixel_format_struct "Gloss" uint8_t
let _ = field pixel_format_struct "Bloss" uint8_t
let _ = field pixel_format_struct "Aloss" uint8_t
let _ = field pixel_format_struct "Rshift" uint8_t
let _ = field pixel_format_struct "Gshift" uint8_t
let _ = field pixel_format_struct "Bshift" uint8_t
let _ = field pixel_format_struct "Ashift" uint8_t
let _ = field pixel_format_struct "refcount" int
let _ = field pixel_format_struct "next" (ptr pixel_format_struct)
let () = seal pixel_format_struct

type pixel_format = pixel_format_struct ptr
let pixel_format : pixel_format typ = ptr pixel_format_struct
let pixel_format_opt : pixel_format option typ = ptr_opt pixel_format_struct

let unsafe_pixel_format_of_ptr addr : pixel_format =
  from_voidp pixel_format_struct (ptr_of_raw_address addr)
let unsafe_ptr_of_pixel_format pixel_format =
  raw_address_of_ptr (to_voidp pixel_format)

let alloc_format =
  foreign "SDL_AllocFormat"
    (uint32_t @-> returning (some_to_ok pixel_format_opt))

let free_format =
  foreign "SDL_FreeFormat" (pixel_format @-> returning void)

let get_pixel_format_name =
  foreign "SDL_GetPixelFormatName" (uint32_t @-> returning string)

let get_pixel_format_format pf =
  getf (!@ pf) pf_format

let get_pixel_format_bits_pp pf =
  Unsigned.UInt8.to_int (getf (!@ pf) pf_bits_per_pixel)

let get_pixel_format_bytes_pp pf =
  Unsigned.UInt8.to_int (getf (!@ pf) pf_bytes_per_pixel)

let get_rgb =
  foreign "SDL_GetRGB"
    (int32_as_uint32_t @-> pixel_format @-> ptr uint8_t @->
     ptr uint8_t @-> ptr uint8_t @-> returning void)

let get_rgb pf p =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let to_int = Unsigned.UInt8.to_int in
  let r, g, b = alloc (), alloc (), alloc () in
  get_rgb p pf r g b;
   to_int (!@ r), to_int (!@ g), to_int (!@ b)

let get_rgba =
  foreign "SDL_GetRGBA"
    (int32_as_uint32_t @-> pixel_format @-> ptr uint8_t @->
     ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> returning void)

let get_rgba pf p =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let to_int = Unsigned.UInt8.to_int in
  let r, g, b, a = alloc (), alloc (), alloc (), alloc () in
  get_rgba p pf r g b a;
   to_int (!@ r), to_int (!@ g), to_int (!@ b), to_int (!@ a)

let map_rgb =
  foreign "SDL_MapRGB"
    (pixel_format @-> int_as_uint8_t @-> int_as_uint8_t @-> int_as_uint8_t @->
     returning int32_as_uint32_t)

let map_rgba =
  foreign "SDL_MapRGBA"
    (pixel_format @-> int_as_uint8_t @-> int_as_uint8_t @-> int_as_uint8_t @->
     int_as_uint8_t @-> returning int32_as_uint32_t)

let masks_to_pixel_format_enum =
  foreign "SDL_MasksToPixelFormatEnum"
    (int @-> int32_as_uint32_t @-> int32_as_uint32_t @-> int32_as_uint32_t @->
     int32_as_uint32_t @-> returning uint32_t)

let pixel_format_enum_to_masks =
  foreign "SDL_PixelFormatEnumToMasks"
    (uint32_t @-> ptr int @->
     ptr uint32_t @-> ptr uint32_t @-> ptr uint32_t @-> ptr uint32_t @->
     returning bool)

let pixel_format_enum_to_masks pf =
  let ui () = allocate uint32_t (Unsigned.UInt32.of_int 0) in
  let get iptr = Unsigned.UInt32.to_int32 (!@ iptr) in
  let bpp = allocate int 0 in
  let rm, gm, bm, am = ui (), ui (), ui (), ui () in
  if not (pixel_format_enum_to_masks pf bpp rm gm bm am) then error () else
  Ok (!@ bpp, get rm, get gm, get bm, get am)

let set_pixel_format_palette =
  foreign "SDL_SetPixelFormatPalette"
    (pixel_format @-> palette @-> returning zero_to_ok)

(* Surface *)

type _surface
type surface_struct = _surface structure
let surface_struct : surface_struct typ = structure "SDL_Surface"
let _ = field surface_struct "flags" uint32_t
let surface_format = field surface_struct "format" pixel_format
let surface_w = field surface_struct "w" int
let surface_h = field surface_struct "h" int
let surface_pitch = field surface_struct "pitch" int
let surface_pixels = field surface_struct "pixels" (ptr void)
let _ = field surface_struct "userdata" (ptr void)
let _ = field surface_struct "locked" int
let _ = field surface_struct "lock_data" (ptr void)
let _ = field surface_struct "clip_rect" rect
let _ = field surface_struct "map" (ptr void)
let _ = field surface_struct "refcount" int
let () = seal surface_struct

type surface = surface_struct ptr
let surface : surface typ = ptr surface_struct
let surface_opt : surface option typ = ptr_opt surface_struct

let unsafe_surface_of_ptr addr : surface =
  from_voidp surface_struct (ptr_of_raw_address addr)
let unsafe_ptr_of_surface surface =
  raw_address_of_ptr (to_voidp surface)

let blit_scaled =
  (* SDL_BlitScaled is #ifdef'd to SDL_UpperBlitScaled *)
  foreign "SDL_UpperBlitScaled"
    (surface @-> ptr rect @-> surface @-> ptr rect @-> returning zero_to_ok)

let blit_scaled ~src sr ~dst dr =
  blit_scaled src (Rect.opt_addr sr) dst (Rect.opt_addr dr)

let blit_surface =
  (* SDL_BlitSurface is #ifdef'd to SDL_UpperBlit *)
  foreign "SDL_UpperBlit"
    (surface @-> ptr rect @-> surface @-> ptr rect @-> returning zero_to_ok)

let blit_surface ~src sr ~dst dr =
  blit_surface src (Rect.opt_addr sr) dst (Rect.opt_addr dr)

let convert_pixels =
  foreign "SDL_ConvertPixels"
    (int @-> int @-> uint32_t @-> ptr void @-> int @-> uint32_t @->
     ptr void @-> int @-> returning zero_to_ok)

let convert_pixels ~w ~h ~src sp spitch ~dst dp dpitch =
  (* FIXME: we could try check bounds. *)
  let spitch = ba_kind_byte_size (Bigarray.Array1.kind sp) * spitch in
  let dpitch = ba_kind_byte_size (Bigarray.Array1.kind dp) * dpitch in
  let sp = to_voidp (bigarray_start array1 sp) in
  let dp = to_voidp (bigarray_start array1 dp) in
  convert_pixels w h src sp spitch dst dp dpitch

let convert_surface =
  foreign "SDL_ConvertSurface"
    (surface @-> pixel_format @-> uint32_t @->
     returning (some_to_ok surface_opt))

let convert_surface s pf =
  convert_surface s pf Unsigned.UInt32.zero

let convert_surface_format =
  foreign "SDL_ConvertSurfaceFormat"
    (surface @-> uint32_t @-> uint32_t @-> returning (some_to_ok surface_opt))

let convert_surface_format s pf =
  convert_surface_format s pf Unsigned.UInt32.zero

let create_rgb_surface =
  foreign "SDL_CreateRGBSurface"
    (uint32_t @-> int @-> int @-> int @-> int32_as_uint32_t @->
     int32_as_uint32_t @-> int32_as_uint32_t @-> int32_as_uint32_t @->
     returning (some_to_ok surface_opt))

let create_rgb_surface ~w ~h ~depth rmask gmask bmask amask =
  create_rgb_surface Unsigned.UInt32.zero w h depth rmask gmask bmask amask

let create_rgb_surface_from =
  foreign "SDL_CreateRGBSurfaceFrom"
    (ptr void @-> int @-> int @-> int @-> int @-> int32_as_uint32_t @->
     int32_as_uint32_t @-> int32_as_uint32_t @-> int32_as_uint32_t @->
     returning (some_to_ok surface_opt))

let create_rgb_surface_from p ~w ~h ~depth ~pitch rmask gmask bmask amask =
  (* FIXME: we could try check bounds. *)
  let pitch = ba_kind_byte_size (Bigarray.Array1.kind p) * pitch in
  let p = to_voidp (bigarray_start array1 p) in
  create_rgb_surface_from p w h depth pitch rmask gmask bmask amask

let create_rgb_surface_with_format =
  foreign "SDL_CreateRGBSurfaceWithFormat"
    (uint32_t @-> int @-> int @-> int @-> uint32_t @->
     returning (some_to_ok surface_opt))

let create_rgb_surface_with_format ~w ~h ~depth format =
  create_rgb_surface_with_format Unsigned.UInt32.zero w h depth format

let create_rgb_surface_with_format_from =
  foreign "SDL_CreateRGBSurfaceWithFormatFrom"
    (ptr void @-> int @-> int @-> int @-> int @-> uint32_t @->
     returning (some_to_ok surface_opt))

let create_rgb_surface_with_format_from p ~w ~h ~depth ~pitch format =
  (* FIXME: check bounds? *)
  let pitch = ba_kind_byte_size (Bigarray.Array1.kind p) * pitch in
  let p = to_voidp (bigarray_start array1 p) in
  create_rgb_surface_with_format_from p w h depth pitch format

let duplicate_surface =
  foreign "SDL_DuplicateSurface" (surface @-> returning surface)

let fill_rect =
  foreign "SDL_FillRect"
    (surface @-> ptr rect @-> int32_as_uint32_t @-> returning zero_to_ok)

let fill_rect s r c =
  fill_rect s (Rect.opt_addr r) c

let fill_rects =
  foreign "SDL_FillRects"
    (surface @-> ptr void @-> int @-> int32_as_uint32_t @->
     returning zero_to_ok)

let fill_rects_ba s rs col =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  fill_rects s rs count col

let fill_rects s rs col =
  let a = CArray.of_list rect rs in
  fill_rects s (to_voidp (CArray.start a)) (CArray.length a) col

let free_surface =
  foreign "SDL_FreeSurface" (surface @-> returning void)

let get_clip_rect =
  foreign "SDL_GetClipRect" (surface @-> ptr rect @-> returning void)

let get_clip_rect s =
  let r = make rect in
  (get_clip_rect s (addr r); r)

let get_color_key =
  foreign "SDL_GetColorKey"
    (surface @-> ptr uint32_t @-> returning zero_to_ok)

let get_color_key s =
  let key = allocate uint32_t Unsigned.UInt32.zero in
  match get_color_key s key with
  | Ok () -> Ok (Unsigned.UInt32.to_int32 (!@ key)) | Error _ as e -> e

let get_surface_alpha_mod =
  foreign "SDL_GetSurfaceAlphaMod"
    (surface @-> ptr uint8_t @-> returning zero_to_ok)

let get_surface_alpha_mod s =
  let alpha = allocate uint8_t Unsigned.UInt8.zero in
  match get_surface_alpha_mod s alpha with
  | Ok () -> Ok (Unsigned.UInt8.to_int (!@ alpha)) | Error _ as e -> e

let get_surface_blend_mode =
  foreign "SDL_GetSurfaceBlendMode"
    (surface @-> ptr int @-> returning zero_to_ok)

let get_surface_blend_mode s =
  let mode = allocate int 0 in
  match get_surface_blend_mode s mode with
  Ok () -> Ok (!@ mode) | Error _ as e -> e

let get_surface_color_mod =
  foreign "SDL_GetSurfaceColorMod"
    (surface @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @->
     returning zero_to_ok)

let get_surface_color_mod s =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let get v = Unsigned.UInt8.to_int (!@ v) in
  let r, g, b = alloc (), alloc (), alloc () in
  match get_surface_color_mod s r g b with
  | Ok () -> Ok (get r, get g, get b) | Error _ as e -> e

let get_surface_format_enum s =
  (* We don't give direct access to the format field. This prevents
     memory ownership problems. *)
  get_pixel_format_format (getf (!@ s) surface_format)

let get_surface_pitch s =
  getf (!@ s) surface_pitch

let get_surface_pixels s kind =
  let pitch = get_surface_pitch s in
  let kind_size = ba_kind_byte_size kind in
  if pitch mod kind_size <> 0
  then invalid_arg (err_bigarray_pitch pitch kind_size)
  else
  let h = getf (!@ s) surface_h in
  let ba_size = (pitch * h) / kind_size in
  let pixels = getf (!@ s) surface_pixels in
  let pixels = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) pixels in
  bigarray_of_ptr array1 ba_size kind pixels

let get_surface_size s =
  getf (!@ s) surface_w, getf (!@ s) surface_h

let load_bmp_rw =
  foreign "SDL_LoadBMP_RW"
    (rw_ops @-> bool @-> returning (some_to_ok surface_opt))

let load_bmp_rw rw ~close =
  load_bmp_rw rw close

let load_bmp file =
  (* SDL_LoadBMP is cpp based *)
  match rw_from_file file "rb" with
  | Error _ as e -> e
  | Ok rw -> load_bmp_rw rw ~close:true

let lock_surface =
  foreign "SDL_LockSurface" (surface @-> returning zero_to_ok)

let lower_blit =
  foreign "SDL_LowerBlit"
    (surface @-> ptr rect @-> surface @-> ptr rect @-> returning zero_to_ok)

let lower_blit ~src sr ~dst dr =
  lower_blit src (addr sr) dst (addr dr)

let lower_blit_scaled =
  foreign "SDL_LowerBlitScaled"
    (surface @-> ptr rect @-> surface @-> ptr rect @-> returning zero_to_ok)

let lower_blit_scaled ~src sr ~dst dr =
  lower_blit_scaled src (addr sr) dst (addr dr)

let save_bmp_rw =
  foreign "SDL_SaveBMP_RW"
    (surface @-> rw_ops @-> bool @-> returning zero_to_ok)

let save_bmp_rw s rw ~close =
  save_bmp_rw s rw close

let save_bmp s file =
  (* SDL_SaveBMP is cpp based *)
  match rw_from_file file "wb" with
  | Error _ as e -> e
  | Ok rw -> save_bmp_rw s rw ~close:true

let set_clip_rect =
  foreign "SDL_SetClipRect" (surface @-> ptr rect @-> returning bool)

let set_clip_rect s r =
  set_clip_rect s (addr r)

let set_color_key =
  foreign "SDL_SetColorKey"
    (surface @-> bool @-> int32_as_uint32_t @-> returning zero_to_ok)

let set_surface_alpha_mod =
  foreign "SDL_SetSurfaceAlphaMod"
    (surface @-> int_as_uint8_t @-> returning zero_to_ok)

let set_surface_blend_mode =
  foreign "SDL_SetSurfaceBlendMode"
    (surface @-> int @-> returning zero_to_ok)

let set_surface_color_mod =
  foreign "SDL_SetSurfaceColorMod"
    (surface @-> int_as_uint8_t @-> int_as_uint8_t @-> int_as_uint8_t @->
     returning zero_to_ok)

let set_surface_palette =
  foreign "SDL_SetSurfacePalette"
    (surface @-> palette @-> returning zero_to_ok)

let set_surface_rle =
  foreign "SDL_SetSurfaceRLE" (surface @-> bool @-> returning zero_to_ok)

let unlock_surface =
  foreign "SDL_UnlockSurface" (surface @-> returning void)

(* Renderers *)

type flip = int
module Flip = struct
  let ( + ) = ( lor )
  let none = sdl_flip_none
  let horizontal = sdl_flip_horizontal
  let vertical = sdl_flip_vertical
end

type texture = unit ptr
let texture : texture typ = ptr void
let texture_opt : texture option typ = ptr_opt void

let unsafe_texture_of_ptr addr : texture =
  ptr_of_raw_address addr
let unsafe_ptr_of_texture texture =
  raw_address_of_ptr (to_voidp texture)

type renderer = unit ptr
let renderer : renderer typ = ptr void
let renderer_opt : renderer option typ = ptr_opt void

let unsafe_renderer_of_ptr addr : renderer =
  ptr_of_raw_address addr
let unsafe_ptr_of_renderer renderer =
  raw_address_of_ptr (to_voidp renderer)

module Renderer = struct
  type flags = Unsigned.uint32
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let ( - ) f f' = Unsigned.UInt32.(logand f (lognot f'))
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let software = i sdl_renderer_software
  let accelerated = i sdl_renderer_accelerated
  let presentvsync = i sdl_renderer_presentvsync
  let targettexture = i sdl_renderer_targettexture
end

type renderer_info =
  { ri_name : string;
    ri_flags : Renderer.flags;
    ri_texture_formats : Pixel.format_enum list;
    ri_max_texture_width : int;
    ri_max_texture_height : int; }

let renderer_info = structure "SDL_RendererInfo"
let ri_name = field renderer_info "name" string
let ri_flags = field renderer_info "flags" uint32_t
let ri_num_tf = field renderer_info "num_texture_formats" uint32_t
let ri_tfs = field renderer_info "texture_formats" (array 16 uint32_t)
let ri_max_texture_width = field renderer_info "max_texture_width" int
let ri_max_texture_height = field renderer_info "max_texture_height" int
let () = seal renderer_info

let renderer_info_of_c c =
  let ri_name = getf c ri_name in
  let ri_flags = getf c ri_flags in
  let num_tf = Unsigned.UInt32.to_int (getf c ri_num_tf) in
  let tfs = getf c ri_tfs in
  let ri_texture_formats =
    let acc = ref [] in
    for i = 0 to num_tf - 1 do acc := (CArray.get tfs i) :: !acc done;
    List.rev !acc
  in
  let ri_max_texture_width = getf c ri_max_texture_width in
  let ri_max_texture_height = getf c ri_max_texture_height in
  { ri_name; ri_flags; ri_texture_formats; ri_max_texture_width;
    ri_max_texture_height }

let create_renderer =
  foreign "SDL_CreateRenderer"
    (window @-> int @-> uint32_t @-> returning (some_to_ok renderer_opt))

let create_renderer ?(index = -1) ?(flags = Unsigned.UInt32.zero) w =
  create_renderer w index flags

let create_software_renderer =
  foreign "SDL_CreateSoftwareRenderer"
    (surface @-> returning (some_to_ok renderer_opt))

let destroy_renderer =
  foreign "SDL_DestroyRenderer" (renderer @-> returning void)

let get_num_render_drivers =
  foreign "SDL_GetNumRenderDrivers" (void @-> returning nat_to_ok)

let get_render_draw_blend_mode =
  foreign "SDL_GetRenderDrawBlendMode"
    (renderer @-> ptr int @-> returning zero_to_ok)

let get_render_draw_blend_mode r =
  let m = allocate int 0 in
  match get_render_draw_blend_mode r m with
  | Ok () -> Ok !@m | Error _ as e -> e

let get_render_draw_color =
  foreign "SDL_GetRenderDrawColor"
    (renderer @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @->
     ptr uint8_t @-> returning zero_to_ok)

let get_render_draw_color rend =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let get v = Unsigned.UInt8.to_int (!@ v) in
  let r, g, b, a = alloc (), alloc (), alloc (), alloc () in
  match get_render_draw_color rend r g b a with
  | Ok () -> Ok (get r, get g, get b, get a) | Error _ as e -> e

let get_render_driver_info =
  foreign "SDL_GetRenderDriverInfo"
    (int @-> ptr renderer_info @-> returning zero_to_ok)

let get_render_driver_info i =
  let info = make renderer_info in
  match get_render_driver_info i (addr info) with
  | Ok () -> Ok (renderer_info_of_c info) | Error _ as e -> e

let get_render_target =
  foreign "SDL_GetRenderTarget" (renderer @-> returning texture_opt)

let get_renderer =
  foreign "SDL_GetRenderer"
    (window @-> returning (some_to_ok renderer_opt))

let get_renderer_info =
  foreign "SDL_GetRendererInfo"
    (renderer @-> ptr renderer_info @-> returning zero_to_ok)

let get_renderer_info r =
  let info = make renderer_info in
  match get_renderer_info r (addr info) with
  | Ok () -> Ok (renderer_info_of_c info) | Error _ as e -> e

let get_renderer_output_size =
  foreign "SDL_GetRendererOutputSize"
    (renderer @-> ptr int @-> ptr int @-> returning zero_to_ok)

let get_renderer_output_size r =
  let w = allocate int 0 in
  let h = allocate int 0 in
  match get_renderer_output_size r w h with
  | Ok () -> Ok (!@ w, !@ h) | Error _ as e -> e

let render_clear =
  foreign "SDL_RenderClear" (renderer @-> returning zero_to_ok)

let render_copy =
  foreign "SDL_RenderCopy"
    (renderer @-> texture @-> ptr rect @-> ptr rect @->
     returning zero_to_ok)

let render_copy ?src ?dst r t =
  render_copy r t (Rect.opt_addr src) (Rect.opt_addr dst)

let render_copy_ex =
  foreign "SDL_RenderCopyEx"
    (renderer @-> texture @-> ptr rect @-> ptr rect @-> double @->
     ptr point @-> int @-> returning zero_to_ok)

let render_copy_ex ?src ?dst r t angle c flip =
  render_copy_ex r t (Rect.opt_addr src) (Rect.opt_addr dst) angle
    (Point.opt_addr c) flip

let render_draw_line =
  foreign "SDL_RenderDrawLine"
    (renderer @-> int @-> int @-> int @-> int @-> returning zero_to_ok)

let render_draw_line_f =
  foreign "SDL_RenderDrawLineF"
    (renderer @-> float @-> float @-> float @-> float @-> returning zero_to_ok)

let render_draw_lines =
  foreign "SDL_RenderDrawLines"
    (renderer @-> ptr void @-> int @-> returning zero_to_ok)

let render_draw_lines_ba r ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  render_draw_lines r ps count

let render_draw_lines r ps =
  let a = CArray.of_list point ps in
  render_draw_lines r (to_voidp (CArray.start a)) (CArray.length a)

let render_draw_point =
  foreign "SDL_RenderDrawPoint"
    (renderer @-> int @-> int @-> returning zero_to_ok)

let render_draw_points =
  foreign "SDL_RenderDrawPoints"
    (renderer @-> ptr void @-> int @-> returning zero_to_ok)

let render_draw_points_ba r ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  render_draw_points r ps count

let render_draw_points r ps =
  let a = CArray.of_list point ps in
  render_draw_points r (to_voidp (CArray.start a)) (CArray.length a)

let render_draw_point_f =
  foreign "SDL_RenderDrawPointF"
    (renderer @-> float @-> float @-> returning zero_to_ok)

let render_draw_points_f =
  foreign "SDL_RenderDrawPointsF"
    (renderer @-> ptr void @-> int @-> returning zero_to_ok)

let render_draw_points_f_ba r ps =
  let len = Bigarray.Array1.dim ps in
  if len mod 2 <> 0 then invalid_arg (err_length_mul len 2) else
  let count = len / 2 in
  let ps = to_voidp (bigarray_start array1 ps) in
  render_draw_points_f r ps count

let render_draw_points_f r ps =
  let a = CArray.of_list fpoint ps in
  render_draw_points_f r (to_voidp (CArray.start a)) (CArray.length a)

let render_draw_rect =
  foreign "SDL_RenderDrawRect"
    (renderer @-> ptr rect @-> returning zero_to_ok)

let render_draw_rect rend r =
  render_draw_rect rend (Rect.opt_addr r)

let render_draw_rects =
  foreign "SDL_RenderDrawRects"
    (renderer @-> ptr void @-> int @-> returning zero_to_ok)

let render_draw_rects_ba r rs =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  render_draw_rects r rs count

let render_draw_rects r rs =
  let a = CArray.of_list rect rs in
  render_draw_rects r (to_voidp (CArray.start a)) (CArray.length a)

let render_fill_rect =
  foreign "SDL_RenderFillRect"
    (renderer @-> ptr rect @-> returning zero_to_ok)

let render_fill_rect rend r =
  render_fill_rect rend (Rect.opt_addr r)

let render_fill_rects =
  foreign "SDL_RenderFillRects"
    (renderer @-> ptr void @-> int @-> returning zero_to_ok)

let render_fill_rects_ba r rs =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  render_fill_rects r rs count

let render_fill_rects r rs =
  let a = CArray.of_list rect rs in
  render_fill_rects r (to_voidp (CArray.start a)) (CArray.length a)

let render_geometry =
  foreign "SDL_RenderGeometry"
    (renderer @-> texture @-> ptr void @-> int @-> ptr void @-> int @->
     returning zero_to_ok)

let render_geometry ?indices ?texture r vertices =
  let a1 = CArray.of_list vertex vertices in
  let t = match texture with
  | None -> null | Some texture -> texture
  in
  let a2_ptr, a2_len = match indices with
  | None -> (null, 0)
  | Some is ->
      let a2 = CArray.of_list int is in
      (to_voidp (CArray.start a2), CArray.length a2)
  in
  render_geometry
    r t (to_voidp (CArray.start a1)) (CArray.length a1) a2_ptr a2_len

let render_geometry_raw =
  foreign "SDL_RenderGeometryRaw"
    (renderer @-> texture @->
     ptr void @-> int @->
     ptr void @-> int @->
     ptr void @-> int @->
     int @-> ptr void @-> int @-> int @-> returning zero_to_ok)

let render_geometry_raw
    ?indices ?texture r ~xy ?(xy_stride = 8) ~color ?(color_stride = 4)
    ~uv ?(uv_stride = 8) ~num_vertices ()
  =
  let t = match texture with
  | None -> null | Some texture -> texture
  in
  let i_ptr, i_len = match indices with
  | None -> null, 0
  | Some is -> to_voidp (bigarray_start array1 is), Bigarray.Array1.dim is
  in
  let i_stride = 4 in (* indices are assumed to be 4-byte integers *)
  let xy_ptr = to_voidp (bigarray_start array1 xy) in
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
  let uv_ptr = to_voidp (bigarray_start array1 uv) in
  let uv_len_bytes = Bigarray.Array1.dim uv * 4 in
  let uv_exp_bytes = num_vertices * uv_stride - (uv_stride - 8) in
  if uv_len_bytes < uv_exp_bytes then begin
    let msg =
      "uv " ^ err_array_to_short ~exp:uv_exp_bytes ~fnd:uv_len_bytes
    in
    invalid_arg msg
  end;
  render_geometry_raw
    r t xy_ptr xy_stride color_ptr color_stride uv_ptr uv_stride num_vertices
    i_ptr i_len i_stride

let render_get_clip_rect =
  foreign "SDL_RenderGetClipRect"
    (renderer @-> ptr rect @-> returning void)

let render_get_clip_rect rend =
  let r = make rect in
  render_get_clip_rect rend (addr r);
  r

let render_is_clip_enabled =
  foreign "SDL_RenderIsClipEnabled" (renderer @-> returning bool)

let render_get_integer_scale =
 foreign "SDL_RenderGetIntegerScale"
    (renderer @-> returning bool)

let render_get_logical_size =
  foreign "SDL_RenderGetLogicalSize"
    (renderer @-> ptr int @-> ptr int @-> returning void)

let render_get_logical_size r =
  let w = allocate int 0 in
  let h = allocate int 0 in
  render_get_logical_size r w h;
  !@ w, !@ h

let render_get_scale =
  foreign "SDL_RenderGetScale"
    (renderer @-> ptr float @-> ptr float @-> returning void)

let render_get_scale r =
  let x = allocate float 0. in
  let y = allocate float 0. in
  render_get_scale r x y;
  !@ x, !@ y

let render_get_viewport =
  foreign "SDL_RenderGetViewport"
    (renderer @-> ptr rect @-> returning void)

let render_get_viewport rend =
  let r = make rect in
  render_get_viewport rend (addr r);
  r

let render_present =
  foreign ~release_runtime_lock:true "SDL_RenderPresent"
    (renderer @-> returning void)

let render_read_pixels =
  foreign "SDL_RenderReadPixels"
    (renderer @-> ptr rect @-> uint32_t @-> ptr void @-> int @->
     returning zero_to_ok)

let render_read_pixels r rect format pixels pitch =
  let format = match format with None -> Unsigned.UInt32.zero | Some f -> f in
  let pixels = to_voidp (bigarray_start array1 pixels) in
  render_read_pixels r (Rect.opt_addr rect) format pixels pitch

let render_set_clip_rect =
  foreign "SDL_RenderSetClipRect"
    (renderer @-> ptr rect @-> returning zero_to_ok)

let render_set_clip_rect rend r =
  render_set_clip_rect rend (Rect.opt_addr r)

let render_set_integer_scale =
  foreign "SDL_RenderSetIntegerScale"
    (renderer @-> bool @-> returning zero_to_ok)

let render_set_logical_size =
  foreign "SDL_RenderSetLogicalSize"
    (renderer @-> int @-> int @-> returning zero_to_ok)

let render_set_scale =
  foreign "SDL_RenderSetScale"
    (renderer @-> float @-> float @-> returning zero_to_ok)

let render_set_viewport =
  foreign "SDL_RenderSetViewport"
    (renderer @-> ptr rect @-> returning zero_to_ok)

let render_set_viewport rend r =
  render_set_viewport rend (Rect.opt_addr r)

let render_target_supported =
  foreign "SDL_RenderTargetSupported" (renderer @-> returning bool)

let set_render_draw_blend_mode =
  foreign "SDL_SetRenderDrawBlendMode"
    (renderer @-> int @-> returning zero_to_ok)

let set_render_draw_color =
  foreign "SDL_SetRenderDrawColor"
    (renderer @-> int_as_uint8_t @-> int_as_uint8_t @-> int_as_uint8_t @->
     int_as_uint8_t @-> returning zero_to_ok)

let set_render_target =
  foreign "SDL_SetRenderTarget"
    (renderer @-> texture @-> returning zero_to_ok)

let set_render_target r t =
  let t = match t with None -> null | Some t -> t in
  set_render_target r t

(* Textures *)

module Texture = struct
  type access = int
  let access_static = sdl_textureaccess_static
  let access_streaming = sdl_textureaccess_streaming
  let access_target = sdl_textureaccess_target

  let i = Unsigned.UInt32.of_int
  type modulate = Unsigned.uint32
  let modulate_none = i sdl_texturemodulate_none
  let modulate_color = i sdl_texturemodulate_color
  let modulate_alpha = i sdl_texturemodulate_alpha
end

let create_texture =
  foreign "SDL_CreateTexture"
    (renderer @-> uint32_t @-> int @-> int @-> int @->
     returning (some_to_ok texture_opt))

let create_texture r pf access ~w ~h =
  create_texture r pf access w h

let create_texture_from_surface =
  foreign "SDL_CreateTextureFromSurface"
    (renderer @-> surface @-> returning (some_to_ok texture_opt))

let destroy_texture =
  foreign "SDL_DestroyTexture" (texture @-> returning void)

let get_texture_alpha_mod =
  foreign "SDL_GetTextureAlphaMod"
    (texture @-> ptr uint8_t @-> returning zero_to_ok)

let get_texture_alpha_mod t =
  let alpha = allocate uint8_t Unsigned.UInt8.zero in
  match get_texture_alpha_mod t alpha with
  | Ok () -> Ok (Unsigned.UInt8.to_int (!@ alpha)) | Error _ as e -> e

let get_texture_blend_mode =
  foreign "SDL_GetTextureBlendMode"
    (texture @-> ptr int @-> returning zero_to_ok)

let get_texture_blend_mode t =
  let m = allocate int 0 in
  match get_texture_blend_mode t m with
  | Ok () -> Ok (!@ m) | Error _ as e -> e

let get_texture_color_mod =
  foreign "SDL_GetTextureColorMod"
    (texture @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @->
     returning zero_to_ok)

let get_texture_color_mod t =
  let alloc () = allocate uint8_t Unsigned.UInt8.zero in
  let get v = Unsigned.UInt8.to_int (!@ v) in
  let r, g, b = alloc (), alloc (), alloc () in
  match get_texture_color_mod t r g b with
  | Ok () -> Ok (get r, get g, get b) | Error _ as e -> e

let query_texture =
  foreign "SDL_QueryTexture"
    (texture @-> ptr uint32_t @-> ptr int @-> ptr int @-> ptr int @->
     returning zero_to_ok)

let _texture_height t =
  let h = allocate int 0 in
  let unull = coerce (ptr void) (ptr uint32_t) null in
  let inull = coerce (ptr void) (ptr int) null in
  match query_texture t unull inull inull h with
  | Ok () -> Ok (!@ h) | Error _ as e -> e

let lock_texture =
  foreign "SDL_LockTexture"
    (texture @-> ptr rect @-> ptr (ptr void) @-> ptr int @->
     returning zero_to_ok)

let lock_texture t r kind =
  match (match r with None -> _texture_height t | Some r -> Ok (Rect.h r)) with
  | Error _ as e -> e
  | Ok h ->
      let pitch = allocate int 0 in
      let p = allocate (ptr void) null in
      match lock_texture t (Rect.opt_addr r) p pitch with
      | Error _ as e -> e
      | Ok () ->
          let p = !@ p in
          let pitch = !@ pitch in
          let kind_size = ba_kind_byte_size kind in
          if pitch mod kind_size <> 0
          then invalid_arg (err_bigarray_pitch pitch kind_size)
          else
          let ba_size = (pitch * h) / kind_size in
          let pixels = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
          Ok (bigarray_of_ptr array1 ba_size kind pixels, pitch / kind_size)

let query_texture t =
  let pf = allocate uint32_t Unsigned.UInt32.zero in
  let access = allocate int 0 in
  let w = allocate int 0 in
  let h = allocate int 0 in
  match query_texture t pf access w h with
  | Ok () -> Ok (!@ pf, !@ access, (!@ w, !@ h)) | Error _ as e -> e

let set_texture_alpha_mod =
  foreign "SDL_SetTextureAlphaMod"
    (texture @-> int_as_uint8_t @-> returning zero_to_ok)

let set_texture_blend_mode =
  foreign "SDL_SetTextureBlendMode"
    (texture @-> int @-> returning zero_to_ok)

let set_texture_color_mod =
  foreign "SDL_SetTextureColorMod"
    (texture @-> int_as_uint8_t @-> int_as_uint8_t @-> int_as_uint8_t @->
     returning zero_to_ok)

let unlock_texture =
  foreign "SDL_UnlockTexture" (texture @-> returning void)

let update_texture =
  foreign "SDL_UpdateTexture"
    (texture @-> ptr rect @-> ptr void @-> int @-> returning zero_to_ok)

let update_texture t rect pixels pitch =
  let pitch = pitch * (ba_kind_byte_size (Bigarray.Array1.kind pixels)) in
  let pixels = to_voidp (bigarray_start array1 pixels) in
  update_texture t (Rect.opt_addr rect) pixels pitch

let update_yuv_texture =
  foreign "SDL_UpdateYUVTexture"
    (texture @-> ptr rect @->
     ptr void @-> int @-> ptr void @-> int @-> ptr void @-> int @->
     returning zero_to_ok)

let update_yuv_texture r rect ~y ypitch ~u upitch ~v vpitch =
  let yp = to_voidp (bigarray_start array1 y) in
  let up = to_voidp (bigarray_start array1 u) in
  let vp = to_voidp (bigarray_start array1 v) in
  update_yuv_texture r (Rect.opt_addr rect) yp ypitch up upitch vp vpitch

(* Video drivers *)

let get_current_video_driver =
  foreign "SDL_GetCurrentVideoDriver" (void @-> returning string_opt)

let get_num_video_drivers =
  foreign "SDL_GetNumVideoDrivers" (void @-> returning nat_to_ok)

let get_video_driver =
  foreign "SDL_GetVideoDriver" (int @-> returning (some_to_ok string_opt))

let video_init =
  foreign "SDL_VideoInit" (string_opt @-> returning zero_to_ok)

let video_quit =
  foreign "SDL_VideoQuit" (void @-> returning void)

(* Displays *)

type driverdata = unit ptr
let driverdata = ptr_opt void

type display_mode =
  { dm_format : Pixel.format_enum;
    dm_w : int;
    dm_h : int;
    dm_refresh_rate : int option;
    dm_driverdata : driverdata option }

type _display_mode
let display_mode : _display_mode structure typ = structure "SDL_DisplayMode"
let dm_format = field display_mode "format" uint32_t
let dm_w = field display_mode "w" int
let dm_h = field display_mode "h" int
let dm_refresh_rate = field display_mode "refresh_rate" int
let dm_driverdata = field display_mode "driverdata" driverdata
let () = seal display_mode

let display_mode_to_c o =
  let c = make display_mode in
  let rate = match o.dm_refresh_rate with None -> 0 | Some r -> r in
  setf c dm_format o.dm_format;
  setf c dm_w o.dm_w;
  setf c dm_h o.dm_h;
  setf c dm_refresh_rate rate;
  setf c dm_driverdata o.dm_driverdata;
  c

let display_mode_of_c c =
  let dm_format = getf c dm_format in
  let dm_w = getf c dm_w in
  let dm_h = getf c dm_h in
  let dm_refresh_rate = match getf c dm_refresh_rate with
  | 0 -> None | r -> Some r
  in
  let dm_driverdata = getf c dm_driverdata in
  { dm_format; dm_w; dm_h; dm_refresh_rate; dm_driverdata }

let get_closest_display_mode =
  foreign "SDL_GetClosestDisplayMode"
    (int @-> ptr display_mode @-> ptr display_mode @->
       returning (ptr_opt void))

let get_closest_display_mode i m =
  let mode = display_mode_to_c m in
  let closest = make display_mode in
  match get_closest_display_mode i (addr mode) (addr closest) with
  | None -> None
  | Some _ -> Some (display_mode_of_c closest)

let get_current_display_mode =
  foreign "SDL_GetCurrentDisplayMode"
    (int @-> ptr display_mode @-> returning zero_to_ok)

let get_current_display_mode i =
  let mode = make display_mode in
  match get_current_display_mode i (addr mode) with
  | Ok () -> Ok (display_mode_of_c mode) | Error _ as e -> e

let get_desktop_display_mode =
  foreign "SDL_GetDesktopDisplayMode"
    (int @-> ptr display_mode @-> returning zero_to_ok)

let get_desktop_display_mode i =
  let mode = make display_mode in
  match get_desktop_display_mode i (addr mode) with
  | Ok () -> Ok (display_mode_of_c mode) | Error _ as e -> e

let get_display_bounds =
  foreign "SDL_GetDisplayBounds"
    (int @-> ptr rect @-> returning zero_to_ok)

let get_display_bounds i =
  let r = make rect in
  match get_display_bounds i (addr r) with
  | Ok () -> Ok r | Error _ as e -> e

let get_display_dpi =
  foreign "SDL_GetDisplayDPI"
    (int @-> ptr float @-> ptr float @-> ptr float @-> returning zero_to_ok)

let get_display_dpi display =
  let diagonal = allocate float 0. in
  let horizontal = allocate float 0. in
  let vertical = allocate float 0. in
  match get_display_dpi display diagonal horizontal vertical with
  | Ok () -> Ok (!@diagonal,!@horizontal,!@vertical)
  | Error _ as err -> err

let get_display_mode =
  foreign "SDL_GetDisplayMode"
    (int @-> int @-> ptr display_mode @-> returning zero_to_ok)

let get_display_mode d i =
  let mode = make display_mode in
  match get_display_mode d i (addr mode) with
  | Ok () -> Ok (display_mode_of_c mode) | Error _ as e -> e

let get_display_usable_bounds =
  foreign "SDL_GetDisplayUsableBounds"
    (int @-> ptr rect @-> returning zero_to_ok)

let get_display_usable_bounds i =
  let r = make rect in
  match get_display_usable_bounds i (addr r) with
  | Ok () -> Ok r | Error _ as e -> e

let get_num_display_modes =
  foreign "SDL_GetNumDisplayModes" (int @-> returning nat_to_ok)

let get_display_name =
  foreign "SDL_GetDisplayName" (int @-> returning (some_to_ok string_opt))

let get_num_video_displays =
  foreign "SDL_GetNumVideoDisplays" (void @-> returning nat_to_ok)

(* Windows *)

module Window = struct
  let pos_undefined = sdl_windowpos_undefined
  let pos_centered = sdl_windowpos_centered

  type flags = Unsigned.uint32
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let ( - ) f f' = Unsigned.UInt32.(logand f (lognot f'))
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let windowed = i 0
  let fullscreen = i sdl_window_fullscreen
  let fullscreen_desktop = i sdl_window_fullscreen_desktop
  let opengl = i sdl_window_opengl
  let shown = i sdl_window_shown
  let hidden = i sdl_window_hidden
  let borderless = i sdl_window_borderless
  let resizable = i sdl_window_resizable
  let minimized = i sdl_window_minimized
  let maximized = i sdl_window_maximized
  let input_grabbed = i sdl_window_input_grabbed
  let input_focus = i sdl_window_input_focus
  let mouse_focus = i sdl_window_mouse_focus
  let foreign = i sdl_window_foreign
  let allow_highdpi = i sdl_window_allow_highdpi
  let mouse_capture = i sdl_window_mouse_capture
  let always_on_top = i sdl_window_always_on_top
  let skip_taskbar = i sdl_window_skip_taskbar
  let utility = i sdl_window_utility
  let popup_menu = i sdl_window_popup_menu
  let vulkan = i sdl_window_vulkan
end

let create_window =
  foreign "SDL_CreateWindow"
    (string @-> int @-> int @-> int @-> int @-> uint32_t @->
     returning (some_to_ok window_opt))

let create_window t ?(x = Window.pos_undefined) ?(y = Window.pos_undefined)
    ~w ~h flags = create_window t x y w h flags

let create_window_and_renderer =
  foreign "SDL_CreateWindowAndRenderer"
    (int @-> int @-> uint32_t @-> ptr window @-> ptr renderer @->
     (returning zero_to_ok))

let create_window_and_renderer ~w ~h flags =
  let win = allocate window null in
  let r = allocate renderer null in
  match create_window_and_renderer w h flags win r with
  | Ok () -> Ok (!@ win, !@ r) | Error _ as e -> e

let destroy_window =
  foreign "SDL_DestroyWindow" (window @-> returning void)

let get_window_brightness =
  foreign "SDL_GetWindowBrightness" (window @-> returning float)

let get_window_borders_size =
  foreign "SDL_GetWindowBordersSize"
    (window @-> ptr int @-> ptr int @-> ptr int @-> ptr int @->
     returning zero_to_ok)

let get_window_borders_size w =
  let top = allocate int 0 in
  let left = allocate int 0 in
  let bottom = allocate int 0 in
  let right = allocate int 0 in
  match get_window_borders_size w top bottom left right with
  | Ok () -> Ok (!@ top, !@ left, !@ bottom, !@ right)
  | Error _ as err -> err

let get_window_display_index =
  foreign "SDL_GetWindowDisplayIndex" (window @-> returning nat_to_ok)

let get_window_display_mode =
  foreign "SDL_GetWindowDisplayMode"
    (window @-> (ptr display_mode) @-> returning int)

let get_window_display_mode w =
  let mode = make display_mode in
  match get_window_display_mode w (addr mode) with
  | 0 -> Ok (display_mode_of_c mode) | err -> error ()

let get_window_flags =
  foreign "SDL_GetWindowFlags" (window @-> returning uint32_t)

let get_window_from_id =
  foreign "SDL_GetWindowFromID"
    (int_as_uint32_t @-> returning (some_to_ok window_opt))

let get_window_gamma_ramp =
  foreign "SDL_GetWindowGammaRamp"
    (window @-> ptr void @-> ptr void @-> ptr void @-> returning zero_to_ok)

let get_window_gamma_ramp w =
  let create_ramp () = ba_create Bigarray.int16_unsigned 256 in
  let r, g, b = create_ramp (), create_ramp (), create_ramp () in
  let ramp_ptr r = to_voidp (bigarray_start array1 r) in
  match get_window_gamma_ramp w (ramp_ptr r) (ramp_ptr g) (ramp_ptr b) with
  | Ok () -> Ok (r, g, b) | Error _ as e -> e

let get_window_grab =
  foreign "SDL_GetWindowGrab" (window @-> returning bool)

let get_grabbed_window =
  foreign "SDL_GetGrabbedWindow" (void @-> returning window)

let get_window_id =
  foreign "SDL_GetWindowID" (window @-> returning int_as_uint32_t)

let get_window_maximum_size =
  foreign "SDL_GetWindowMaximumSize"
    (window @-> (ptr int) @-> (ptr int) @-> returning void)

let get_window_maximum_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  get_window_maximum_size win w h;
  !@ w, !@ h

let get_window_minimum_size =
  foreign "SDL_GetWindowMinimumSize"
    (window @-> (ptr int) @-> (ptr int) @-> returning void)

let get_window_minimum_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  get_window_minimum_size win w h;
  !@ w, !@ h

let get_window_opacity =
  foreign "SDL_GetWindowOpacity"
    (window @-> (ptr float) @-> returning zero_to_ok)

let get_window_opacity win =
  let x = allocate float 0. in
  match get_window_opacity win x with
  | Ok () -> Ok !@x
  | Error _ as e -> e

let get_window_pixel_format =
  foreign "SDL_GetWindowPixelFormat" (window @-> returning uint32_t)

let get_window_position =
  foreign "SDL_GetWindowPosition"
    (window @-> (ptr int) @-> (ptr int) @-> returning void)

let get_window_position win =
  let x = allocate int 0 in
  let y = allocate int 0 in
  get_window_position win x y;
  !@ x, !@ y

let get_window_size =
  foreign "SDL_GetWindowSize"
    (window @-> (ptr int) @-> (ptr int) @-> returning void)

let get_window_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  get_window_size win w h;
  !@ w, !@ h

let get_window_surface =
  foreign "SDL_GetWindowSurface"
    (window @-> returning (some_to_ok surface_opt))

let get_window_title =
  foreign "SDL_GetWindowTitle" (window @-> returning string)

let hide_window =
  foreign "SDL_HideWindow" (window @-> returning void)

let maximize_window =
  foreign "SDL_MaximizeWindow" (window @-> returning void)

let minimize_window =
  foreign "SDL_MinimizeWindow" (window @-> returning void)

let raise_window =
  foreign "SDL_RaiseWindow" (window @-> returning void)

let restore_window =
  foreign "SDL_RestoreWindow" (window @-> returning void)

let set_window_bordered =
  foreign "SDL_SetWindowBordered" (window @-> bool @-> returning void)

let set_window_brightness =
  foreign "SDL_SetWindowBrightness"
    (window @-> float @-> returning zero_to_ok)

let set_window_display_mode =
  foreign "SDL_SetWindowDisplayMode"
    (window @-> (ptr display_mode) @-> returning zero_to_ok)

let set_window_display_mode w m =
  let mode = display_mode_to_c m in
  set_window_display_mode w (addr mode)

let set_window_fullscreen =
  foreign "SDL_SetWindowFullscreen"
    (window @-> uint32_t @-> returning zero_to_ok)

let set_window_gamma_ramp =
  foreign "SDL_SetWindowGammaRamp"
    (window @-> ptr void @-> ptr void @-> ptr void @->
     returning zero_to_ok)

let set_window_gamma_ramp w r g b =
  let ramp_ptr r = to_voidp (bigarray_start array1 r) in
  set_window_gamma_ramp w (ramp_ptr r) (ramp_ptr g) (ramp_ptr b)

let set_window_grab =
  foreign "SDL_SetWindowGrab" (window @-> bool @-> returning void)

let set_window_icon =
  foreign "SDL_SetWindowIcon" (window @-> surface @-> returning void)

let set_window_input_focus =
  foreign "SDL_SetWindowInputFocus" (window @-> returning zero_to_ok)

let set_window_maximum_size =
  foreign "SDL_SetWindowMaximumSize"
    (window @-> int @-> int @-> returning void)

let set_window_maximum_size win ~w ~h =
  set_window_maximum_size win w h

let set_window_minimum_size =
  foreign "SDL_SetWindowMinimumSize"
    (window @-> int @-> int @-> returning void)

let set_window_minimum_size win ~w ~h =
  set_window_minimum_size win w h

let set_window_modal_for =
  foreign "SDL_SetWindowModalFor" ( window @-> window @-> returning zero_to_ok)

let set_window_modal_for ~modal ~parent = set_window_modal_for modal parent

let set_window_opacity =
  foreign "SDL_SetWindowOpacity" ( window @-> float @-> returning zero_to_ok)

let set_window_position =
  foreign "SDL_SetWindowPosition"
    (window @-> int @-> int @-> returning void)

let set_window_position win ~x ~y =
  set_window_position win x y

let set_window_resizable =
  foreign "SDL_SetWindowResizable" (window @-> bool @-> returning void)

let set_window_size =
  foreign "SDL_SetWindowSize" (window @-> int @-> int @-> returning void)

let set_window_size win ~w ~h =
  set_window_size win w h

let set_window_title =
  foreign "SDL_SetWindowTitle" (window @-> string @-> returning void)

let show_window =
  foreign "SDL_ShowWindow" (window @-> returning void)

let update_window_surface =
  foreign "SDL_UpdateWindowSurface" (window @-> returning zero_to_ok)

let update_window_surface_rects =
  foreign "SDL_UpdateWindowSurfaceRects"
    (window @-> ptr void @-> int @-> returning zero_to_ok)

let update_window_surface_rects_ba w rs =
  let len = Bigarray.Array1.dim rs in
  if len mod 4 <> 0 then invalid_arg (err_length_mul len 4) else
  let count = len / 4 in
  let rs = to_voidp (bigarray_start array1 rs) in
  update_window_surface_rects w rs count

let update_window_surface_rects w rs =
  let a = CArray.of_list rect rs in
  let rs = to_voidp (CArray.start a) in
  update_window_surface_rects w rs (CArray.length a)

(* OpenGL contexts *)

type gl_context = unit ptr
let gl_context : unit ptr typ = ptr void
let gl_context_opt : unit ptr option typ = ptr_opt void

let unsafe_gl_context_of_ptr addr : gl_context =
  ptr_of_raw_address addr
let unsafe_ptr_of_gl_context gl_context =
  raw_address_of_ptr (to_voidp gl_context)

module Gl = struct
  type context_flags = int
  let context_debug_flag = sdl_gl_context_debug_flag
  let context_forward_compatible_flag = sdl_gl_context_forward_compatible_flag
  let context_robust_access_flag = sdl_gl_context_robust_access_flag
  let context_reset_isolation_flag = sdl_gl_context_reset_isolation_flag

  type profile = int
  let context_profile_core = sdl_gl_context_profile_core
  let context_profile_compatibility = sdl_gl_context_profile_compatibility
  let context_profile_es = sdl_gl_context_profile_es

  type attr = int
  let red_size = sdl_gl_red_size
  let green_size = sdl_gl_green_size
  let blue_size = sdl_gl_blue_size
  let alpha_size = sdl_gl_alpha_size
  let buffer_size = sdl_gl_buffer_size
  let doublebuffer = sdl_gl_doublebuffer
  let depth_size = sdl_gl_depth_size
  let stencil_size = sdl_gl_stencil_size
  let accum_red_size = sdl_gl_accum_red_size
  let accum_green_size = sdl_gl_accum_green_size
  let accum_blue_size = sdl_gl_accum_blue_size
  let accum_alpha_size = sdl_gl_accum_alpha_size
  let stereo = sdl_gl_stereo
  let multisamplebuffers = sdl_gl_multisamplebuffers
  let multisamplesamples = sdl_gl_multisamplesamples
  let accelerated_visual = sdl_gl_accelerated_visual
  let context_major_version = sdl_gl_context_major_version
  let context_minor_version = sdl_gl_context_minor_version
  let context_egl = sdl_gl_context_egl
  let context_flags = sdl_gl_context_flags
  let context_profile_mask = sdl_gl_context_profile_mask
  let context_release_behavior = sdl_gl_context_release_behavior
  let share_with_current_context = sdl_gl_share_with_current_context
  let framebuffer_srgb_capable = sdl_gl_framebuffer_srgb_capable
end

let gl_bind_texture =
  foreign "SDL_GL_BindTexture"
    (texture @-> ptr float @-> ptr float @-> returning zero_to_ok)

let gl_bind_texture t =
  let w = allocate float 0. in
  let h = allocate float 0. in
  match gl_bind_texture t w h with
  | Ok () -> Ok (!@ w, !@ h) | Error _ as e -> e

let gl_create_context =
  foreign "SDL_GL_CreateContext"
    (window @-> returning (some_to_ok gl_context_opt))

let gl_delete_context =
  foreign "SDL_GL_DeleteContext" (gl_context @-> returning void)

let gl_extension_supported =
  foreign "SDL_GL_ExtensionSupported" (string @-> returning bool)

let gl_get_attribute =
  foreign "SDL_GL_GetAttribute" (int @-> (ptr int) @-> returning int)

let gl_get_attribute att =
  let value = allocate int 0 in
  match gl_get_attribute att value with
  | 0 -> Ok (!@ value) | err -> error ()

let gl_get_current_context =
  foreign "SDL_GL_GetCurrentContext"
    (void @-> returning (some_to_ok gl_context_opt))

let gl_get_drawable_size =
  foreign "SDL_GL_GetDrawableSize"
    (window @-> ptr int @-> ptr int @-> returning void)

let gl_get_drawable_size win =
  let w = allocate int 0 in
  let h = allocate int 0 in
  gl_get_drawable_size win w h;
  (!@ w, !@ h)

let int_to_ok =
  let read n = Ok n in
  view ~read ~write:write_never int

let gl_get_swap_interval =
  foreign "SDL_GL_GetSwapInterval" (void @-> returning int_to_ok)

let gl_make_current =
  foreign "SDL_GL_MakeCurrent"
    (window @-> gl_context @-> returning zero_to_ok)

let gl_reset_attributes =
  foreign "SDL_GL_ResetAttributes" ~stub (void @-> returning void)

let gl_set_attribute =
  foreign "SDL_GL_SetAttribute" (int @-> int @-> returning zero_to_ok)

let gl_set_swap_interval =
  foreign "SDL_GL_SetSwapInterval" (int @-> returning zero_to_ok)

let gl_swap_window =
  foreign "SDL_GL_SwapWindow" (window @-> returning void)

let gl_unbind_texture =
  foreign "SDL_GL_UnbindTexture" (texture @-> returning zero_to_ok)

(* Vulkan *)

module Vulkan = struct

  type instance = unit ptr
  let instance = ptr void
  let unsafe_ptr_of_instance = raw_address_of_ptr
  let unsafe_instance_of_ptr x = ptr_of_raw_address x

  type surface = uint64
  let surface = int64_t
  let unsafe_uint64_of_surface x = x
  let unsafe_surface_of_uint64 x = x

  let load_library =
    foreign "SDL_Vulkan_LoadLibrary" (string_opt @-> returning zero_to_ok)

  let unload_library =
    foreign "SDL_Vulkan_UnloadLibrary" (void @-> returning void)

  let get_instance_extensions =
    foreign "SDL_Vulkan_GetInstanceExtensions"
      (window @-> ptr int @-> ptr string @-> returning bool)

  let get_instance_extensions window =
    let n = allocate int 0 in
    match get_instance_extensions window n
            (Ctypes.coerce (ptr void) (ptr string) null) with
    | false -> None
    | true ->
        let exts = allocate_n string ~count:(!@n) in
        match get_instance_extensions window n exts with
        | false -> None
        | true -> Some CArray.(to_list @@ from_ptr exts (!@n))

  let create_surface =
    foreign "SDL_Vulkan_CreateSurface"
      (window @-> instance @-> ptr surface @-> returning bool)

  let create_surface window instance =
    let s = allocate_n surface ~count:1 in
    if create_surface window instance s then
      Some !@s
    else
    None

  let get_drawable_size =
    foreign "SDL_Vulkan_GetDrawableSize"
      (window @-> ptr int @-> ptr int @-> returning void)

  let get_drawable_size window =
    let w = allocate int 0 in
    let h = allocate int 0 in
    get_drawable_size window w h;
    !@w, !@h
end

(* Screen saver *)

let disable_screen_saver =
  foreign "SDL_DisableScreenSaver" (void @-> returning void)

let enable_screen_saver =
  foreign "SDL_EnableScreenSaver" (void @-> returning void)

let is_screen_saver_enabled =
  foreign "SDL_IsScreenSaverEnabled" (void @-> returning bool)

(* Message boxes *)

module Message_box = struct
  let i = Unsigned.UInt32.of_int

  type button_flags = Unsigned.uint32
  let button_no_default = i 0
  let button_returnkey_default = i sdl_messagebox_button_returnkey_default
  let button_escapekey_default = i sdl_messagebox_button_escapekey_default

  type button_data =
    { button_flags : button_flags;
      button_id : int;
      button_text : string }

  let button_data = structure "SDL_MessageBoxButtonData"
  let button_flags = field button_data "flags" uint32_t
  let button_buttonid = field button_data "buttonid" int
  let button_text = field button_data "text" string
  let () = seal button_data

  type flags = Unsigned.uint32
  let error = i sdl_messagebox_error
  let warning = i sdl_messagebox_warning
  let information = i sdl_messagebox_information

  type color = int * int * int
  let color = structure "SDL_MessageBoxColor"
  let color_r = field color "r" uint8_t
  let color_g = field color "g" uint8_t
  let color_b = field color "b" uint8_t
  let () = seal color

  let color_background = sdl_messagebox_color_background
  let color_text = sdl_messagebox_color_text
  let color_button_border = sdl_messagebox_color_button_border
  let color_button_background = sdl_messagebox_color_button_background
  let color_button_selected = sdl_messagebox_color_button_selected
  let color_button_max = sdl_messagebox_color_max

  type color_scheme =
    { color_background : color;
      color_text : color;
      color_button_border : color;
      color_button_background : color;
      color_button_selected : color; }

  let color_scheme = structure "SDL_MessageBoxColorScheme"
  let colors = field color_scheme "colors" (array color_button_max color)
  let () = seal color_scheme

  type data =
    { flags : flags;
      window : window option;
      title : string;
      message : string;
      buttons : button_data list;
      color_scheme : color_scheme option }

  let data = structure "SDL_MessageBoxData"
  let d_flags = field data "flags" uint32_t
  let d_window = field data "window" window
  let d_title = field data "title" string
  let d_message = field data "message" string
  let d_numbuttons = field data "numbuttons" int
  let d_buttons = field data "buttons" (ptr button_data)
  let d_color_scheme = field data "colorScheme" (ptr color_scheme)
  let () = seal data

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
    let colors = getf st colors in
    let set i (rv, gv, bv) =
      let ct = CArray.get colors i in
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
    setf dt d_window (match d.window with None -> null | Some w -> w);
    setf dt d_title d.title;
    setf dt d_message d.message;
    setf dt d_numbuttons (List.length d.buttons);
    setf dt d_buttons (buttons_to_c d.buttons);
    setf dt d_color_scheme
      begin match d.color_scheme with
      | None -> coerce (ptr void) (ptr color_scheme) null
      | Some s -> addr (color_scheme_to_c s)
      end;
    dt
end

let show_message_box =
  foreign "SDL_ShowMessageBox"
    (ptr Message_box.data @-> ptr int @-> returning zero_to_ok)

let show_message_box d =
  let d = addr (Message_box.data_to_c d) in
  let ret = allocate int 0 in
  match show_message_box d ret with
  | Ok () -> Ok (!@ ret) | Error _ as e -> e

let show_simple_message_box =
  foreign "SDL_ShowSimpleMessageBox"
    (uint32_t @-> string @-> string @-> window_opt @-> returning zero_to_ok)

let show_simple_message_box t ~title msg w =
  show_simple_message_box t title msg w

(* Clipboard *)

let get_clipboard_text =
  foreign "SDL_GetClipboardText" (void @-> returning (ptr char))

let get_clipboard_text () =
  let p = get_clipboard_text () in
  if (to_voidp p) = null then error () else
  let b = Buffer.create 255 in
  let ptr = ref p in
  while (!@ !ptr) <> '\000' do
    Buffer.add_char b (!@ !ptr);
    ptr := !ptr +@ 1;
  done;
  sdl_free (to_voidp p);
  Ok (Buffer.contents b)

let has_clipboard_text =
  foreign "SDL_HasClipboardText" (void @-> returning bool)

let set_clipboard_text =
  foreign "SDL_SetClipboardText" (string @-> returning zero_to_ok)

(* Input *)

type button_state = uint8
let pressed = sdl_pressed
let released = sdl_released

type toggle_state = uint8
let disable = sdl_disable
let enable = sdl_enable

(* Keyboard *)

type scancode = int
let scancode = int

module Scancode = struct
  let num_scancodes = sdl_num_scancodes
  let unknown = sdl_scancode_unknown
  let a = sdl_scancode_a
  let b = sdl_scancode_b
  let c = sdl_scancode_c
  let d = sdl_scancode_d
  let e = sdl_scancode_e
  let f = sdl_scancode_f
  let g = sdl_scancode_g
  let h = sdl_scancode_h
  let i = sdl_scancode_i
  let j = sdl_scancode_j
  let k = sdl_scancode_k
  let l = sdl_scancode_l
  let m = sdl_scancode_m
  let n = sdl_scancode_n
  let o = sdl_scancode_o
  let p = sdl_scancode_p
  let q = sdl_scancode_q
  let r = sdl_scancode_r
  let s = sdl_scancode_s
  let t = sdl_scancode_t
  let u = sdl_scancode_u
  let v = sdl_scancode_v
  let w = sdl_scancode_w
  let x = sdl_scancode_x
  let y = sdl_scancode_y
  let z = sdl_scancode_z
  let k1 = sdl_scancode_1
  let k2 = sdl_scancode_2
  let k3 = sdl_scancode_3
  let k4 = sdl_scancode_4
  let k5 = sdl_scancode_5
  let k6 = sdl_scancode_6
  let k7 = sdl_scancode_7
  let k8 = sdl_scancode_8
  let k9 = sdl_scancode_9
  let k0 = sdl_scancode_0
  let return = sdl_scancode_return
  let escape = sdl_scancode_escape
  let backspace = sdl_scancode_backspace
  let tab = sdl_scancode_tab
  let space = sdl_scancode_space
  let minus = sdl_scancode_minus
  let equals = sdl_scancode_equals
  let leftbracket = sdl_scancode_leftbracket
  let rightbracket = sdl_scancode_rightbracket
  let backslash = sdl_scancode_backslash
  let nonushash = sdl_scancode_nonushash
  let semicolon = sdl_scancode_semicolon
  let apostrophe = sdl_scancode_apostrophe
  let grave = sdl_scancode_grave
  let comma = sdl_scancode_comma
  let period = sdl_scancode_period
  let slash = sdl_scancode_slash
  let capslock = sdl_scancode_capslock
  let f1 = sdl_scancode_f1
  let f2 = sdl_scancode_f2
  let f3 = sdl_scancode_f3
  let f4 = sdl_scancode_f4
  let f5 = sdl_scancode_f5
  let f6 = sdl_scancode_f6
  let f7 = sdl_scancode_f7
  let f8 = sdl_scancode_f8
  let f9 = sdl_scancode_f9
  let f10 = sdl_scancode_f10
  let f11 = sdl_scancode_f11
  let f12 = sdl_scancode_f12
  let printscreen = sdl_scancode_printscreen
  let scrolllock = sdl_scancode_scrolllock
  let pause = sdl_scancode_pause
  let insert = sdl_scancode_insert
  let home = sdl_scancode_home
  let pageup = sdl_scancode_pageup
  let delete = sdl_scancode_delete
  let kend = sdl_scancode_end
  let pagedown = sdl_scancode_pagedown
  let right = sdl_scancode_right
  let left = sdl_scancode_left
  let down = sdl_scancode_down
  let up = sdl_scancode_up
  let numlockclear = sdl_scancode_numlockclear
  let kp_divide = sdl_scancode_kp_divide
  let kp_multiply = sdl_scancode_kp_multiply
  let kp_minus = sdl_scancode_kp_minus
  let kp_plus = sdl_scancode_kp_plus
  let kp_enter = sdl_scancode_kp_enter
  let kp_1 = sdl_scancode_kp_1
  let kp_2 = sdl_scancode_kp_2
  let kp_3 = sdl_scancode_kp_3
  let kp_4 = sdl_scancode_kp_4
  let kp_5 = sdl_scancode_kp_5
  let kp_6 = sdl_scancode_kp_6
  let kp_7 = sdl_scancode_kp_7
  let kp_8 = sdl_scancode_kp_8
  let kp_9 = sdl_scancode_kp_9
  let kp_0 = sdl_scancode_kp_0
  let kp_period = sdl_scancode_kp_period
  let nonusbackslash = sdl_scancode_nonusbackslash
  let application = sdl_scancode_application
  let kp_equals = sdl_scancode_kp_equals
  let f13 = sdl_scancode_f13
  let f14 = sdl_scancode_f14
  let f15 = sdl_scancode_f15
  let f16 = sdl_scancode_f16
  let f17 = sdl_scancode_f17
  let f18 = sdl_scancode_f18
  let f19 = sdl_scancode_f19
  let f20 = sdl_scancode_f20
  let f21 = sdl_scancode_f21
  let f22 = sdl_scancode_f22
  let f23 = sdl_scancode_f23
  let f24 = sdl_scancode_f24
  let execute = sdl_scancode_execute
  let help = sdl_scancode_help
  let menu = sdl_scancode_menu
  let select = sdl_scancode_select
  let stop = sdl_scancode_stop
  let again = sdl_scancode_again
  let undo = sdl_scancode_undo
  let cut = sdl_scancode_cut
  let copy = sdl_scancode_copy
  let paste = sdl_scancode_paste
  let find = sdl_scancode_find
  let mute = sdl_scancode_mute
  let volumeup = sdl_scancode_volumeup
  let volumedown = sdl_scancode_volumedown
  let kp_comma = sdl_scancode_kp_comma
  let kp_equalsas400 = sdl_scancode_kp_equalsas400
  let international1 = sdl_scancode_international1
  let international2 = sdl_scancode_international2
  let international3 = sdl_scancode_international3
  let international4 = sdl_scancode_international4
  let international5 = sdl_scancode_international5
  let international6 = sdl_scancode_international6
  let international7 = sdl_scancode_international7
  let international8 = sdl_scancode_international8
  let international9 = sdl_scancode_international9
  let lang1 = sdl_scancode_lang1
  let lang2 = sdl_scancode_lang2
  let lang3 = sdl_scancode_lang3
  let lang4 = sdl_scancode_lang4
  let lang5 = sdl_scancode_lang5
  let lang6 = sdl_scancode_lang6
  let lang7 = sdl_scancode_lang7
  let lang8 = sdl_scancode_lang8
  let lang9 = sdl_scancode_lang9
  let alterase = sdl_scancode_alterase
  let sysreq = sdl_scancode_sysreq
  let cancel = sdl_scancode_cancel
  let clear = sdl_scancode_clear
  let prior = sdl_scancode_prior
  let return2 = sdl_scancode_return2
  let separator = sdl_scancode_separator
  let out = sdl_scancode_out
  let oper = sdl_scancode_oper
  let clearagain = sdl_scancode_clearagain
  let crsel = sdl_scancode_crsel
  let exsel = sdl_scancode_exsel
  let kp_00 = sdl_scancode_kp_00
  let kp_000 = sdl_scancode_kp_000
  let thousandsseparator = sdl_scancode_thousandsseparator
  let decimalseparator = sdl_scancode_decimalseparator
  let currencyunit = sdl_scancode_currencyunit
  let currencysubunit = sdl_scancode_currencysubunit
  let kp_leftparen = sdl_scancode_kp_leftparen
  let kp_rightparen = sdl_scancode_kp_rightparen
  let kp_leftbrace = sdl_scancode_kp_leftbrace
  let kp_rightbrace = sdl_scancode_kp_rightbrace
  let kp_tab = sdl_scancode_kp_tab
  let kp_backspace = sdl_scancode_kp_backspace
  let kp_a = sdl_scancode_kp_a
  let kp_b = sdl_scancode_kp_b
  let kp_c = sdl_scancode_kp_c
  let kp_d = sdl_scancode_kp_d
  let kp_e = sdl_scancode_kp_e
  let kp_f = sdl_scancode_kp_f
  let kp_xor = sdl_scancode_kp_xor
  let kp_power = sdl_scancode_kp_power
  let kp_percent = sdl_scancode_kp_percent
  let kp_less = sdl_scancode_kp_less
  let kp_greater = sdl_scancode_kp_greater
  let kp_ampersand = sdl_scancode_kp_ampersand
  let kp_dblampersand = sdl_scancode_kp_dblampersand
  let kp_verticalbar = sdl_scancode_kp_verticalbar
  let kp_dblverticalbar = sdl_scancode_kp_dblverticalbar
  let kp_colon = sdl_scancode_kp_colon
  let kp_hash = sdl_scancode_kp_hash
  let kp_space = sdl_scancode_kp_space
  let kp_at = sdl_scancode_kp_at
  let kp_exclam = sdl_scancode_kp_exclam
  let kp_memstore = sdl_scancode_kp_memstore
  let kp_memrecall = sdl_scancode_kp_memrecall
  let kp_memclear = sdl_scancode_kp_memclear
  let kp_memadd = sdl_scancode_kp_memadd
  let kp_memsubtract = sdl_scancode_kp_memsubtract
  let kp_memmultiply = sdl_scancode_kp_memmultiply
  let kp_memdivide = sdl_scancode_kp_memdivide
  let kp_plusminus = sdl_scancode_kp_plusminus
  let kp_clear = sdl_scancode_kp_clear
  let kp_clearentry = sdl_scancode_kp_clearentry
  let kp_binary = sdl_scancode_kp_binary
  let kp_octal = sdl_scancode_kp_octal
  let kp_decimal = sdl_scancode_kp_decimal
  let kp_hexadecimal = sdl_scancode_kp_hexadecimal
  let lctrl = sdl_scancode_lctrl
  let lshift = sdl_scancode_lshift
  let lalt = sdl_scancode_lalt
  let lgui = sdl_scancode_lgui
  let rctrl = sdl_scancode_rctrl
  let rshift = sdl_scancode_rshift
  let ralt = sdl_scancode_ralt
  let rgui = sdl_scancode_rgui
  let mode = sdl_scancode_mode
  let audionext = sdl_scancode_audionext
  let audioprev = sdl_scancode_audioprev
  let audiostop = sdl_scancode_audiostop
  let audioplay = sdl_scancode_audioplay
  let audiomute = sdl_scancode_audiomute
  let mediaselect = sdl_scancode_mediaselect
  let www = sdl_scancode_www
  let mail = sdl_scancode_mail
  let calculator = sdl_scancode_calculator
  let computer = sdl_scancode_computer
  let ac_search = sdl_scancode_ac_search
  let ac_home = sdl_scancode_ac_home
  let ac_back = sdl_scancode_ac_back
  let ac_forward = sdl_scancode_ac_forward
  let ac_stop = sdl_scancode_ac_stop
  let ac_refresh = sdl_scancode_ac_refresh
  let ac_bookmarks = sdl_scancode_ac_bookmarks
  let brightnessdown = sdl_scancode_brightnessdown
  let brightnessup = sdl_scancode_brightnessup
  let displayswitch = sdl_scancode_displayswitch
  let kbdillumtoggle = sdl_scancode_kbdillumtoggle
  let kbdillumdown = sdl_scancode_kbdillumdown
  let kbdillumup = sdl_scancode_kbdillumup
  let eject = sdl_scancode_eject
  let sleep = sdl_scancode_sleep
  let app1 = sdl_scancode_app1
  let app2 = sdl_scancode_app2

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
let keycode = int

module K = struct
  let scancode_mask = sdlk_scancode_mask
  let unknown = sdlk_unknown
  let return = sdlk_return
  let escape = sdlk_escape
  let backspace = sdlk_backspace
  let tab = sdlk_tab
  let space = sdlk_space
  let exclaim = sdlk_exclaim
  let quotedbl = sdlk_quotedbl
  let hash = sdlk_hash
  let percent = sdlk_percent
  let dollar = sdlk_dollar
  let ampersand = sdlk_ampersand
  let quote = sdlk_quote
  let leftparen = sdlk_leftparen
  let rightparen = sdlk_rightparen
  let asterisk = sdlk_asterisk
  let plus = sdlk_plus
  let comma = sdlk_comma
  let minus = sdlk_minus
  let period = sdlk_period
  let slash = sdlk_slash
  let k0 = sdlk_0
  let k1 = sdlk_1
  let k2 = sdlk_2
  let k3 = sdlk_3
  let k4 = sdlk_4
  let k5 = sdlk_5
  let k6 = sdlk_6
  let k7 = sdlk_7
  let k8 = sdlk_8
  let k9 = sdlk_9
  let colon = sdlk_colon
  let semicolon = sdlk_semicolon
  let less = sdlk_less
  let equals = sdlk_equals
  let greater = sdlk_greater
  let question = sdlk_question
  let at = sdlk_at
  let leftbracket = sdlk_leftbracket
  let backslash = sdlk_backslash
  let rightbracket = sdlk_rightbracket
  let caret = sdlk_caret
  let underscore = sdlk_underscore
  let backquote = sdlk_backquote
  let a = sdlk_a
  let b = sdlk_b
  let c = sdlk_c
  let d = sdlk_d
  let e = sdlk_e
  let f = sdlk_f
  let g = sdlk_g
  let h = sdlk_h
  let i = sdlk_i
  let j = sdlk_j
  let k = sdlk_k
  let l = sdlk_l
  let m = sdlk_m
  let n = sdlk_n
  let o = sdlk_o
  let p = sdlk_p
  let q = sdlk_q
  let r = sdlk_r
  let s = sdlk_s
  let t = sdlk_t
  let u = sdlk_u
  let v = sdlk_v
  let w = sdlk_w
  let x = sdlk_x
  let y = sdlk_y
  let z = sdlk_z
  let capslock = sdlk_capslock
  let f1 = sdlk_f1
  let f2 = sdlk_f2
  let f3 = sdlk_f3
  let f4 = sdlk_f4
  let f5 = sdlk_f5
  let f6 = sdlk_f6
  let f7 = sdlk_f7
  let f8 = sdlk_f8
  let f9 = sdlk_f9
  let f10 = sdlk_f10
  let f11 = sdlk_f11
  let f12 = sdlk_f12
  let printscreen = sdlk_printscreen
  let scrolllock = sdlk_scrolllock
  let pause = sdlk_pause
  let insert = sdlk_insert
  let home = sdlk_home
  let pageup = sdlk_pageup
  let delete = sdlk_delete
  let kend = sdlk_end
  let pagedown = sdlk_pagedown
  let right = sdlk_right
  let left = sdlk_left
  let down = sdlk_down
  let up = sdlk_up
  let numlockclear = sdlk_numlockclear
  let kp_divide = sdlk_kp_divide
  let kp_multiply = sdlk_kp_multiply
  let kp_minus = sdlk_kp_minus
  let kp_plus = sdlk_kp_plus
  let kp_enter = sdlk_kp_enter
  let kp_1 = sdlk_kp_1
  let kp_2 = sdlk_kp_2
  let kp_3 = sdlk_kp_3
  let kp_4 = sdlk_kp_4
  let kp_5 = sdlk_kp_5
  let kp_6 = sdlk_kp_6
  let kp_7 = sdlk_kp_7
  let kp_8 = sdlk_kp_8
  let kp_9 = sdlk_kp_9
  let kp_0 = sdlk_kp_0
  let kp_period = sdlk_kp_period
  let application = sdlk_application
  let power = sdlk_power
  let kp_equals = sdlk_kp_equals
  let f13 = sdlk_f13
  let f14 = sdlk_f14
  let f15 = sdlk_f15
  let f16 = sdlk_f16
  let f17 = sdlk_f17
  let f18 = sdlk_f18
  let f19 = sdlk_f19
  let f20 = sdlk_f20
  let f21 = sdlk_f21
  let f22 = sdlk_f22
  let f23 = sdlk_f23
  let f24 = sdlk_f24
  let execute = sdlk_execute
  let help = sdlk_help
  let menu = sdlk_menu
  let select = sdlk_select
  let stop = sdlk_stop
  let again = sdlk_again
  let undo = sdlk_undo
  let cut = sdlk_cut
  let copy = sdlk_copy
  let paste = sdlk_paste
  let find = sdlk_find
  let mute = sdlk_mute
  let volumeup = sdlk_volumeup
  let volumedown = sdlk_volumedown
  let kp_comma = sdlk_kp_comma
  let kp_equalsas400 = sdlk_kp_equalsas400
  let alterase = sdlk_alterase
  let sysreq = sdlk_sysreq
  let cancel = sdlk_cancel
  let clear = sdlk_clear
  let prior = sdlk_prior
  let return2 = sdlk_return2
  let separator = sdlk_separator
  let out = sdlk_out
  let oper = sdlk_oper
  let clearagain = sdlk_clearagain
  let crsel = sdlk_crsel
  let exsel = sdlk_exsel
  let kp_00 = sdlk_kp_00
  let kp_000 = sdlk_kp_000
  let thousandsseparator = sdlk_thousandsseparator
  let decimalseparator = sdlk_decimalseparator
  let currencyunit = sdlk_currencyunit
  let currencysubunit = sdlk_currencysubunit
  let kp_leftparen = sdlk_kp_leftparen
  let kp_rightparen = sdlk_kp_rightparen
  let kp_leftbrace = sdlk_kp_leftbrace
  let kp_rightbrace = sdlk_kp_rightbrace
  let kp_tab = sdlk_kp_tab
  let kp_backspace = sdlk_kp_backspace
  let kp_a = sdlk_kp_a
  let kp_b = sdlk_kp_b
  let kp_c = sdlk_kp_c
  let kp_d = sdlk_kp_d
  let kp_e = sdlk_kp_e
  let kp_f = sdlk_kp_f
  let kp_xor = sdlk_kp_xor
  let kp_power = sdlk_kp_power
  let kp_percent = sdlk_kp_percent
  let kp_less = sdlk_kp_less
  let kp_greater = sdlk_kp_greater
  let kp_ampersand = sdlk_kp_ampersand
  let kp_dblampersand = sdlk_kp_dblampersand
  let kp_verticalbar = sdlk_kp_verticalbar
  let kp_dblverticalbar = sdlk_kp_dblverticalbar
  let kp_colon = sdlk_kp_colon
  let kp_hash = sdlk_kp_hash
  let kp_space = sdlk_kp_space
  let kp_at = sdlk_kp_at
  let kp_exclam = sdlk_kp_exclam
  let kp_memstore = sdlk_kp_memstore
  let kp_memrecall = sdlk_kp_memrecall
  let kp_memclear = sdlk_kp_memclear
  let kp_memadd = sdlk_kp_memadd
  let kp_memsubtract = sdlk_kp_memsubtract
  let kp_memmultiply = sdlk_kp_memmultiply
  let kp_memdivide = sdlk_kp_memdivide
  let kp_plusminus = sdlk_kp_plusminus
  let kp_clear = sdlk_kp_clear
  let kp_clearentry = sdlk_kp_clearentry
  let kp_binary = sdlk_kp_binary
  let kp_octal = sdlk_kp_octal
  let kp_decimal = sdlk_kp_decimal
  let kp_hexadecimal = sdlk_kp_hexadecimal
  let lctrl = sdlk_lctrl
  let lshift = sdlk_lshift
  let lalt = sdlk_lalt
  let lgui = sdlk_lgui
  let rctrl = sdlk_rctrl
  let rshift = sdlk_rshift
  let ralt = sdlk_ralt
  let rgui = sdlk_rgui
  let mode = sdlk_mode
  let audionext = sdlk_audionext
  let audioprev = sdlk_audioprev
  let audiostop = sdlk_audiostop
  let audioplay = sdlk_audioplay
  let audiomute = sdlk_audiomute
  let mediaselect = sdlk_mediaselect
  let www = sdlk_www
  let mail = sdlk_mail
  let calculator = sdlk_calculator
  let computer = sdlk_computer
  let ac_search = sdlk_ac_search
  let ac_home = sdlk_ac_home
  let ac_back = sdlk_ac_back
  let ac_forward = sdlk_ac_forward
  let ac_stop = sdlk_ac_stop
  let ac_refresh = sdlk_ac_refresh
  let ac_bookmarks = sdlk_ac_bookmarks
  let brightnessdown = sdlk_brightnessdown
  let brightnessup = sdlk_brightnessup
  let displayswitch = sdlk_displayswitch
  let kbdillumtoggle = sdlk_kbdillumtoggle
  let kbdillumdown = sdlk_kbdillumdown
  let kbdillumup = sdlk_kbdillumup
  let eject = sdlk_eject
  let sleep = sdlk_sleep
end

type keymod = int
let keymod = int_as_uint16_t

module Kmod = struct
  let none = kmod_none
  let lshift = kmod_lshift
  let rshift = kmod_rshift
  let lctrl = kmod_lctrl
  let rctrl = kmod_rctrl
  let lalt = kmod_lalt
  let ralt = kmod_ralt
  let lgui = kmod_lgui
  let rgui = kmod_rgui
  let num = kmod_num
  let caps = kmod_caps
  let mode = kmod_mode
  let reserved = kmod_reserved
  let ctrl = kmod_ctrl
  let shift = kmod_shift
  let alt = kmod_alt
  let gui = kmod_gui
end

let get_keyboard_focus =
  foreign "SDL_GetKeyboardFocus" (void @-> returning window_opt)

let get_keyboard_state =
  foreign "SDL_GetKeyboardState" (ptr int @-> returning (ptr int))

let get_keyboard_state () =
  let count = allocate int 0 in
  let p = get_keyboard_state count in
  bigarray_of_ptr array1 (!@ count) Bigarray.int8_unsigned p

let get_key_from_name =
  foreign "SDL_GetKeyFromName" (string @-> returning keycode)

let get_key_from_scancode =
  foreign "SDL_GetKeyFromScancode" (scancode @-> returning keycode)

let get_key_name =
  foreign "SDL_GetKeyName" (keycode @-> returning string)

let get_mod_state =
  foreign "SDL_GetModState" (void @-> returning keymod)

let get_scancode_from_key =
  foreign "SDL_GetScancodeFromKey" (keycode @-> returning scancode)

let get_scancode_from_name =
  foreign "SDL_GetScancodeFromName" (string @-> returning scancode)

let get_scancode_name =
  foreign "SDL_GetScancodeName" (scancode @-> returning string)

let has_screen_keyboard_support =
  foreign "SDL_HasScreenKeyboardSupport" (void @-> returning bool)

let is_screen_keyboard_shown =
  foreign "SDL_IsScreenKeyboardShown" (window @-> returning bool)

let is_text_input_active =
  foreign "SDL_IsTextInputActive" (void @-> returning bool)

let set_mod_state =
  foreign "SDL_SetModState" (keymod @-> returning void)

let set_text_input_rect =
  foreign "SDL_SetTextInputRect" (ptr rect @-> returning void)

let set_text_input_rect r =
  set_text_input_rect (Rect.opt_addr r)

let start_text_input =
  foreign "SDL_StartTextInput" (void @-> returning void)

let stop_text_input =
  foreign "SDL_StopTextInput" (void @-> returning void)

(* Mouse *)

type cursor = unit ptr
let cursor : cursor typ = ptr void
let cursor_opt : cursor option typ = ptr_opt void

let unsafe_cursor_of_ptr addr : cursor =
  ptr_of_raw_address addr
let unsafe_ptr_of_cursor cursor =
  raw_address_of_ptr (to_voidp cursor)

module System_cursor = struct
  type t = int
  let arrow = sdl_system_cursor_arrow
  let ibeam = sdl_system_cursor_ibeam
  let wait = sdl_system_cursor_wait
  let crosshair = sdl_system_cursor_crosshair
  let waitarrow = sdl_system_cursor_waitarrow
  let size_nw_se = sdl_system_cursor_sizenwse
  let size_ne_sw = sdl_system_cursor_sizenesw
  let size_we = sdl_system_cursor_sizewe
  let size_ns = sdl_system_cursor_sizens
  let size_all = sdl_system_cursor_sizeall
  let no = sdl_system_cursor_no
  let hand = sdl_system_cursor_hand
end

module Button = struct
  let left = sdl_button_left
  let right = sdl_button_right
  let middle = sdl_button_middle
  let x1 = sdl_button_x1
  let x2 = sdl_button_x2

  let i = Int32.of_int
  let lmask = i sdl_button_lmask
  let mmask = i sdl_button_mmask
  let rmask = i sdl_button_rmask
  let x1mask = i sdl_button_x1mask
  let x2mask = i sdl_button_x2mask
end

let capture_mouse =
  foreign "SDL_CaptureMouse" (bool @-> returning zero_to_ok)

let create_color_cursor =
  foreign "SDL_CreateColorCursor"
    (surface @-> int @-> int @-> returning (some_to_ok cursor_opt))

let create_color_cursor s ~hot_x ~hot_y =
  create_color_cursor s hot_x hot_y

let create_cursor =
  foreign "SDL_CreateCursor"
    (ptr void @-> ptr void @-> int @-> int @-> int @-> int @->
     returning (some_to_ok cursor_opt))

let create_cursor d m ~w ~h ~hot_x ~hot_y =
  (* FIXME: we could try to check bounds *)
  let d = to_voidp (bigarray_start array1 d) in
  let m = to_voidp (bigarray_start array1 m) in
  create_cursor d m w h hot_x hot_y

let create_system_cursor =
  foreign "SDL_CreateSystemCursor"
    (int @-> returning (some_to_ok cursor_opt))

let free_cursor =
  foreign "SDL_FreeCursor" (cursor @-> returning void)

let get_cursor =
  foreign "SDL_GetCursor" (void @-> returning cursor_opt)

let get_default_cursor =
  foreign "SDL_GetDefaultCursor" (void @-> returning cursor_opt)

let get_global_mouse_state =
  foreign "SDL_GetGlobalMouseState"
    (ptr int @-> ptr int @-> returning int32_as_uint32_t)

let get_global_mouse_state () =
  let x = allocate int 0 in
  let y = allocate int 0 in
  let s = get_global_mouse_state x y in
  s, (!@ x, !@ y)

let get_mouse_focus =
  foreign "SDL_GetMouseFocus" (void @-> returning window_opt)

let get_mouse_state =
  foreign "SDL_GetMouseState"
    (ptr int @-> ptr int @-> returning int32_as_uint32_t)

let get_mouse_state () =
  let x = allocate int 0 in
  let y = allocate int 0 in
  let s = get_mouse_state x y in
  s, (!@ x, !@ y)

let get_relative_mouse_mode =
  foreign "SDL_GetRelativeMouseMode" (void @-> returning bool)

let get_relative_mouse_state =
  foreign "SDL_GetRelativeMouseState"
    (ptr int @-> ptr int @-> returning int32_as_uint32_t)

let get_relative_mouse_state () =
  let x = allocate int 0 in
  let y = allocate int 0 in
  let s = get_relative_mouse_state x y in
  s, (!@ x, !@ y)

let show_cursor =
  foreign "SDL_ShowCursor" (int @-> returning bool_to_ok)

let get_cursor_shown () =
  show_cursor (-1)

let set_cursor =
  foreign "SDL_SetCursor" (cursor_opt @-> returning void)

let set_relative_mouse_mode =
  foreign "SDL_SetRelativeMouseMode" (bool @-> returning zero_to_ok)

let show_cursor b =
  show_cursor (if b then 1 else 0)

let warp_mouse_in_window =
  foreign "SDL_WarpMouseInWindow"
    (window_opt @-> int @-> int @-> returning void)

let warp_mouse_in_window w ~x ~y =
  warp_mouse_in_window w x y

let warp_mouse_global=
  foreign "SDL_WarpMouseGlobal" (int @-> int @-> returning zero_to_ok)

let warp_mouse_global ~x ~y =
  warp_mouse_global x y

(* Touch *)

type touch_id = int64
let touch_id = int64_t
let touch_mouse_id = Int64.of_int32 (sdl_touch_mouseid)

type gesture_id = int64
let gesture_id = int64_t

type finger_id = int64
let finger_id = int64_t

type _finger
type finger = _finger structure
let finger : finger typ = structure "SDL_Finger"
let finger_finger_id = field finger "id" finger_id
let finger_x = field finger "x" float
let finger_y = field finger "y" float
let finger_pressure = field finger "pressure" float
let () = seal finger

module Finger = struct
  let id f = getf f finger_finger_id
  let x f = getf f finger_x
  let y f = getf f finger_y
  let pressure f = getf f finger_pressure
end

let get_num_touch_devices =
  foreign "SDL_GetNumTouchDevices" (void @-> returning int)

let get_num_touch_fingers =
  foreign "SDL_GetNumTouchFingers" (touch_id @-> returning int)

let get_touch_device =
  foreign "SDL_GetTouchDevice" (int @-> returning touch_id)

let get_touch_device i =
  match get_touch_device i with
  | 0L -> error () | id -> Ok id

let get_touch_finger =
  foreign "SDL_GetTouchFinger"
    (touch_id @-> int @-> returning (ptr_opt finger))

let get_touch_finger id i =
  match get_touch_finger id i with
  | None -> None | Some p -> Some (!@ p)

let load_dollar_templates =
  foreign "SDL_LoadDollarTemplates"
    (touch_id @-> rw_ops @-> returning zero_to_ok)

let record_gesture =
  foreign "SDL_RecordGesture" (touch_id @-> returning one_to_ok)

let save_dollar_template =
  foreign "SDL_SaveDollarTemplate"
    (gesture_id @-> rw_ops @-> returning zero_to_ok)

let save_all_dollar_templates =
  foreign "SDL_SaveAllDollarTemplates" (rw_ops @-> returning zero_to_ok)

(* Joystick *)

type _joystick_guid
type joystick_guid = _joystick_guid structure
let joystick_guid : joystick_guid typ = structure "SDL_JoystickGUID"
(* FIXME: No array here, see
   https://github.com/ocamllabs/ocaml-ctypes/issues/113 *)
(* let _= field joystick_guid "data" (array 16 uint8_t) *)
let _= field joystick_guid "data0" uint8_t
let _= field joystick_guid "data1" uint8_t
let _= field joystick_guid "data2" uint8_t
let _= field joystick_guid "data3" uint8_t
let _= field joystick_guid "data4" uint8_t
let _= field joystick_guid "data5" uint8_t
let _= field joystick_guid "data6" uint8_t
let _= field joystick_guid "data7" uint8_t
let _= field joystick_guid "data8" uint8_t
let _= field joystick_guid "data9" uint8_t
let _= field joystick_guid "data10" uint8_t
let _= field joystick_guid "data11" uint8_t
let _= field joystick_guid "data12" uint8_t
let _= field joystick_guid "data13" uint8_t
let _= field joystick_guid "data14" uint8_t
let _= field joystick_guid "data15" uint8_t
let () = seal joystick_guid

type joystick_id = int32
let joystick_id = int32_t

type joystick = unit ptr
let joystick : joystick typ = ptr void
let joystick_opt : joystick option typ = ptr_opt void

let unsafe_joystick_of_ptr addr : joystick =
  ptr_of_raw_address addr
let unsafe_ptr_of_joystick joystick =
  raw_address_of_ptr (to_voidp joystick)

module Hat = struct
  type t = int
  let centered = sdl_hat_centered
  let up = sdl_hat_up
  let right = sdl_hat_right
  let down = sdl_hat_down
  let left = sdl_hat_left
  let rightup = sdl_hat_rightup
  let rightdown = sdl_hat_rightdown
  let leftup = sdl_hat_leftup
  let leftdown = sdl_hat_leftdown
end

module Joystick_power_level = struct
  type t = int
  let unknown = sdl_joystick_power_unknown
  let low = sdl_joystick_power_low
  let medium = sdl_joystick_power_medium
  let full = sdl_joystick_power_full
  let wired = sdl_joystick_power_wired
  let max = sdl_joystick_power_max
end

module Joystick_type = struct
  type t = int
  let unknown        = sdl_joystick_type_unknown
  let gamecontroller = sdl_joystick_type_gamecontroller
  let wheel          = sdl_joystick_type_wheel
  let arcade_stick   = sdl_joystick_type_arcade_stick
  let flight_stick   = sdl_joystick_type_flight_stick
  let dance_pad      = sdl_joystick_type_dance_pad
  let guitar         = sdl_joystick_type_guitar
  let drum_kit       = sdl_joystick_type_drum_kit
  let arcade_pad     = sdl_joystick_type_arcade_pad
  let throttle      = sdl_joystick_type_throttle
end

let joystick_close =
  foreign "SDL_JoystickClose" (joystick @-> returning void)

let joystick_current_power_level =
  foreign "SDL_JoystickCurrentPowerLevel"
    (joystick @-> returning int)

let joystick_event_state =
  foreign "SDL_JoystickEventState" (int @-> returning nat_to_ok)

let joystick_from_instance_id =
  foreign "SDL_JoystickFromInstanceID" (joystick_id @-> returning joystick)

let joystick_get_event_state () =
  joystick_event_state sdl_query

let joystick_set_event_state s =
  joystick_event_state s

let joystick_get_attached =
  foreign "SDL_JoystickGetAttached" (joystick @-> returning bool)

let joystick_get_axis =
  foreign "SDL_JoystickGetAxis" (joystick @-> int @-> returning int16_t)

let joystick_get_axis_initial_state =
  foreign "SDL_JoystickGetAxisInitialState"
    (joystick @-> int @-> returning int16_t)

let joystick_get_ball =
  foreign "SDL_JoystickGetBall"
    (joystick @-> int @-> (ptr int) @-> (ptr int) @-> returning int)

let joystick_get_ball j i =
  let x = allocate int 0 in
  let y = allocate int 0 in
  match joystick_get_ball j i x y with
  | 0 -> Ok (!@ x, !@ y) | _ -> error ()

let joystick_get_button =
  foreign "SDL_JoystickGetButton"
    (joystick @-> int @-> returning int_as_uint8_t)

let joystick_get_device_guid =
  foreign "SDL_JoystickGetDeviceGUID" (int @-> returning joystick_guid)

let joystick_get_device_product =
  foreign "SDL_JoystickGetDeviceProduct" (int @-> returning int_as_uint16_t)

let joystick_get_device_product_version =
  foreign "SDL_JoystickGetDeviceProductVersion"
    (int @-> returning int_as_uint16_t)

let joystick_get_device_type =
  foreign "SDL_JoystickGetDeviceType" (int @-> returning int)

let joystick_get_device_instance_id =
  foreign "SDL_JoystickGetDeviceInstanceID" (int @-> returning joystick_id)

let joystick_get_device_vendor =
  foreign "SDL_JoystickGetDeviceVendor" (int @-> returning int_as_uint16_t)

let joystick_get_guid =
  foreign "SDL_JoystickGetGUID" (joystick @-> returning joystick_guid)

let joystick_get_guid_from_string =
  foreign "SDL_JoystickGetGUIDFromString" (string @-> returning joystick_guid)

let joystick_get_guid_string =
  foreign "SDL_JoystickGetGUIDString"
    (joystick_guid @-> ptr char @-> int @-> returning void)

let joystick_get_guid_string guid =
  let len = 33 in
  let s = CArray.start (CArray.make char 33) in
  joystick_get_guid_string guid s len;
  coerce (ptr char) string s

let joystick_get_hat =
  foreign "SDL_JoystickGetHat" (joystick @-> int @-> returning int_as_uint8_t)

let joystick_get_product =
  foreign "SDL_JoystickGetProduct" (joystick @-> returning int_as_uint16_t)

let joystick_get_product_version =
  foreign "SDL_JoystickGetProductVersion"
    (joystick @-> returning int_as_uint16_t)

let joystick_get_type =
  foreign "SDL_JoystickGetType" (joystick @-> returning int)

let joystick_get_vendor =
  foreign "SDL_JoystickGetVendor" (joystick @-> returning int_as_uint16_t)

let joystick_instance_id =
  foreign "SDL_JoystickInstanceID" (joystick @-> returning joystick_id)

let joystick_instance_id j =
  match joystick_instance_id j with
  | n when n < 0l -> error () | n -> Ok n

let joystick_name =
  foreign "SDL_JoystickName" (joystick @-> returning (some_to_ok string_opt))

let joystick_name_for_index =
  foreign "SDL_JoystickNameForIndex" (int @-> returning (some_to_ok string_opt))

let joystick_num_axes =
  foreign "SDL_JoystickNumAxes" (joystick @-> returning nat_to_ok)

let joystick_num_balls =
  foreign "SDL_JoystickNumBalls" (joystick @-> returning nat_to_ok)

let joystick_num_buttons =
  foreign "SDL_JoystickNumButtons" (joystick @-> returning nat_to_ok)

let joystick_num_hats =
  foreign "SDL_JoystickNumHats" (joystick @-> returning nat_to_ok)

let joystick_open =
  foreign "SDL_JoystickOpen" (int @-> returning (some_to_ok joystick_opt))

let joystick_update =
  foreign "SDL_JoystickUpdate" (void @-> returning void)

let num_joysticks =
  foreign "SDL_NumJoysticks" (void @-> returning nat_to_ok)

(* Game controller *)

type game_controller = unit ptr
let game_controller : game_controller typ = ptr void
let game_controller_opt : game_controller option typ = ptr_opt void

let unsafe_game_controller_of_ptr addr : game_controller =
  ptr_of_raw_address addr
let unsafe_ptr_of_game_controller game_controller =
  raw_address_of_ptr (to_voidp game_controller)

type _button_bind
let button_bind : _button_bind structure typ =
  structure "SDL_GameControllerBindType"
let button_bind_bind_type = field button_bind "bindType" int
let button_bind_value1 = field button_bind "value1" int  (* simplified enum *)
let button_bind_value2 = field button_bind "value2" int
let () = seal button_bind

module Controller = struct
  type bind_type = int
  let bind_type_none = sdl_controller_bindtype_none
  let bind_type_button = sdl_controller_bindtype_button
  let bind_type_axis = sdl_controller_bindtype_axis
  let bind_type_hat = sdl_controller_bindtype_hat

  type axis = int
  let axis_invalid = sdl_controller_axis_invalid
  let axis_left_x = sdl_controller_axis_leftx
  let axis_left_y = sdl_controller_axis_lefty
  let axis_right_x = sdl_controller_axis_rightx
  let axis_right_y = sdl_controller_axis_righty
  let axis_trigger_left = sdl_controller_axis_triggerleft
  let axis_trigger_right = sdl_controller_axis_triggerright
  let axis_max = sdl_controller_axis_max

  type button = int
  let button_invalid = sdl_controller_button_invalid
  let button_a = sdl_controller_button_a
  let button_b = sdl_controller_button_b
  let button_x = sdl_controller_button_x
  let button_y = sdl_controller_button_y
  let button_back = sdl_controller_button_back
  let button_guide = sdl_controller_button_guide
  let button_start = sdl_controller_button_start
  let button_left_stick = sdl_controller_button_leftstick
  let button_right_stick = sdl_controller_button_rightstick
  let button_left_shoulder = sdl_controller_button_leftshoulder
  let button_right_shoulder = sdl_controller_button_rightshoulder
  let button_dpad_up = sdl_controller_button_dpad_up
  let button_dpad_down = sdl_controller_button_dpad_down
  let button_dpad_left = sdl_controller_button_dpad_left
  let button_dpad_right = sdl_controller_button_dpad_right
  let button_max = sdl_controller_button_max

  type button_bind = _button_bind structure
  let bind_type v = getf v button_bind_bind_type
  let bind_button_value v = getf v button_bind_value1
  let bind_axis_value v = getf v button_bind_value1
  let bind_hat_value v = getf v button_bind_value1, getf v button_bind_value2
end

let game_controller_add_mapping =
  foreign "SDL_GameControllerAddMapping" (string @-> returning bool_to_ok)

let game_controller_add_mapping_from_rw =
  foreign "SDL_GameControllerAddMappingsFromRW"
    ~stub (rw_ops @-> bool @-> returning nat_to_ok)

let game_controller_close =
  foreign "SDL_GameControllerClose" (game_controller @-> returning void)

let game_controller_event_state =
  foreign "SDL_GameControllerEventState" (int @-> returning nat_to_ok)

let game_controller_from_instance_id =
  foreign "SDL_GameControllerFromInstanceID"
    (joystick_id @-> returning game_controller)

let game_controller_get_event_state () =
  game_controller_event_state sdl_query

let game_controller_set_event_state t =
  game_controller_event_state t

let game_controller_get_attached =
  foreign "SDL_GameControllerGetAttached" (game_controller @-> returning bool)

let game_controller_get_axis =
  foreign "SDL_GameControllerGetAxis"
    (game_controller @-> int @-> returning int16_t)

let game_controller_get_axis_from_string =
  foreign "SDL_GameControllerGetAxisFromString"
    (string @-> returning int)

let game_controller_get_bind_for_axis =
  foreign "SDL_GameControllerGetBindForAxis"
    (game_controller @-> int @-> returning button_bind)

let game_controller_get_bind_for_button =
  foreign "SDL_GameControllerGetBindForButton"
    (game_controller @-> int @-> returning button_bind)

let game_controller_get_button =
  foreign "SDL_GameControllerGetButton"
    (game_controller @-> int @-> returning int_as_uint8_t)

let game_controller_get_button_from_string =
  foreign "SDL_GameControllerGetButtonFromString" (string @-> returning int)

let game_controller_get_joystick =
  foreign "SDL_GameControllerGetJoystick"
    (game_controller @-> returning (some_to_ok joystick_opt))

let game_controller_get_product =
  foreign "SDL_GameControllerGetProduct"
    (game_controller @-> returning int_as_uint16_t)

let game_controller_get_product_version =
  foreign "SDL_GameControllerGetProductVersion"
    (game_controller @-> returning int_as_uint16_t)

let game_controller_get_string_for_axis =
  foreign "SDL_GameControllerGetStringForAxis" (int @-> returning string_opt)

let game_controller_get_string_for_button =
  foreign "SDL_GameControllerGetStringForButton" (int @-> returning string_opt)

let game_controller_get_vendor =
  foreign "SDL_GameControllerGetVendor"
    (game_controller @-> returning int_as_uint16_t)

let game_controller_mapping =
  foreign "SDL_GameControllerMapping"
    (game_controller @-> returning (some_to_ok string_opt))

let game_controller_mapping_for_index =
  foreign "SDL_GameControllerMappingForIndex"
    (int @-> returning (some_to_ok string_opt))

let game_controller_mapping_for_guid =
  foreign "SDL_GameControllerMappingForGUID"
    (joystick_guid @-> returning (some_to_ok string_opt))

let game_controller_name =
  foreign "SDL_GameControllerName"
    (game_controller @-> returning (some_to_ok string_opt))

let game_controller_name_for_index =
  foreign "SDL_GameControllerNameForIndex"
    (int @-> returning (some_to_ok string_opt))

let game_controller_num_mappings =
  foreign "SDL_GameControllerNumMappings" (void @-> returning int)

let game_controller_open =
  foreign "SDL_GameControllerOpen"
    (int @-> returning (some_to_ok game_controller_opt))

let game_controller_update =
  foreign "SDL_GameControllerUpdate" (void @-> returning void)

let is_game_controller =
  foreign "SDL_IsGameController" (int @-> returning bool)

(* Events *)

type event_type = int
let event_type : event_type typ = int_as_uint32_t

module Event = struct

  (* Event structures *)

  module Common = struct
    type t
    let t : t structure typ = structure "SDL_CommonEvent"
    let typ = field t "type" int_as_uint32_t
    let timestamp = field t "timestamp" int32_as_uint32_t
    let () = seal t
  end

  module Controller_axis_event = struct
    type t
    let t : t structure typ = structure "SDL_ControllerAxisEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let axis = field t "axis" int_as_uint8_t
    let _ = field t "padding1" uint8_t
    let _ = field t "padding2" uint8_t
    let _ = field t "padding3" uint8_t
    let value = field t "value" int16_t
    let _ = field t "padding4" uint16_t
    let () = seal t
  end

  module Controller_button_event = struct
    type t
    let t : t structure typ = structure "SDL_ControllerButtonEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let button = field t "button" int_as_uint8_t
    let state = field t "state" int_as_uint8_t
    let _ = field t "padding1" uint8_t
    let _ = field t "padding2" uint8_t
    let () = seal t
  end

  module Controller_device_event = struct
    type t
    let t : t structure typ = structure "SDL_ControllerDeviceEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let () = seal t
  end

  module Dollar_gesture_event = struct
    type t
    let t : t structure typ = structure "SDL_DollarGestureEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let touch_id = field t "touchId" touch_id
    let gesture_id = field t "gestureId" gesture_id
    let num_fingers = field t "numFingers" int_as_uint32_t
    let error = field t "error" float
    let x = field t "x" float
    let y = field t "y" float
    let () = seal t
  end

  module Drop_event = struct
    type t
    let t : t structure typ = structure "SDL_DropEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let file = field t "file" (ptr char)
    let window_id = field t "windowID" int_as_uint32_t
    let () = seal t
  end

  module Keyboard_event = struct
    type t
    let t : t structure typ = structure "SDL_KeyboardEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let state = field t "state" int_as_uint8_t
    let repeat = field t "repeat" int_as_uint8_t
    let padding2 = field t "padding2" uint8_t
    let padding3 = field t "padding3" uint8_t
    (* We inline the definition of SDL_Keysym *)
    let scancode = field t "scancode" scancode
    let keycode = field t "sym" keycode
    let keymod = field t "mod" keymod
    let unused = field t "unused" uint32_t
    let () = seal t
  end

  module Joy_axis_event = struct
    type t
    let t : t structure typ = structure "SDL_JoyAxisEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let axis = field t "axis" int_as_uint8_t
    let _ = field t "padding1" uint8_t
    let _ = field t "padding2" uint8_t
    let _ = field t "padding3" uint8_t
    let value = field t "value" int16_t
    let _ = field t "padding4" uint16_t
    let () = seal t
  end

  module Joy_ball_event = struct
    type t
    let t : t structure typ = structure "SDL_JoyBallEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let ball = field t "ball" int_as_uint8_t
    let _ = field t "padding1" uint8_t
    let _ = field t "padding2" uint8_t
    let _ = field t "padding3" uint8_t
    let xrel = field t "xrel" int16_t
    let yrel = field t "yrel" int16_t
    let () = seal t
  end

  module Joy_button_event = struct
    type t
    let t : t structure typ = structure "SDL_JoyButtonEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let button = field t "button" int_as_uint8_t
    let state = field t "state" int_as_uint8_t
    let _ = field t "padding1" uint8_t
    let _ = field t "padding2" uint8_t
    let () = seal t
  end

  module Joy_device_event = struct
    type t
    let t : t structure typ = structure "SDL_JoyDeviceEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let () = seal t
  end

  module Joy_hat_event = struct
    type t
    let t : t structure typ = structure "SDL_JoyHatEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" joystick_id
    let hat = field t "hat" int_as_uint8_t
    let value = field t "value" int_as_uint8_t
    let _ = field t "padding1" uint8_t
    let _ = field t "padding2" uint8_t
    let () = seal t
  end

  module Mouse_button_event = struct
    type t
    let t : t structure typ = structure "SDL_MouseButtonEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let which = field t "which" int32_as_uint32_t
    let button = field t "button" int_as_uint8_t
    let state = field t "state" int_as_uint8_t
    let clicks = field t "clicks" int_as_uint8_t
    let _ = field t "padding1" int_as_uint8_t
    let x = field t "x" int_as_int32_t
    let y = field t "y" int_as_int32_t
    let () = seal t
  end

  module Mouse_motion_event = struct
    type t
    let t : t structure typ = structure "SDL_MouseMotionEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let which = field t "which" int32_as_uint32_t
    let state = field t "state" int32_as_uint32_t
    let x = field t "x" int_as_int32_t
    let y = field t "y" int_as_int32_t
    let xrel = field t "xrel" int_as_int32_t
    let yrel = field t "yrel" int_as_int32_t
    let () = seal t
  end

    type mouse_wheel_direction = int
    let mouse_wheel_normal = sdl_mousewheel_normal
    let mouse_wheel_flipped = sdl_mousewheel_flipped

  module Mouse_wheel_event = struct
    type t
    let t : t structure typ = structure "SDL_MouseWheelEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let which = field t "which" int32_as_uint32_t
    let x = field t "x" int_as_int32_t
    let y = field t "y" int_as_int32_t
    let direction = field t "direction" int_as_uint32_t
    let () = seal t
  end

  module Multi_gesture_event = struct
    type t
    let t : t structure typ = structure "SDL_MultiGestureEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let touch_id = field t "touchId" touch_id
    let dtheta = field t "dTheta" float
    let ddist = field t "ddist" float
    let x = field t "x" float
    let y = field t "y" float
    let num_fingers = field t "numFingers" int_as_uint16_t
    let _ = field t "padding" uint16_t
    let () = seal t
  end

  module Sensor_event = struct
    type t
    let t : t structure typ = structure "SDL_SensorEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let which = field t "which" int32_as_uint32_t
    (* FIXME: No array here, see
       https://github.com/ocamllabs/ocaml-ctypes/issues/113 *)
    let data0 = field t "data0" float
    let data1 = field t "data1" float
    let data2 = field t "data2" float
    let data3 = field t "data3" float
    let data4 = field t "data4" float
    let data5 = field t "data5" float
    let () = seal t
  end

  module Quit_event = struct
    type t
    let t : t structure typ = structure "SDL_QuitEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let () = seal t
  end

  module Sys_wm_event = struct
    type t
    let t : t structure typ = structure "SDL_SysWMEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let _ = field t "msg" (ptr void)
    let () = seal t
  end

  module Text_editing_event = struct
    type t
    let t : t structure typ = structure "SDL_TextEditingEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let text = field t "text" (string_as_char_array
                                 sdl_texteditingevent_text_size)
    let start = field t "start" int_as_int32_t
    let length = field t "end" int_as_int32_t
    let () = seal t
  end

  module Text_input_event = struct
    type t
    let t : t structure typ = structure "SDL_TextIfmtsnputEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let text = field t "text" (string_as_char_array
                                 sdl_textinputevent_text_size)
    let () = seal t
  end

  module Touch_finger_event = struct
    type t
    let t : t structure typ = structure "SDL_TouchFingerEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let touch_id = field t "touchId" touch_id
    let finger_id = field t "fingerId" finger_id
    let x = field t "x" float
    let y = field t "y" float
    let dx = field t "dx" float
    let dy = field t "dy" float
    let pressure = field t "pressure" float
    let () = seal t
  end

  module User_event = struct
    type t
    let t : t structure typ = structure "SDL_UserEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let code = field t "code" int_as_int32_t
    let _ = field t "data1" (ptr void)
    let _ = field t "data2" (ptr void)
    let () = seal t
  end

  module Window_event = struct
    type t
    let t : t structure typ = structure "SDL_WindowEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let window_id = field t "windowID" int_as_uint32_t
    let event = field t "event" int_as_uint8_t
    let padding1 = field t "padding1" uint8_t
    let padding2 = field t "padding2" uint8_t
    let padding3 = field t "padding3" uint8_t
    let data1 = field t "data1" int32_t
    let data2 = field t "data2" int32_t
    let () = seal t
  end

  module Display_event = struct
    type t
    let t : t structure typ = structure "SDL_DisplayEvent"
    let _ = field t "type" int_as_uint32_t
    let _ = field t "timestamp" int32_as_uint32_t
    let display = field t "display" int32_as_uint32_t
    let event = field t "event" int_as_uint8_t
    let padding1 = field t "padding1" uint8_t
    let padding2 = field t "padding2" uint8_t
    let padding3 = field t "padding3" uint8_t
    let data1 = field t "data1" int32_t
    let () = seal t
  end

  module Audio_device_event = struct
    type t
    let t : t structure typ = structure "SDL_AudioDevice"
    let _ = field t "type" int_as_uint32_t
    let timestamp = field t "timestamp" int32_as_uint32_t
    let which = field t "which" int32_as_uint32_t
    let iscapture = field t "iscapture" int_as_uint8_t
    let () = seal t
  end

  type t
  let t : t union typ = union "SDL_Event"
  let typ = field t "type" int_as_uint32_t
  let audio_device_event = field t "adevice" Audio_device_event.t
  let common = field t "common" Common.t
  let controller_axis_event = field t "caxis" Controller_axis_event.t
  let controller_button_event = field t "cbutton" Controller_button_event.t
  let controller_device_event = field t "cdevice" Controller_device_event.t
  let dollar_gesture_event = field t "dgesture" Dollar_gesture_event.t
  let drop_event = field t "drop" Drop_event.t
  let joy_axis_event = field t "jaxis" Joy_axis_event.t
  let joy_ball_event = field t "jball" Joy_ball_event.t
  let joy_button_event = field t "jbutton" Joy_button_event.t
  let joy_device_event = field t "jdevice" Joy_device_event.t
  let joy_hat_event = field t "jhat" Joy_hat_event.t
  let keyboard_event = field t "key" Keyboard_event.t
  let mouse_button_event = field t "button" Mouse_button_event.t
  let mouse_motion_event = field t "motion" Mouse_motion_event.t
  let mouse_wheel_event = field t "wheel" Mouse_wheel_event.t
  let multi_gesture_event = field t "mgesture" Multi_gesture_event.t
  let quit_event = field t "quit" Quit_event.t
  let sys_wm_event = field t "syswm" Sys_wm_event.t
  let text_editing_event = field t "edit" Text_editing_event.t
  let text_input_event = field t "text" Text_input_event.t
  let touch_finger_event = field t "tfinger" Touch_finger_event.t
  let user_event = field t "user" User_event.t
  let window_event = field t "window" Window_event.t
  let display_event = field t "display" Display_event.t
  let sensor_event = field t "sensor" Sensor_event.t
  let padding = field t "padding"
      (abstract ~name:"padding" ~size:tsdl_sdl_event_size ~alignment:1)
  let () = seal t

  let create () = make t
  let opt_addr = function
  | None -> coerce (ptr void) (ptr t) null
  | Some v -> addr v

  type _ field =
      F : (* existential to hide the 'a structure *)
        (('a structure, t union) Ctypes.field *
         ('b, 'a structure) Ctypes.field) -> 'b field

  let get e (F (s, f)) = getf (getf e s) f
  let set e (F (s, f)) v = setf (getf e s) f v

  (* Aliases *)

  let first_event = sdl_firstevent
  let last_event = sdl_lastevent

  (* Common *)

  let typ  = F (common, Common.typ)
  let timestamp = F (common, Common.timestamp)

  (* Application events. *)

  let app_terminating = sdl_app_terminating
  let app_low_memory = sdl_app_lowmemory
  let app_will_enter_background = sdl_app_willenterbackground
  let app_did_enter_background = sdl_app_didenterbackground
  let app_will_enter_foreground = sdl_app_willenterforeground
  let app_did_enter_foreground = sdl_app_didenterforeground

  (* Clipboard events *)

  let clipboard_update = sdl_clipboardupdate

  (* Controller events *)

  let controller_axis_motion = sdl_controlleraxismotion
  let controller_button_down = sdl_controllerbuttondown
  let controller_button_up = sdl_controllerbuttonup
  let controller_device_added = sdl_controllerdeviceadded
  let controller_device_remapped = sdl_controllerdeviceremapped
  let controller_device_removed = sdl_controllerdeviceremoved

  let controller_axis_which =
    F (controller_axis_event, Controller_axis_event.which)
  let controller_axis_axis =
    F (controller_axis_event, Controller_axis_event.axis)
  let controller_axis_value =
    F (controller_axis_event, Controller_axis_event.value)

  let controller_button_which =
    F (controller_button_event, Controller_button_event.which)
  let controller_button_button =
    F (controller_button_event, Controller_button_event.button)
  let controller_button_state =
    F (controller_button_event, Controller_button_event.state)

  let controller_device_which =
    F (controller_device_event, Controller_device_event.which)

  (* Dollar gesture events *)

  let dollar_gesture = sdl_dollargesture
  let dollar_record = sdl_dollarrecord

  let dollar_gesture_touch_id =
    F (dollar_gesture_event, Dollar_gesture_event.touch_id)
  let dollar_gesture_gesture_id =
    F (dollar_gesture_event, Dollar_gesture_event.gesture_id)
  let dollar_gesture_num_fingers =
    F (dollar_gesture_event, Dollar_gesture_event.num_fingers)
  let dollar_gesture_error =
    F (dollar_gesture_event, Dollar_gesture_event.error)
  let dollar_gesture_x = F (dollar_gesture_event, Dollar_gesture_event.x)
  let dollar_gesture_y = F (dollar_gesture_event, Dollar_gesture_event.y)

  (* Drop file event *)

  let drop_file = sdl_dropfile
  let drop_text = sdl_droptext
  let drop_begin = sdl_dropbegin
  let drop_complete = sdl_dropcomplete

  let drop_file_file = F (drop_event, Drop_event.file)
  let drop_window_id = F (drop_event, Drop_event.window_id)

  let drop_file_free e =
    let sp = to_voidp (get e drop_file_file) in
    if is_null sp then () else sdl_free sp

  let drop_file_file e =
    let sp = get e drop_file_file in
    if is_null sp then None else Some (coerce (ptr char) string sp)

  (* Touch events *)

  let finger_down = sdl_fingerdown
  let finger_motion = sdl_fingermotion
  let finger_up = sdl_fingerup

  let touch_finger_touch_id = F (touch_finger_event,Touch_finger_event.touch_id)
  let touch_finger_finger_id =
    F (touch_finger_event, Touch_finger_event.finger_id)
  let touch_finger_x = F (touch_finger_event, Touch_finger_event.x)
  let touch_finger_y = F (touch_finger_event, Touch_finger_event.y)
  let touch_finger_dx = F (touch_finger_event, Touch_finger_event.dx)
  let touch_finger_dy = F (touch_finger_event, Touch_finger_event.dy)
  let touch_finger_pressure =
    F (touch_finger_event, Touch_finger_event.pressure)

  (* Joystick events. *)

  let joy_axis_motion = sdl_joyaxismotion
  let joy_ball_motion = sdl_joyballmotion
  let joy_button_down = sdl_joybuttondown
  let joy_button_up = sdl_joybuttonup
  let joy_device_added = sdl_joydeviceadded
  let joy_device_removed = sdl_joydeviceremoved
  let joy_hat_motion = sdl_joyhatmotion

  let joy_axis_which = F (joy_axis_event, Joy_axis_event.which)
  let joy_axis_axis = F (joy_axis_event, Joy_axis_event.axis)
  let joy_axis_value = F (joy_axis_event, Joy_axis_event.value)

  let joy_ball_which = F (joy_ball_event, Joy_ball_event.which)
  let joy_ball_ball = F (joy_ball_event, Joy_ball_event.ball)
  let joy_ball_xrel = F (joy_ball_event, Joy_ball_event.xrel)
  let joy_ball_yrel = F (joy_ball_event, Joy_ball_event.yrel)

  let joy_button_which = F (joy_button_event, Joy_button_event.which)
  let joy_button_button = F (joy_button_event, Joy_button_event.button)
  let joy_button_state = F (joy_button_event, Joy_button_event.state)

  let joy_device_which = F (joy_device_event, Joy_device_event.which)

  let joy_hat_which = F (joy_hat_event, Joy_hat_event.which)
  let joy_hat_hat = F (joy_hat_event, Joy_hat_event.hat)
  let joy_hat_value = F (joy_hat_event, Joy_hat_event.value)

  (* Keyboard events *)

  let key_down = sdl_keydown
  let key_up = sdl_keyup
  let keymap_changed = sdl_keymapchanged

  let keyboard_window_id = F (keyboard_event, Keyboard_event.window_id)
  let keyboard_repeat = F (keyboard_event, Keyboard_event.repeat)
  let keyboard_state = F (keyboard_event, Keyboard_event.state)
  let keyboard_scancode = F (keyboard_event, Keyboard_event.scancode)
  let keyboard_keycode = F (keyboard_event, Keyboard_event.keycode)
  let keyboard_keymod = F (keyboard_event, Keyboard_event.keymod)

  (* Mouse events *)

  let mouse_button_down = sdl_mousebuttondown
  let mouse_button_up = sdl_mousebuttonup
  let mouse_motion = sdl_mousemotion
  let mouse_wheel = sdl_mousewheel

  let mouse_button_window_id =
    F (mouse_button_event, Mouse_button_event.window_id)
  let mouse_button_which = F (mouse_button_event, Mouse_button_event.which)
  let mouse_button_state = F (mouse_button_event, Mouse_button_event.state)
  let mouse_button_button = F (mouse_button_event, Mouse_button_event.button)
  let mouse_button_clicks = F (mouse_button_event, Mouse_button_event.clicks)
  let mouse_button_x = F (mouse_button_event, Mouse_button_event.x)
  let mouse_button_y = F (mouse_button_event, Mouse_button_event.y)

  let mouse_motion_window_id =
    F (mouse_motion_event, Mouse_motion_event.window_id)
  let mouse_motion_which = F (mouse_motion_event, Mouse_motion_event.which)
  let mouse_motion_state = F (mouse_motion_event, Mouse_motion_event.state)
  let mouse_motion_x = F (mouse_motion_event, Mouse_motion_event.x)
  let mouse_motion_y = F (mouse_motion_event, Mouse_motion_event.y)
  let mouse_motion_xrel = F (mouse_motion_event, Mouse_motion_event.xrel)
  let mouse_motion_yrel = F (mouse_motion_event, Mouse_motion_event.yrel)

  let mouse_wheel_window_id = F (mouse_wheel_event, Mouse_wheel_event.window_id)
  let mouse_wheel_which = F (mouse_wheel_event, Mouse_wheel_event.which)
  let mouse_wheel_x = F (mouse_wheel_event, Mouse_wheel_event.x)
  let mouse_wheel_y = F (mouse_wheel_event, Mouse_wheel_event.y)
  let mouse_wheel_direction = F(mouse_wheel_event, Mouse_wheel_event.direction)

  (* Multi gesture events *)

  let multi_gesture = sdl_multigesture

  let multi_gesture_touch_id =
    F (multi_gesture_event, Multi_gesture_event.touch_id)
  let multi_gesture_dtheta = F (multi_gesture_event, Multi_gesture_event.dtheta)
  let multi_gesture_ddist = F (multi_gesture_event, Multi_gesture_event.ddist)
  let multi_gesture_x = F (multi_gesture_event, Multi_gesture_event.x)
  let multi_gesture_y = F (multi_gesture_event, Multi_gesture_event.y)
  let multi_gesture_num_fingers =
    F (multi_gesture_event, Multi_gesture_event.num_fingers)

  (* Quit events *)

  let quit = sdl_quit

  (* System window manager events *)

  let sys_wm_event = sdl_syswmevent

  (* Text events *)

  let text_editing = sdl_textediting
  let text_input = sdl_textinput

  let text_editing_window_id =
    F (text_editing_event, Text_editing_event.window_id)
  let text_editing_text = F (text_editing_event, Text_editing_event.text)
  let text_editing_start = F (text_editing_event, Text_editing_event.start)
  let text_editing_length = F (text_editing_event, Text_editing_event.length)

  let text_input_window_id = F (text_input_event, Text_input_event.window_id)
  let text_input_text = F (text_input_event, Text_input_event.text)

  (* User events *)

  let user_window_id = F (user_event, User_event.window_id)
  let user_code = F (user_event, User_event.code)
  let user_event = sdl_userevent

  (* Window events *)

  type window_event_id = int
  let window_event_shown = sdl_windowevent_shown
  let window_event_hidden = sdl_windowevent_hidden
  let window_event_exposed = sdl_windowevent_exposed
  let window_event_moved = sdl_windowevent_moved
  let window_event_resized = sdl_windowevent_resized
  let window_event_size_changed = sdl_windowevent_size_changed
  let window_event_minimized = sdl_windowevent_minimized
  let window_event_maximized = sdl_windowevent_maximized
  let window_event_restored = sdl_windowevent_restored
  let window_event_enter = sdl_windowevent_enter
  let window_event_leave = sdl_windowevent_leave
  let window_event_focus_gained = sdl_windowevent_focus_gained
  let window_event_focus_lost = sdl_windowevent_focus_lost
  let window_event_close = sdl_windowevent_close
  let window_event_take_focus = sdl_windowevent_take_focus
  let window_event_hit_test = sdl_windowevent_hit_test

  let window_window_id = F (window_event, Window_event.window_id)
  let window_event_id = F (window_event, Window_event.event)
  let window_data1 = F (window_event, Window_event.data1)
  let window_data2 = F (window_event, Window_event.data2)

  let window_event = sdl_windowevent

  (* Window event id enum *)

  type window_event_enum =
    [ `Close | `Enter | `Exposed | `Focus_gained | `Focus_lost | `Hidden
    | `Hit_test | `Leave | `Maximized | `Minimized | `Moved | `Resized
    | `Restored | `Shown | `Size_changed | `Take_focus
    | `Unknown of window_event_id ]

  let enum_of_window_event_id =
    let add acc (k, v) = Imap.add k v acc in
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
    List.fold_left add Imap.empty enums

  let window_event_enum id =
    try Imap.find id enum_of_window_event_id with Not_found -> `Unknown id

  (* Display event *)

  let display_display =
    F (display_event, Display_event.display)

  let display_event_id =
    F (display_event, Display_event.event)

  let display_data1 =
    F (display_event, Display_event.data1)

  let display_event = sdl_displayevent

  (* Sensor event *)

  let sensor_which =
    F (sensor_event, Sensor_event.which)

  let sensor_data0 =
    F (sensor_event, Sensor_event.data0)

  let sensor_data1 =
    F (sensor_event, Sensor_event.data1)

  let sensor_data2 =
    F (sensor_event, Sensor_event.data2)

  let sensor_data3 =
    F (sensor_event, Sensor_event.data3)

  let sensor_data4 =
    F (sensor_event, Sensor_event.data4)

  let sensor_data5 =
    F (sensor_event, Sensor_event.data5)

  let sensor_update = sdl_sensorupdate

  (* Render events *)

  let render_targets_reset = sdl_render_targets_reset
  let render_device_reset = sdl_render_device_reset

  (* Audio device event *)

  let audio_device_added = sdl_audiodeviceadded
  let audio_device_removed = sdl_audiodeviceremoved

  let audio_device_timestamp =
    F (audio_device_event, Audio_device_event.timestamp)

  let audio_device_which =
    F (audio_device_event, Audio_device_event.which)

  let audio_device_is_capture =
    F (audio_device_event, Audio_device_event.iscapture)

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
                  user_event, `User_event;
                  quit, `Quit;
                  window_event, `Window_event;
                  display_event, `Display_event;
                  sensor_update, `Sensor_update; ]
    in
    List.fold_left add Imap.empty enums

  let enum t = try Imap.find t enum_of_event_type with Not_found -> `Unknown t

end

type event = Event.t union

let event_state =
  foreign "SDL_EventState" (event_type @-> int @-> returning int_as_uint8_t)

let get_event_state e =
  event_state e sdl_query

let set_event_state e s =
  ignore (event_state e s)

let flush_event =
  foreign "SDL_FlushEvent" (event_type @-> returning void)

let flush_events =
  foreign "SDL_FlushEvents" (event_type @-> event_type @-> returning void)

let has_event =
  foreign "SDL_HasEvent" (event_type @-> returning bool)

let has_events =
  foreign "SDL_HasEvents" (event_type @-> event_type @-> returning bool)

let poll_event =
  foreign "SDL_PollEvent" (ptr Event.t @-> returning bool)

let poll_event e =
  poll_event (Event.opt_addr e)

let pump_events =
  foreign "SDL_PumpEvents" (void @-> returning void)

let push_event =
  foreign "SDL_PushEvent" (ptr Event.t @-> returning bool_to_ok)

let push_event e =
  push_event (addr e)

let register_events =
  foreign "SDL_RegisterEvents" (int @-> returning uint32_t)

let register_event () = match Unsigned.UInt32.to_int32 (register_events 1) with
| -1l -> None | t -> Some (Int32.to_int t)

let wait_event =
  foreign ~release_runtime_lock:true
    "SDL_WaitEvent" (ptr Event.t @-> returning int)

let wait_event e = match wait_event (Event.opt_addr e) with
| 1 -> Ok () | _ -> error ()

let wait_event_timeout =
  foreign "SDL_WaitEventTimeout" ~release_runtime_lock:true
    (ptr Event.t @-> int @-> returning bool)

let wait_event_timeout e t =
  wait_event_timeout (Event.opt_addr e) t

(* Force feedback *)

type haptic = unit ptr
let haptic : haptic typ = ptr void
let haptic_opt : haptic option typ = ptr_opt void

module Haptic = struct
  let infinity = -1l

  (* Features *)

  type feature = int
  let gain = sdl_haptic_gain
  let autocenter = sdl_haptic_autocenter
  let status = sdl_haptic_status
  let pause = sdl_haptic_pause

  (* Directions *)

  type direction_type = int
  let polar = sdl_haptic_polar
  let cartesian = sdl_haptic_cartesian
  let spherical = sdl_haptic_spherical

  module Direction = struct
    type _t
    type t = _t structure
    let t : _t structure typ = structure "SDL_HapticDirection"
    let typ = field t "type" int_as_uint8_t
    let dir_0 = field t "dir0" int32_t
    let dir_1 = field t "dir1" int32_t
    let dir_2 = field t "dir2" int32_t
    let () = seal t

    let create typv d0 d1 d2 =
      let d = make t in
      setf d typ typv;
      setf d dir_0 d0;
      setf d dir_1 d1;
      setf d dir_2 d2;
      d

    let typ d = getf d typ
    let dir_0 d = getf d dir_0
    let dir_1 d = getf d dir_1
    let dir_2 d = getf d dir_2
  end

  (* Effects *)

  module Constant = struct
    type t
    let t : t structure typ = structure "SDL_HapticConstant"
    let typ = field t "type" int_as_uint16_t
    let direction = field t "direction" Direction.t
    let length = field t "length" int32_as_uint32_t
    let delay = field t "delay" int_as_uint16_t
    let button = field t "button" int_as_uint16_t
    let interval = field t "interval" int_as_uint16_t

    let level = field t "level" int16_t
    let attack_length = field t "attack_length" int_as_uint16_t
    let attack_level = field t "attack_level" int_as_uint16_t
    let fade_length = field t "fade_length" int_as_uint16_t
    let fade_level = field t "fade_level" int_as_uint16_t
    let () = seal t
  end

  module Periodic = struct
    type t
    let t : t structure typ = structure "SDL_HapticPeriodic"
    let typ = field t "type" int_as_uint16_t
    let direction = field t "direction" Direction.t
    let length = field t "length" int32_as_uint32_t
    let delay = field t "delay" int_as_uint16_t
    let button = field t "button" int_as_uint16_t
    let interval = field t "interval" int_as_uint16_t

    let period = field t "period" int_as_uint16_t
    let magnitude = field t "magnitude" int16_t
    let offset = field t "offset" int16_t
    let phase = field t "phase" int_as_uint16_t
    let attack_length = field t "attack_length" int_as_uint16_t
    let attack_level = field t "attack_level" int_as_uint16_t
    let fade_length = field t "fade_length" int_as_uint16_t
    let fade_level = field t "fade_level" int_as_uint16_t
    let () = seal t
  end

  module Condition = struct
    type t
    let t : t structure typ = structure "SDL_HapticCondition"
    let typ = field t "type" int_as_uint16_t
    let direction = field t "direction" Direction.t
    let length = field t "length" int32_as_uint32_t
    let delay = field t "delay" int_as_uint16_t
    let button = field t "button" int_as_uint16_t
    let interval = field t "interval" int_as_uint16_t

    let right_sat_0 = field t "right_sat[0]" int_as_uint16_t
    let right_sat_1 = field t "right_sat[1]" int_as_uint16_t
    let right_sat_2 = field t "right_sat[2]" int_as_uint16_t
    let left_sat_0 = field t "left_sat[0]" int_as_uint16_t
    let left_sat_1 = field t "left_sat[1]" int_as_uint16_t
    let left_sat_2 = field t "left_sat[2]" int_as_uint16_t
    let right_coeff_0 = field t "right_coeff[0]" int16_t
    let right_coeff_1 = field t "right_coeff[1]" int16_t
    let right_coeff_2 = field t "right_coeff[2]" int16_t
    let left_coeff_0 = field t "left_coeff[0]" int16_t
    let left_coeff_1 = field t "left_coeff[1]" int16_t
    let left_coeff_2 = field t "left_coeff[2]" int16_t
    let deadband_0 = field t "deadband[0]" int_as_uint16_t
    let deadband_1 = field t "deadband[1]" int_as_uint16_t
    let deadband_2 = field t "deadband[2]" int_as_uint16_t
    let center_0 = field t "center[0]" int16_t
    let center_1 = field t "center[1]" int16_t
    let center_2 = field t "center[2]" int16_t
    let () = seal t
  end

  module Ramp = struct
    type t
    let t : t structure typ = structure "SDL_HapticRamp"
    let typ = field t "type" int_as_uint16_t
    let direction = field t "direction" Direction.t
    let length = field t "length" int32_as_uint32_t
    let delay = field t "delay" int_as_uint16_t
    let button = field t "button" int_as_uint16_t
    let interval = field t "interval" int_as_uint16_t

    let start = field t "start" int16_t
    let end_ = field t "end" int16_t
    let attack_length = field t "attack_length" int_as_uint16_t
    let attack_level = field t "attack_level" int_as_uint16_t
    let fade_length = field t "fade_length" int_as_uint16_t
    let fade_level = field t "fade_level" int_as_uint16_t
    let () = seal t
  end

  module Left_right = struct
    type t
    let t : t structure typ = structure "SDL_HapticLeftRight"
    let typ = field t "type" int_as_uint16_t
    let direction = field t "direction" Direction.t
    let length = field t "length" int32_as_uint32_t

    let large_magnitude = field t "large_magnitude" int_as_uint16_t
    let small_magnitude = field t "small_magnitude" int_as_uint16_t
    let () = seal t
  end

  module Custom = struct
    let int_list_as_uint16_t_ptr =
      let read _ = invalid_arg err_read_field in
      let write l =
        let l = List.map Unsigned.UInt16.of_int l in
        let a = CArray.of_list uint16_t l in
        CArray.start a
      in
      view ~read ~write (ptr uint16_t)

    type t
    let t : t structure typ = structure "SDL_HapticCustom"
    let typ = field t "type" int_as_uint16_t
    let direction = field t "direction" Direction.t
    let length = field t "length" int32_as_uint32_t
    let delay = field t "delay" int_as_uint16_t
    let button = field t "button" int_as_uint16_t
    let interval = field t "interval" int_as_uint16_t

    let channels = field t "channels" int_as_uint8_t
    let period = field t "period" int_as_uint16_t
    let samples = field t "samples" int_as_uint16_t
    let data = field t "data" int_list_as_uint16_t_ptr
    let attack_length = field t "attack_length" int_as_uint16_t
    let attack_level = field t "attack_level" int_as_uint16_t
    let fade_length = field t "fade_length" int_as_uint16_t
    let fade_level = field t "fade_level" int_as_uint16_t
    let () = seal t
  end

  module Effect = struct
    type t
    let t : t union typ = union "SDL_HapticEffect"
    let typ = field t "type" int_as_uint16_t
    let constant = field t "constant" Constant.t
    let periodic = field t "periodic" Periodic.t
    let condition = field t "condition" Condition.t
    let ramp = field t "ramp" Ramp.t
    let left_right = field t "condition" Left_right.t
    let custom = field t "custom" Custom.t
    let () = seal t
  end

  type effect_type = int

  let create_effect () = make Effect.t

  type _ field =
      F : (* existential to hide the 'a structure *)
        (('a structure, Effect.t union) Ctypes.field *
         ('b, 'a structure) Ctypes.field) -> 'b field

  let get e (F (s, f)) = getf (getf e s) f
  let set e (F (s, f)) v = setf (getf e s) f v
  let typ = F (Effect.constant, Constant.typ) (* same in each enum *)

  (* Constant *)
  let constant = sdl_haptic_constant

  let constant_type = F (Effect.constant, Constant.typ)
  let constant_direction = F (Effect.constant, Constant.direction)
  let constant_length = F (Effect.constant, Constant.length)
  let constant_delay = F (Effect.constant, Constant.delay)
  let constant_button = F (Effect.constant, Constant.button)
  let constant_interval = F (Effect.constant, Constant.interval)
  let constant_level = F (Effect.constant, Constant.level)
  let constant_attack_length = F (Effect.constant, Constant.attack_length)
  let constant_attack_level = F (Effect.constant, Constant.attack_level)
  let constant_fade_length = F (Effect.constant, Constant.fade_length)
  let constant_fade_level = F (Effect.constant, Constant.fade_level)

  (* Periodic *)

  let sine = sdl_haptic_sine
  let left_right = sdl_haptic_leftright
  let triangle = sdl_haptic_triangle
  let sawtooth_up = sdl_haptic_sawtoothup
  let sawtooth_down = sdl_haptic_sawtoothdown

  let periodic_type = F (Effect.periodic, Periodic.typ)
  let periodic_direction = F (Effect.periodic, Periodic.direction)
  let periodic_length = F (Effect.periodic, Periodic.length)
  let periodic_delay = F (Effect.periodic, Periodic.delay)
  let periodic_button = F (Effect.periodic, Periodic.button)
  let periodic_interval = F (Effect.periodic, Periodic.interval)
  let periodic_period = F (Effect.periodic, Periodic.period)
  let periodic_magnitude = F (Effect.periodic, Periodic.magnitude)
  let periodic_offset = F (Effect.periodic, Periodic.offset)
  let periodic_phase = F (Effect.periodic, Periodic.phase)
  let periodic_attack_length = F (Effect.periodic, Periodic.attack_length)
  let periodic_attack_level = F (Effect.periodic, Periodic.attack_level)
  let periodic_fade_length = F (Effect.periodic, Periodic.fade_length)
  let periodic_fade_level = F (Effect.periodic, Periodic.fade_level)

  (* Condition *)

  let spring = sdl_haptic_spring
  let damper = sdl_haptic_damper
  let inertia = sdl_haptic_inertia
  let friction = sdl_haptic_friction

  let condition_type = F (Effect.condition, Condition.typ)
  let condition_direction = F (Effect.condition, Condition.direction)
  let condition_length = F (Effect.condition, Condition.length)
  let condition_delay = F (Effect.condition, Condition.delay)
  let condition_button = F (Effect.condition, Condition.button)
  let condition_interval = F (Effect.condition, Condition.interval)
  let condition_right_sat_0 = F (Effect.condition, Condition.right_sat_0)
  let condition_right_sat_1 = F (Effect.condition, Condition.right_sat_1)
  let condition_right_sat_2 = F (Effect.condition, Condition.right_sat_2)
  let condition_left_sat_0 = F (Effect.condition, Condition.left_sat_0)
  let condition_left_sat_1 = F (Effect.condition, Condition.left_sat_1)
  let condition_left_sat_2 = F (Effect.condition, Condition.left_sat_2)
  let condition_right_coeff_0 = F (Effect.condition, Condition.right_coeff_0)
  let condition_right_coeff_1 = F (Effect.condition, Condition.right_coeff_1)
  let condition_right_coeff_2 = F (Effect.condition, Condition.right_coeff_2)
  let condition_left_coeff_0 = F (Effect.condition, Condition.left_coeff_0)
  let condition_left_coeff_1 = F (Effect.condition, Condition.left_coeff_1)
  let condition_left_coeff_2 = F (Effect.condition, Condition.left_coeff_2)
  let condition_deadband_0 = F (Effect.condition, Condition.deadband_0)
  let condition_deadband_1 = F (Effect.condition, Condition.deadband_1)
  let condition_deadband_2 = F (Effect.condition, Condition.deadband_2)
  let condition_center_0 = F (Effect.condition, Condition.center_0)
  let condition_center_1 = F (Effect.condition, Condition.center_1)
  let condition_center_2 = F (Effect.condition, Condition.center_2)

  (* Ramp *)

  let ramp = sdl_haptic_ramp

  let ramp_type = F (Effect.ramp, Ramp.typ)
  let ramp_direction = F (Effect.ramp, Ramp.direction)
  let ramp_length = F (Effect.ramp, Ramp.length)
  let ramp_delay = F (Effect.ramp, Ramp.delay)
  let ramp_button = F (Effect.ramp, Ramp.button)
  let ramp_interval = F (Effect.ramp, Ramp.interval)
  let ramp_start = F (Effect.ramp, Ramp.start)
  let ramp_end = F (Effect.ramp, Ramp.end_)
  let ramp_attack_length = F (Effect.ramp, Ramp.attack_length)
  let ramp_attack_level = F (Effect.ramp, Ramp.attack_level)
  let ramp_fade_length = F (Effect.ramp, Ramp.fade_length)
  let ramp_fade_level = F (Effect.ramp, Ramp.fade_level)

  (* Left right *)

  let left_right_type = F (Effect.left_right, Left_right.typ)
  let left_right_length = F (Effect.left_right, Left_right.length)
  let left_right_large_magnitude =
    F (Effect.left_right, Left_right.large_magnitude)
  let left_right_small_magnitude =
    F (Effect.left_right, Left_right.small_magnitude)

  (* Custom *)

  let custom = sdl_haptic_custom

  let custom_type = F (Effect.custom, Custom.typ)
  let custom_direction = F (Effect.custom, Custom.direction)
  let custom_length = F (Effect.custom, Custom.length)
  let custom_delay = F (Effect.custom, Custom.delay)
  let custom_button = F (Effect.custom, Custom.button)
  let custom_interval = F (Effect.custom, Custom.interval)
  let custom_channels = F (Effect.custom, Custom.channels)
  let custom_period = F (Effect.custom, Custom.period)
  let custom_samples = F (Effect.custom, Custom.samples)
  let custom_data = F (Effect.custom, Custom.data)
  let custom_attack_length = F (Effect.custom, Custom.attack_length)
  let custom_attack_level = F (Effect.custom, Custom.attack_level)
  let custom_fade_length = F (Effect.custom, Custom.fade_length)
  let custom_fade_level = F (Effect.custom, Custom.fade_level)
end

type haptic_effect = Haptic.Effect.t union

type haptic_effect_id = int
let haptic_effect_id : int typ = int

let haptic_close =
  foreign "SDL_HapticClose" (haptic @-> returning void)

let haptic_destroy_effect =
  foreign "SDL_HapticDestroyEffect"
    (haptic @-> int @-> returning void)

let haptic_effect_supported =
  foreign "SDL_HapticEffectSupported"
    (haptic @-> ptr Haptic.Effect.t @-> returning bool_to_ok)

let haptic_effect_supported h e =
  haptic_effect_supported h (addr e)

let haptic_get_effect_status =
  foreign "SDL_HapticGetEffectStatus"
    (haptic @-> haptic_effect_id @-> returning bool_to_ok)

let haptic_index =
  foreign "SDL_HapticIndex" (haptic @-> returning nat_to_ok)

let haptic_name =
  foreign "SDL_HapticName" (int @-> returning (some_to_ok string_opt))

let haptic_new_effect =
  foreign "SDL_HapticNewEffect"
    (haptic @-> ptr Haptic.Effect.t @-> returning nat_to_ok)

let haptic_new_effect h e =
  haptic_new_effect h (addr e)

let haptic_num_axes =
  foreign "SDL_HapticNumAxes" (haptic @-> returning nat_to_ok)

let haptic_num_effects =
  foreign "SDL_HapticNumEffects" (haptic @-> returning nat_to_ok)

let haptic_num_effects_playing =
  foreign "SDL_HapticNumEffectsPlaying" (haptic @-> returning nat_to_ok)

let haptic_open =
  foreign "SDL_HapticOpen" (int @-> returning (some_to_ok haptic_opt))

let haptic_open_from_joystick =
  foreign "SDL_HapticOpenFromJoystick"
  (joystick @-> returning (some_to_ok haptic_opt))

let haptic_open_from_mouse =
  foreign "SDL_HapticOpenFromMouse"
    (void @-> returning (some_to_ok haptic_opt))

let haptic_opened =
  foreign "SDL_HapticOpened" (int @-> returning int)

let haptic_opened i = match haptic_opened i with
| 0 -> false | 1 -> true | _ -> assert false

let haptic_pause =
  foreign "SDL_HapticPause" (haptic @-> returning zero_to_ok)

let haptic_query =
  foreign "SDL_HapticQuery" (haptic @-> returning int)

let haptic_rumble_init =
  foreign "SDL_HapticRumbleInit" (haptic @-> returning zero_to_ok)

let haptic_rumble_play =
  foreign "SDL_HapticRumblePlay"
    (haptic @-> float @-> int32_t @-> returning zero_to_ok)

let haptic_rumble_stop =
  foreign "SDL_HapticRumbleStop" (haptic @-> returning zero_to_ok)

let haptic_rumble_supported =
  foreign "SDL_HapticRumbleSupported" (haptic @-> returning bool_to_ok)

let haptic_run_effect =
  foreign "SDL_HapticRunEffect"
    (haptic @-> haptic_effect_id  @-> int32_t @-> returning zero_to_ok)

let haptic_set_autocenter =
  foreign "SDL_HapticSetAutocenter" (haptic @-> int @-> returning zero_to_ok)

let haptic_set_gain =
  foreign "SDL_HapticSetGain" (haptic @-> int @-> returning zero_to_ok)

let haptic_stop_all =
  foreign "SDL_HapticStopAll" (haptic @-> returning zero_to_ok)

let haptic_stop_effect =
  foreign "SDL_HapticStopEffect"
    (haptic @-> haptic_effect_id @-> returning zero_to_ok)

let haptic_unpause =
  foreign "SDL_HapticUnpause" (haptic @-> returning zero_to_ok)

let haptic_update_effect =
  foreign "SDL_HapticUpdateEffect"
    (haptic @-> haptic_effect_id @-> ptr Haptic.Effect.t @->
     returning zero_to_ok)

let haptic_update_effect h id e =
  haptic_update_effect h id (addr e)

let joystick_is_haptic =
  foreign "SDL_JoystickIsHaptic"
    (joystick @-> returning bool_to_ok)

let mouse_is_haptic =
  foreign "SDL_MouseIsHaptic" (void @-> returning bool_to_ok)

let num_haptics =
  foreign "SDL_NumHaptics" (void @-> returning nat_to_ok)

(* Audio *)

(* Audio drivers *)

let audio_init =
  foreign "SDL_AudioInit" (string_opt @-> returning zero_to_ok)

let audio_quit =
  foreign "SDL_AudioQuit" (void @-> returning void)

let get_audio_driver =
  foreign "SDL_GetAudioDriver"
    (int @-> returning (some_to_ok string_opt))

let get_current_audio_driver =
  foreign "SDL_GetCurrentAudioDriver" (void @-> returning string_opt)

let get_num_audio_drivers =
  foreign "SDL_GetNumAudioDrivers" (void @-> returning nat_to_ok)

(* Audio devices *)

module Audio = struct
  type status = int
  let stopped = sdl_audio_stopped
  let playing = sdl_audio_playing
  let paused = sdl_audio_paused

  type format = int
  let format = int_as_uint16_t
  let s8 = audio_s8
  let u8 = audio_u8
  let s16_lsb = audio_s16lsb
  let s16_msb = audio_s16msb
  let s16_sys = audio_s16sys
  let s16 = audio_s16
  let u16_lsb = audio_u16lsb
  let u16_msb = audio_u16msb
  let u16_sys = audio_u16sys
  let u16 = audio_u16
  let s32_lsb = audio_s32lsb
  let s32_msb = audio_s32msb
  let s32_sys = audio_s32sys
  let s32 = audio_s32
  let f32_lsb = audio_f32lsb
  let f32_msb = audio_f32msb
  let f32_sys = audio_f32sys
  let f32 = audio_f32

  type allow = int
  let allow = int
  let allow_frequency_change = sdl_audio_allow_frequency_change
  let allow_format_change = sdl_audio_allow_format_change
  let allow_channels_change = sdl_audio_allow_channels_change
  let allow_any_change = sdl_audio_allow_any_change
end

type audio_device_id = int32
let audio_device_id = int32_as_uint32_t

type audio_callback =
  unit Ctypes_static.ptr -> Unsigned.uint8 Ctypes_static.ptr -> int -> unit

type audio_spec =
  { as_freq : int;
    as_format : Audio.format;
    as_channels : uint8;
    as_silence : uint8;
    as_samples : uint8;
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
  (ptr void @-> ptr uint8_t @-> int @-> returning void)

type _audio_spec
let audio_spec : _audio_spec structure typ = structure "SDL_AudioSpec"
let as_freq = field audio_spec "freq" int
let as_format = field audio_spec "format" Audio.format
let as_channels = field audio_spec "channels" int_as_uint8_t
let as_silence = field audio_spec "silence" int_as_uint8_t
let as_samples = field audio_spec "samples" int_as_uint16_t
let _ = field audio_spec "padding" uint16_t
let as_size = field audio_spec "size" int32_as_uint32_t
let as_callback =
  field audio_spec "callback"
    (funptr_opt ~thread_registration:true ~runtime_lock:true as_callback)

let as_userdata = field audio_spec "userdata" (ptr void)
let () = seal audio_spec

let audio_spec_of_c c =
  let as_freq = getf c as_freq in
  let as_format = getf c as_format in
  let as_channels = getf c as_channels in
  let as_silence = getf c as_silence in
  let as_samples = getf c as_samples in
  let as_size = getf c as_size in
  let as_callback = None in
  { as_freq; as_format; as_channels; as_silence; as_samples; as_size;
    as_callback; }

let audio_spec_to_c a =
  let c = make audio_spec in
  setf c as_freq a.as_freq;
  setf c as_format a.as_format;
  setf c as_channels a.as_channels;
  setf c as_silence a.as_silence; (* irrelevant *)
  setf c as_samples a.as_samples;
  setf c as_size a.as_size;       (* irrelevant *)
  setf c as_callback a.as_callback;
  setf c as_userdata null;
  c

let close_audio_device =
  foreign "SDL_CloseAudioDevice" (audio_device_id @-> returning void)

let free_wav =
  foreign "SDL_FreeWAV" (ptr void @-> returning void)

let free_wav ba =
  free_wav (to_voidp (bigarray_start array1 ba))

let get_audio_device_name =
  foreign "SDL_GetAudioDeviceName"
    (int @-> bool @-> returning (some_to_ok string_opt))

let get_audio_device_status =
  foreign "SDL_GetAudioDeviceStatus" (audio_device_id @-> returning int)

let get_num_audio_devices =
  foreign "SDL_GetNumAudioDevices" (bool @-> returning nat_to_ok)

let load_wav_rw =
  foreign ~release_runtime_lock:true "SDL_LoadWAV_RW"
    (rw_ops @-> int @-> ptr audio_spec @-> ptr (ptr void) @-> ptr uint32_t @->
     returning (some_to_ok (ptr_opt audio_spec)))

let load_wav_rw ops spec kind =
  let d = allocate (ptr void) null in
  let len = allocate uint32_t Unsigned.UInt32.zero in
  match load_wav_rw ops 0 (addr (audio_spec_to_c spec)) d len with
  | Error _ as e -> e
  | Ok r ->
      let rspec = audio_spec_of_c (!@ r) in
      let kind_size = ba_kind_byte_size kind in
      let len = Unsigned.UInt32.to_int (!@ len) in
      if len mod kind_size <> 0
      then invalid_arg (err_bigarray_data len kind_size)
      else
      let ba_size = len / kind_size in
      let ba_ptr = access_ptr_typ_of_ba_kind kind in
      let d = coerce (ptr void)  ba_ptr (!@ d) in
      Ok (rspec, bigarray_of_ptr array1 ba_size kind d)

let lock_audio_device =
  foreign "SDL_LockAudioDevice" (audio_device_id @-> returning void)

let open_audio_device =
  foreign "SDL_OpenAudioDevice"
    (string_opt @-> bool @-> ptr audio_spec @-> ptr audio_spec @->
     Audio.allow @-> returning int32_as_uint32_t)

let open_audio_device dev capture desired allow =
  let desiredc = audio_spec_to_c desired in
  let obtained = make audio_spec in
  match open_audio_device dev capture (addr desiredc) (addr obtained) allow
  with
  | id when id = Int32.zero -> error ()
  | id -> Ok (id,  audio_spec_of_c obtained)

let pause_audio_device =
  foreign "SDL_PauseAudioDevice" (audio_device_id @-> bool @-> returning void)

let unlock_audio_device =
  foreign "SDL_UnlockAudioDevice" (audio_device_id @-> returning void)

let queue_audio =
  foreign "SDL_QueueAudio"
    (audio_device_id @-> ptr void @-> int_as_uint32_t @-> returning zero_to_ok)

let queue_audio dev ba =
  let len = Bigarray.Array1.dim ba in
  let kind_size = ba_kind_byte_size (Bigarray.Array1.kind ba) in
  queue_audio dev (to_voidp (bigarray_start array1 ba)) (len * kind_size)

let dequeue_audio =
  foreign "SDL_DequeueAudio"
    (audio_device_id @-> ptr void @-> int @-> returning int_as_uint32_t)

let dequeue_audio dev ba =
  let len = Bigarray.Array1.dim ba in
  let kind_size = ba_kind_byte_size (Bigarray.Array1.kind ba) in
  dequeue_audio dev (to_voidp (bigarray_start array1 ba)) (len * kind_size)

let get_queued_audio_size =
  foreign "SDL_GetQueuedAudioSize"
    (audio_device_id @-> returning int_as_uint32_t)

let clear_queued_audio =
  foreign "SDL_ClearQueuedAudio" (audio_device_id @-> returning void)

(* Timer *)

let delay =
  foreign ~release_runtime_lock:true "SDL_Delay" (int32_t @-> returning void)

let get_ticks =
  foreign "SDL_GetTicks" (void @-> returning int32_t)

let get_ticks64 =
  foreign "SDL_GetTicks64" (void @-> returning int64_t)

let get_performance_counter =
  foreign "SDL_GetPerformanceCounter" (void @-> returning int64_t)

let get_performance_frequency =
  foreign "SDL_GetPerformanceFrequency" (void @-> returning int64_t)

(* Platform and CPU information *)

let get_platform =
  foreign "SDL_GetPlatform" (void @-> returning string)

let get_cpu_cache_line_size =
  foreign "SDL_GetCPUCacheLineSize" (void @-> returning nat_to_ok)

let get_cpu_count =
  foreign "SDL_GetCPUCount" (void @-> returning int)

let get_system_ram =
  foreign "SDL_GetSystemRAM" (void @-> returning int)

let has_3d_now =
  foreign "SDL_Has3DNow" (void @-> returning bool)

let has_altivec =
  foreign "SDL_HasAltiVec" (void @-> returning bool)

let has_avx =
  foreign ~stub "SDL_HasAVX" (void @-> returning bool)

let has_avx2 =
  foreign  "SDL_HasAVX2" (void @-> returning bool)

let has_mmx =
  foreign "SDL_HasMMX" (void @-> returning bool)

let has_neon =
  foreign "SDL_HasNEON" (void @-> returning bool)

let has_rdtsc =
  foreign "SDL_HasRDTSC" (void @-> returning bool)

let has_sse =
  foreign "SDL_HasSSE" (void @-> returning bool)

let has_sse2 =
  foreign "SDL_HasSSE2" (void @-> returning bool)

let has_sse3 =
  foreign "SDL_HasSSE3" (void @-> returning bool)

let has_sse41 =
  foreign "SDL_HasSSE41" (void @-> returning bool)

let has_sse42 =
  foreign "SDL_HasSSE42" (void @-> returning bool)

(* Power management *)

type power_state =
  [ `Unknown | `On_battery | `No_battery | `Charging | `Charged ]

let power_state =
  [ sdl_powerstate_unknown, `Unknown;
    sdl_powerstate_on_battery, `On_battery;
    sdl_powerstate_no_battery, `No_battery;
    sdl_powerstate_charging, `Charging;
    sdl_powerstate_charged, `Charged; ]

type power_info =
  { pi_state : power_state;
    pi_secs : int option;
    pi_pct : int option; }

let get_power_info =
  foreign "SDL_GetPowerInfo" ((ptr int) @-> (ptr int) @-> returning int)

let get_power_info () =
  let secs = allocate int 0 in
  let pct = allocate int 0 in
  let s = get_power_info secs pct in
  let pi_state = try List.assoc s power_state with Not_found -> assert false in
  let pi_secs = match !@ secs with -1 -> None | secs -> Some secs in
  let pi_pct = match !@ pct with -1 -> None | pct -> Some pct in
  { pi_state; pi_secs; pi_pct }
end
