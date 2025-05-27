(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Formatters *)

open Tsdl

let button_state_str = function
| s when s = Sdl.pressed -> "pressed"
| s when s = Sdl.released -> "released"
| _ -> assert false

let pp = Format.fprintf
let pp_int = Format.pp_print_int
let pp_str = Format.pp_print_string
let pp_ipair ppf (x, y) = pp ppf "(%d %d)" x y
let pp_opt pp_v ppf v = match v with
| None -> pp ppf "None" | Some v -> pp ppf "(Some %a)" pp_v v

let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

let pp_unknown pp_v ppf v = match v with
| None -> pp ppf "unknown" | Some v -> pp_v ppf v

let pp_point ppf p =
  pp ppf "@[<1>(%d %d)>@]" (Sdl.Point.x p) (Sdl.Point.y p)

let pp_rect ppf r =
  pp ppf "@[<1><rect (%d %d) (%d %d)>@]"
    (Sdl.Rect.x r) (Sdl.Rect.y r) (Sdl.Rect.w r) (Sdl.Rect.h r)

let pp_color ppf c =
  pp ppf "@[<1><color %d %d %d %d>@]"
    (Sdl.Color.r c) (Sdl.Color.g c) (Sdl.Color.b c) (Sdl.Color.a c)

let pp_render_info ppf i =
  pp ppf "@[<v>@[%s@]@,%a@,@[max tex size %dx%d@]@]"
    i.Sdl.ri_name (pp_list Format.pp_print_string)
    (List.map Sdl.get_pixel_format_name i.Sdl.ri_texture_formats)
    i.Sdl.ri_max_texture_width
    i.Sdl.ri_max_texture_height

let pp_hz ppf v = pp ppf "%dHz" v
let pp_display_mode ppf m =
  pp ppf "@[<1>format:%s@ %dx%d@ @@ %a@]"
    (Sdl.get_pixel_format_name m.Sdl.dm_format)
    m.Sdl.dm_w m.Sdl.dm_h (pp_unknown pp_hz)
    m.Sdl.dm_refresh_rate

let pp_controller_axis_event ppf e =
  pp ppf "@[<1>controller_axis_event which:%ld@ axis:%d value:%d@]"
    Sdl.Event.(get e controller_axis_which)
    Sdl.Event.(get e controller_axis_axis)
    Sdl.Event.(get e controller_axis_value)

let pp_controller_button_event ppf e =
  pp ppf "@[<1>controller_button_event which:%ld@ button:%d state:%s@]"
    Sdl.Event.(get e controller_button_which)
    Sdl.Event.(get e controller_button_button)
    (button_state_str Sdl.Event.(get e controller_button_state))

let pp_controller_device_event ppf e =
  pp ppf "@[<1>controller_device_event %s which:%ld@ @]"
    Sdl.Event.(if get e typ = controller_device_added then "add" else
               if get e typ = controller_device_remapped then "remap" else
               if get e typ = controller_device_removed then "rem" else
               assert false)
    Sdl.Event.(get e controller_device_which)

let pp_dollar_gesture_event ppf e =
  pp ppf "@[<1>dollar_gesture_event touch_id:%Ld@ gesture_id:%Ld@ \
               num_fingers:%d@ error:%g@ (%g,%g)@]"
    Sdl.Event.(get e dollar_gesture_touch_id)
    Sdl.Event.(get e dollar_gesture_gesture_id)
    Sdl.Event.(get e dollar_gesture_num_fingers)
    Sdl.Event.(get e dollar_gesture_error)
    Sdl.Event.(get e dollar_gesture_x)
    Sdl.Event.(get e dollar_gesture_y)

let pp_drop_event ppf e =
  pp ppf "@[<1>drop_event file:%a@]"
    (pp_opt pp_str) Sdl.Event.(drop_file_file e)

let pp_touch_finger_event ppf e =
  pp ppf "@[<1>touch_finger_event %s touch_id:%Ld@ finger_id:%Ld@ (%g,%g)@ \
               rel:(%g,%g)@ pressure:%g"
    Sdl.Event.(if get e typ = finger_down then "down" else
               if get e typ = finger_motion then "motion" else
               if get e typ = finger_up then "up" else assert false)
    Sdl.Event.(get e touch_finger_touch_id)
    Sdl.Event.(get e touch_finger_finger_id)
    Sdl.Event.(get e touch_finger_x)
    Sdl.Event.(get e touch_finger_y)
    Sdl.Event.(get e touch_finger_dx)
    Sdl.Event.(get e touch_finger_dy)
    Sdl.Event.(get e touch_finger_pressure)

let pp_joy_axis_event ppf e =
  pp ppf "@[<1>joy_axis_event which:%ld@ axis:%d value:%d@]"
    Sdl.Event.(get e joy_axis_which)
    Sdl.Event.(get e joy_axis_axis)
    Sdl.Event.(get e joy_axis_value)

let pp_joy_ball_event ppf e =
  pp ppf "@[<1>joy_ball_event which:%ld@ ball:%d (%d,%d)@]"
    Sdl.Event.(get e joy_ball_which)
    Sdl.Event.(get e joy_ball_ball)
    Sdl.Event.(get e joy_ball_xrel)
    Sdl.Event.(get e joy_ball_yrel)

let pp_joy_button_event ppf e =
  pp ppf "@[<1>joy_button_event which:%ld@ button:%d state:%s@]"
    Sdl.Event.(get e joy_button_which)
    Sdl.Event.(get e joy_button_button)
    (button_state_str Sdl.Event.(get e joy_button_state))

let pp_joy_device_event ppf e =
  pp ppf "@[<1>joy_device_event %s which:%ld@ @]"
    Sdl.Event.(if get e typ = joy_device_added then "add" else "rem")
    Sdl.Event.(get e joy_device_which)

let pp_joy_hat_event ppf e =
  pp ppf "@[<1>joy_hat_event which:%ld@ hat:%d value:%d@]"
    Sdl.Event.(get e joy_hat_which)
    Sdl.Event.(get e joy_hat_hat)
    Sdl.Event.(get e joy_hat_value)

let pp_keyboard_event ppf e =
  pp ppf "@[<1>keyboard_event@ window_id:%d@ state:%s@ repeat:%b@ \
               scancode:%s@ keycode:%s@ keymod:%d@]"
    Sdl.Event.(get e keyboard_window_id)
    (button_state_str Sdl.Event.(get e keyboard_state))
    Sdl.Event.(get e keyboard_repeat > 0)
    Sdl.(get_scancode_name Event.(get e keyboard_scancode))
    Sdl.(get_key_name Event.(get e keyboard_keycode))
    Sdl.Event.(get e keyboard_keymod)

let pp_mouse_button_event ppf e =
  pp ppf "@[<1>mouse_button_event window_id:%d@ which:%ld@ button:%d@ \
               state:%s@ (%d,%d)@]"
    Sdl.Event.(get e mouse_button_window_id)
    Sdl.Event.(get e mouse_button_which)
    Sdl.Event.(get e mouse_button_button)
    (button_state_str Sdl.Event.(get e mouse_button_state))
    Sdl.Event.(get e mouse_button_x)
    Sdl.Event.(get e mouse_button_y)

let pp_mouse_motion_event ppf e =
  pp ppf "@[<1>mouse_motion_event window_id:%d@ which:%ld@ state:%ld@ \
               (%d,%d)@ rel:(%d,%d)@]"
    Sdl.Event.(get e mouse_motion_window_id)
    Sdl.Event.(get e mouse_motion_which)
    Sdl.Event.(get e mouse_motion_state)
    Sdl.Event.(get e mouse_motion_x)
    Sdl.Event.(get e mouse_motion_y)
    Sdl.Event.(get e mouse_motion_xrel)
    Sdl.Event.(get e mouse_motion_yrel)

let pp_mouse_wheel_direction ppf x =
  if x = Sdl.Event.mouse_wheel_normal then
    pp ppf "normal"
  else if x = Sdl.Event.mouse_wheel_flipped then
    pp ppf "flipped"
  else
  assert false


let pp_mouse_wheel_event ppf e =
  pp ppf "@[<1>mouse_wheel_event window_id:%d@ which:%ld@ (%d,%d) %a @]"
    Sdl.Event.(get e mouse_wheel_window_id)
    Sdl.Event.(get e mouse_wheel_which)
    Sdl.Event.(get e mouse_wheel_x)
    Sdl.Event.(get e mouse_wheel_y)
    pp_mouse_wheel_direction Sdl.Event.(get e mouse_wheel_direction)

let pp_multi_gesture_event ppf e =
  pp ppf "@[<1>multi_gesture_event touch_id:%Ld@ dtheta:%f@ ddist:%f@ \
               (%f,%f)@ num_fingers:%d@]"
    Sdl.Event.(get e multi_gesture_touch_id)
    Sdl.Event.(get e multi_gesture_dtheta)
    Sdl.Event.(get e multi_gesture_ddist)
    Sdl.Event.(get e multi_gesture_x)
    Sdl.Event.(get e multi_gesture_y)
    Sdl.Event.(get e multi_gesture_num_fingers)

let pp_text_editing_event ppf e =
  pp ppf "@[<1>text_editing_event window_id:%d@ text:'%s'@ start:%d @len:%d@]"
    Sdl.Event.(get e text_editing_window_id)
    Sdl.Event.(get e text_editing_text)
    Sdl.Event.(get e text_editing_start)
    Sdl.Event.(get e text_editing_length)

let pp_text_input_event ppf e =
  pp ppf "@[<1>text_input_event window_id:%d@ text:'%s'@]"
    Sdl.Event.(get e text_input_window_id)
    Sdl.Event.(get e text_input_text)

let pp_user_event ppf e =
  pp ppf "@[<1>user_event window_id:%d code:%d@]"
    Sdl.Event.(get e user_window_id)
    Sdl.Event.(get e user_code)

let pp_window_event ppf e =
  let event_id_str id =
    try List.assoc id [
        Sdl.Event.window_event_shown, "window_event_shown";
        Sdl.Event.window_event_hidden, "window_event_hidden";
        Sdl.Event.window_event_exposed, "window_event_exposed";
        Sdl.Event.window_event_moved, "window_event_moved";
        Sdl.Event.window_event_resized, "window_event_resized";
        Sdl.Event.window_event_size_changed, "window_event_size_changed";
        Sdl.Event.window_event_minimized, "window_event_minimized";
        Sdl.Event.window_event_maximized, "window_event_maximized";
        Sdl.Event.window_event_restored, "window_event_restored";
        Sdl.Event.window_event_enter, "window_event_enter";
        Sdl.Event.window_event_leave, "window_event_leave";
        Sdl.Event.window_event_focus_gained, "window_event_focus_gained";
        Sdl.Event.window_event_focus_lost, "window_event_focus_lost";
        Sdl.Event.window_event_close, "window_event_close"; ]
    with Not_found -> "unkown"
  in
  pp ppf "@[<1>window_event@ %s window_id:%d@ (%ld,%ld)@]"
    (event_id_str Sdl.Event.(get e window_event_id))
    Sdl.Event.(get e window_window_id)
    Sdl.Event.(get e window_data1)
    Sdl.Event.(get e window_data2)

let cst s ppf _e = pp ppf "%s" s
let event_pp e =
  try List.assoc (Sdl.Event.(get e typ)) [
      Sdl.Event.app_did_enter_background, cst "app_did_enter_background";
      Sdl.Event.app_did_enter_foreground, cst "app_did_enter_foreground";
      Sdl.Event.app_low_memory, cst "app_lowmemory";
      Sdl.Event.app_terminating, cst "app_terminating";
      Sdl.Event.app_will_enter_background, cst "app_willenterbackground";
      Sdl.Event.app_will_enter_foreground, cst "app_will_enter_foreground";
      Sdl.Event.clipboard_update, cst "clipboard_update";
      Sdl.Event.controller_axis_motion, pp_controller_axis_event;
      Sdl.Event.controller_button_down, pp_controller_button_event;
      Sdl.Event.controller_button_up, pp_controller_button_event;
      Sdl.Event.controller_device_added, pp_controller_device_event;
      Sdl.Event.controller_device_remapped, pp_controller_device_event;
      Sdl.Event.controller_device_removed, pp_controller_device_event;
      Sdl.Event.dollar_gesture, pp_dollar_gesture_event;
      Sdl.Event.dollar_record, cst "dollar_record";
      Sdl.Event.drop_file, pp_drop_event;
      Sdl.Event.finger_down, pp_touch_finger_event;
      Sdl.Event.finger_motion, pp_touch_finger_event;
      Sdl.Event.finger_up, pp_touch_finger_event;
      Sdl.Event.joy_axis_motion, pp_joy_axis_event;
      Sdl.Event.joy_ball_motion, pp_joy_ball_event;
      Sdl.Event.joy_button_down, pp_joy_button_event;
      Sdl.Event.joy_button_up, pp_joy_button_event;
      Sdl.Event.joy_device_added, pp_joy_device_event;
      Sdl.Event.joy_device_removed, pp_joy_device_event;
      Sdl.Event.joy_hat_motion, pp_joy_hat_event;
      Sdl.Event.key_down, pp_keyboard_event;
      Sdl.Event.key_up, pp_keyboard_event;
      Sdl.Event.mouse_button_down, pp_mouse_button_event;
      Sdl.Event.mouse_button_up, pp_mouse_button_event;
      Sdl.Event.mouse_motion, pp_mouse_motion_event;
      Sdl.Event.mouse_wheel, pp_mouse_wheel_event;
      Sdl.Event.multi_gesture, pp_multi_gesture_event;
      Sdl.Event.quit, cst "quit";
      Sdl.Event.sys_wm_event, cst "sys_wm_event";
      Sdl.Event.text_editing, pp_text_editing_event;
      Sdl.Event.text_input, pp_text_input_event;
      Sdl.Event.user_event, pp_user_event;
      Sdl.Event.window_event, pp_window_event;
      Sdl.Event.first_event, cst "firstevent";
      Sdl.Event.last_event, cst "last_event"; ]
  with Not_found -> cst "unknown"

let pp_event ppf e = pp ppf "%a" (event_pp e) e

let pp_joystick_power_level ppf lvl =
  let open Sdl.Joystick_power_level in
  pp ppf "%s" (List.assoc lvl
                 [low, "low"; medium, "medium"; full, "full"; wired, "wired";
                  max, "max"; unknown, "unknown"]
              )

let pp_joystick_type ppf ty =
  let open Sdl.Joystick_type in
  pp ppf "%s" (List.assoc ty
                 [unknown,"unknown"; gamecontroller, "gamecontroller";
                  wheel,"wheel";arcade_stick,"arcade_stick";
                  flight_stick, "flight_stick";
                  dance_pad,"dance_pad";guitar,"guitar";drum_kit, "drum_kit";
                  arcade_pad,"arcade_pad"; throttle, "throttle" ]
              )
