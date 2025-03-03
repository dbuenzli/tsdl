/*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>
#include "SDL.h"
#include <caml/mlvalues.h>

void let (FILE *fd, const char *symb)
{
  int i;
  fprintf (fd, "let ");
  for (i = 0; i < strlen(symb); i++) { fprintf (fd, "%c", tolower (symb[i])); }
}

void str_v (FILE *fd, char *symb, const char *value)
{ let (fd, symb); fprintf (fd, " = \"%s\"\n", value); }

void int_v (FILE *fd, char *symb, int value)
{ let (fd, symb); fprintf (fd, " = %d\n", value); }

void int_vx (FILE *fd, char *symb, int value)
{ let (fd, symb); fprintf (fd, " = 0x%X\n", value); }

void int32_v (FILE *fd, char *symb, int32_t value)
{ let (fd, symb); fprintf (fd, " = 0x%Xl\n", value); }

void consts (FILE *fd)
{

#define int_v(e) int_v(fd, "" # e, (int)e)
#define int_vx(e) int_vx(fd, "" # e, (int)e)
#define int32_v(e) int32_v(fd, "" # e, (int32_t)e)
#define str_v(e) str_v(fd, "" # e, (const char *)e)

  /* Check that a C int is 32 bits. We rely on that when we pass
     arrays of Point or Rect structs as bigarrays (e.g. see
     Sdl.enclose_points_ba */

  assert(sizeof(int) == 4);

  /* Init */

  int_v (SDL_INIT_TIMER);
  int_v (SDL_INIT_AUDIO);
  int_v (SDL_INIT_VIDEO);
  int_v (SDL_INIT_JOYSTICK);
  int_v (SDL_INIT_HAPTIC);
  int_v (SDL_INIT_GAMECONTROLLER);
  int_v (SDL_INIT_EVENTS);
  int_v (SDL_INIT_EVERYTHING);
  int_v (SDL_INIT_NOPARACHUTE);

  /* Hint */

  str_v (SDL_HINT_FRAMEBUFFER_ACCELERATION);
  str_v (SDL_HINT_IDLE_TIMER_DISABLED);
  str_v (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH);
  str_v (SDL_HINT_ORIENTATIONS);
  str_v (SDL_HINT_RENDER_DRIVER);
  str_v (SDL_HINT_RENDER_OPENGL_SHADERS);
  str_v (SDL_HINT_RENDER_LOGICAL_SIZE_MODE);
  str_v (SDL_HINT_RENDER_SCALE_QUALITY);
  str_v (SDL_HINT_RENDER_VSYNC);

  int_v (SDL_HINT_DEFAULT);
  int_v (SDL_HINT_NORMAL);
  int_v (SDL_HINT_OVERRIDE);

  str_v (SDL_HINT_NO_SIGNAL_HANDLERS);
  str_v (SDL_HINT_THREAD_STACK_SIZE);
  str_v (SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN);

  str_v (SDL_HINT_AUDIO_RESAMPLING_MODE);
  str_v (SDL_HINT_MOUSE_NORMAL_SPEED_SCALE);
  str_v (SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE);
  str_v (SDL_HINT_TOUCH_MOUSE_EVENTS);
  str_v (SDL_HINT_MOUSE_TOUCH_EVENTS);

  /* Log */

  int_v (SDL_LOG_CATEGORY_APPLICATION);
  int_v (SDL_LOG_CATEGORY_ERROR);
  int_v (SDL_LOG_CATEGORY_SYSTEM);
  int_v (SDL_LOG_CATEGORY_AUDIO);
  int_v (SDL_LOG_CATEGORY_VIDEO);
  int_v (SDL_LOG_CATEGORY_RENDER);
  int_v (SDL_LOG_CATEGORY_INPUT);
  int_v (SDL_LOG_CATEGORY_CUSTOM);

  int_v (SDL_LOG_PRIORITY_VERBOSE);
  int_v (SDL_LOG_PRIORITY_DEBUG);
  int_v (SDL_LOG_PRIORITY_INFO);
  int_v (SDL_LOG_PRIORITY_WARN);
  int_v (SDL_LOG_PRIORITY_ERROR);
  int_v (SDL_LOG_PRIORITY_CRITICAL);

  /* Pixel format */

  int_v (SDL_BLENDMODE_NONE);
  int_v (SDL_BLENDMODE_BLEND);
  int_v (SDL_BLENDMODE_ADD);
  int_v (SDL_BLENDMODE_MOD);

  int_v (SDL_BLENDOPERATION_ADD);
  int_v (SDL_BLENDOPERATION_SUBTRACT);
  int_v (SDL_BLENDOPERATION_REV_SUBTRACT);
  int_v (SDL_BLENDOPERATION_MAXIMUM);
  int_v (SDL_BLENDOPERATION_MINIMUM);

  int_v (SDL_BLENDFACTOR_ZERO);
  int_v (SDL_BLENDFACTOR_ONE);
  int_v (SDL_BLENDFACTOR_SRC_COLOR);
  int_v (SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR);
  int_v (SDL_BLENDFACTOR_SRC_ALPHA);
  int_v (SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA);
  int_v (SDL_BLENDFACTOR_DST_COLOR);
  int_v (SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR);
  int_v (SDL_BLENDFACTOR_DST_ALPHA);
  int_v (SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA);

  int32_v (SDL_PIXELFORMAT_UNKNOWN);
  int32_v (SDL_PIXELFORMAT_INDEX1LSB);
  int32_v (SDL_PIXELFORMAT_INDEX1MSB);
  int32_v (SDL_PIXELFORMAT_INDEX4LSB);
  int32_v (SDL_PIXELFORMAT_INDEX4MSB);
  int32_v (SDL_PIXELFORMAT_INDEX8);
  int32_v (SDL_PIXELFORMAT_RGB332);
  int32_v (SDL_PIXELFORMAT_RGB444);
  int32_v (SDL_PIXELFORMAT_RGB555);
  int32_v (SDL_PIXELFORMAT_BGR555);
  int32_v (SDL_PIXELFORMAT_ARGB4444);
  int32_v (SDL_PIXELFORMAT_RGBA4444);
  int32_v (SDL_PIXELFORMAT_ABGR4444);
  int32_v (SDL_PIXELFORMAT_BGRA4444);
  int32_v (SDL_PIXELFORMAT_ARGB1555);
  int32_v (SDL_PIXELFORMAT_RGBA5551);
  int32_v (SDL_PIXELFORMAT_ABGR1555);
  int32_v (SDL_PIXELFORMAT_BGRA5551);
  int32_v (SDL_PIXELFORMAT_RGB565);
  int32_v (SDL_PIXELFORMAT_BGR565);
  int32_v (SDL_PIXELFORMAT_RGB24);
  int32_v (SDL_PIXELFORMAT_BGR24);
  int32_v (SDL_PIXELFORMAT_RGB888);
  int32_v (SDL_PIXELFORMAT_RGBX8888);
  int32_v (SDL_PIXELFORMAT_BGR888);
  int32_v (SDL_PIXELFORMAT_BGRX8888);
  int32_v (SDL_PIXELFORMAT_ARGB8888);
  int32_v (SDL_PIXELFORMAT_RGBA8888);
  int32_v (SDL_PIXELFORMAT_ABGR8888);
  int32_v (SDL_PIXELFORMAT_BGRA8888);
  int32_v (SDL_PIXELFORMAT_ARGB2101010);
  int32_v (SDL_PIXELFORMAT_YV12);
  int32_v (SDL_PIXELFORMAT_IYUV);
  int32_v (SDL_PIXELFORMAT_YUY2);
  int32_v (SDL_PIXELFORMAT_UYVY);
  int32_v (SDL_PIXELFORMAT_YVYU);

  /* Renderer */

  int_v (SDL_FLIP_NONE);
  int_v (SDL_FLIP_HORIZONTAL);
  int_v (SDL_FLIP_VERTICAL);

  int_v (SDL_RENDERER_SOFTWARE);
  int_v (SDL_RENDERER_ACCELERATED);
  int_v (SDL_RENDERER_PRESENTVSYNC);
  int_v (SDL_RENDERER_TARGETTEXTURE);

  int_v (SDL_TEXTUREACCESS_STATIC);
  int_v (SDL_TEXTUREACCESS_STREAMING);
  int_v (SDL_TEXTUREACCESS_TARGET);

  int_v (SDL_TEXTUREMODULATE_NONE);
  int_v (SDL_TEXTUREMODULATE_COLOR);
  int_v (SDL_TEXTUREMODULATE_ALPHA);

  /* Windows and displays */

  int_v (SDL_WINDOW_FULLSCREEN);
  int_v (SDL_WINDOW_FULLSCREEN_DESKTOP);
  int_v (SDL_WINDOW_OPENGL);
  int_v (SDL_WINDOW_SHOWN);
  int_v (SDL_WINDOW_HIDDEN);
  int_v (SDL_WINDOW_BORDERLESS);
  int_v (SDL_WINDOW_RESIZABLE);
  int_v (SDL_WINDOW_MINIMIZED);
  int_v (SDL_WINDOW_MAXIMIZED);
  int_v (SDL_WINDOW_INPUT_GRABBED);
  int_v (SDL_WINDOW_INPUT_FOCUS);
  int_v (SDL_WINDOW_MOUSE_FOCUS);
  int_v (SDL_WINDOW_FOREIGN);
  int_v (SDL_WINDOW_ALLOW_HIGHDPI);
  int_v (SDL_WINDOW_MOUSE_CAPTURE);
  int_v (SDL_WINDOW_ALWAYS_ON_TOP);
  int_v (SDL_WINDOW_SKIP_TASKBAR);
  int_v (SDL_WINDOW_UTILITY);
  int_v (SDL_WINDOW_POPUP_MENU);
  int_v (SDL_WINDOW_VULKAN);

  int_v (SDL_WINDOWPOS_CENTERED);
  int_v (SDL_WINDOWPOS_UNDEFINED);

  /* Hit test */
  int_v (SDL_HITTEST_NORMAL);
  int_v (SDL_HITTEST_DRAGGABLE);
  int_v (SDL_HITTEST_RESIZE_TOPLEFT);
  int_v (SDL_HITTEST_RESIZE_TOP);
  int_v (SDL_HITTEST_RESIZE_TOPRIGHT);
  int_v (SDL_HITTEST_RESIZE_LEFT);
  int_v (SDL_HITTEST_RESIZE_RIGHT);
  int_v (SDL_HITTEST_RESIZE_BOTTOMLEFT);
  int_v (SDL_HITTEST_RESIZE_BOTTOM);
  int_v (SDL_HITTEST_RESIZE_BOTTOMRIGHT);

  /* SDL_GLcontextFlag */

  int_v (SDL_GL_CONTEXT_DEBUG_FLAG);
  int_v (SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG);
  int_v (SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG);
  int_v (SDL_GL_CONTEXT_RESET_ISOLATION_FLAG);
  int_v (SDL_GL_CONTEXT_RELEASE_BEHAVIOR);

  /* SDL_GLprofile */

  int_v (SDL_GL_CONTEXT_PROFILE_CORE);
  int_v (SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
  int_v (SDL_GL_CONTEXT_PROFILE_ES);

  /* SDL_GLattr */

  int_v (SDL_GL_RED_SIZE);
  int_v (SDL_GL_GREEN_SIZE);
  int_v (SDL_GL_BLUE_SIZE);
  int_v (SDL_GL_ALPHA_SIZE);
  int_v (SDL_GL_BUFFER_SIZE);
  int_v (SDL_GL_DOUBLEBUFFER);
  int_v (SDL_GL_DEPTH_SIZE);
  int_v (SDL_GL_STENCIL_SIZE);
  int_v (SDL_GL_ACCUM_RED_SIZE);
  int_v (SDL_GL_ACCUM_GREEN_SIZE);
  int_v (SDL_GL_ACCUM_BLUE_SIZE);
  int_v (SDL_GL_ACCUM_ALPHA_SIZE);
  int_v (SDL_GL_STEREO);
  int_v (SDL_GL_MULTISAMPLEBUFFERS);
  int_v (SDL_GL_MULTISAMPLESAMPLES);
  int_v (SDL_GL_ACCELERATED_VISUAL);
  int_v (SDL_GL_CONTEXT_MAJOR_VERSION);
  int_v (SDL_GL_CONTEXT_MINOR_VERSION);
  int_v (SDL_GL_CONTEXT_EGL);
  int_v (SDL_GL_CONTEXT_FLAGS);
  int_v (SDL_GL_CONTEXT_PROFILE_MASK);
  int_v (SDL_GL_SHARE_WITH_CURRENT_CONTEXT);
  int_v (SDL_GL_FRAMEBUFFER_SRGB_CAPABLE);

  /* Message box */

  int_v (SDL_MESSAGEBOX_ERROR);
  int_v (SDL_MESSAGEBOX_WARNING);
  int_v (SDL_MESSAGEBOX_INFORMATION);

  int_v (SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT);
  int_v (SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT);

  int_v (SDL_MESSAGEBOX_COLOR_BACKGROUND);
  int_v (SDL_MESSAGEBOX_COLOR_TEXT);
  int_v (SDL_MESSAGEBOX_COLOR_BUTTON_BORDER);
  int_v (SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND);
  int_v (SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED);
  int_v (SDL_MESSAGEBOX_COLOR_MAX);

  /* Keyboard */

  assert(sizeof (SDL_Keycode) == 4);
  assert(sizeof (SDL_Scancode) == 4);

  int_v (SDL_SCANCODE_UNKNOWN);
  int_v (SDL_SCANCODE_A);
  int_v (SDL_SCANCODE_B);
  int_v (SDL_SCANCODE_C);
  int_v (SDL_SCANCODE_D);
  int_v (SDL_SCANCODE_E);
  int_v (SDL_SCANCODE_F);
  int_v (SDL_SCANCODE_G);
  int_v (SDL_SCANCODE_H);
  int_v (SDL_SCANCODE_I);
  int_v (SDL_SCANCODE_J);
  int_v (SDL_SCANCODE_K);
  int_v (SDL_SCANCODE_L);
  int_v (SDL_SCANCODE_M);
  int_v (SDL_SCANCODE_N);
  int_v (SDL_SCANCODE_O);
  int_v (SDL_SCANCODE_P);
  int_v (SDL_SCANCODE_Q);
  int_v (SDL_SCANCODE_R);
  int_v (SDL_SCANCODE_S);
  int_v (SDL_SCANCODE_T);
  int_v (SDL_SCANCODE_U);
  int_v (SDL_SCANCODE_V);
  int_v (SDL_SCANCODE_W);
  int_v (SDL_SCANCODE_X);
  int_v (SDL_SCANCODE_Y);
  int_v (SDL_SCANCODE_Z);
  int_v (SDL_SCANCODE_1);
  int_v (SDL_SCANCODE_2);
  int_v (SDL_SCANCODE_3);
  int_v (SDL_SCANCODE_4);
  int_v (SDL_SCANCODE_5);
  int_v (SDL_SCANCODE_6);
  int_v (SDL_SCANCODE_7);
  int_v (SDL_SCANCODE_8);
  int_v (SDL_SCANCODE_9);
  int_v (SDL_SCANCODE_0);
  int_v (SDL_SCANCODE_RETURN);
  int_v (SDL_SCANCODE_ESCAPE);
  int_v (SDL_SCANCODE_BACKSPACE);
  int_v (SDL_SCANCODE_TAB);
  int_v (SDL_SCANCODE_SPACE);
  int_v (SDL_SCANCODE_MINUS);
  int_v (SDL_SCANCODE_EQUALS);
  int_v (SDL_SCANCODE_LEFTBRACKET);
  int_v (SDL_SCANCODE_RIGHTBRACKET);
  int_v (SDL_SCANCODE_BACKSLASH);
  int_v (SDL_SCANCODE_NONUSHASH);
  int_v (SDL_SCANCODE_SEMICOLON);
  int_v (SDL_SCANCODE_APOSTROPHE);
  int_v (SDL_SCANCODE_GRAVE);
  int_v (SDL_SCANCODE_COMMA);
  int_v (SDL_SCANCODE_PERIOD);
  int_v (SDL_SCANCODE_SLASH);
  int_v (SDL_SCANCODE_CAPSLOCK);
  int_v (SDL_SCANCODE_F1);
  int_v (SDL_SCANCODE_F2);
  int_v (SDL_SCANCODE_F3);
  int_v (SDL_SCANCODE_F4);
  int_v (SDL_SCANCODE_F5);
  int_v (SDL_SCANCODE_F6);
  int_v (SDL_SCANCODE_F7);
  int_v (SDL_SCANCODE_F8);
  int_v (SDL_SCANCODE_F9);
  int_v (SDL_SCANCODE_F10);
  int_v (SDL_SCANCODE_F11);
  int_v (SDL_SCANCODE_F12);
  int_v (SDL_SCANCODE_PRINTSCREEN);
  int_v (SDL_SCANCODE_SCROLLLOCK);
  int_v (SDL_SCANCODE_PAUSE);
  int_v (SDL_SCANCODE_INSERT);
  int_v (SDL_SCANCODE_HOME);
  int_v (SDL_SCANCODE_PAGEUP);
  int_v (SDL_SCANCODE_DELETE);
  int_v (SDL_SCANCODE_END);
  int_v (SDL_SCANCODE_PAGEDOWN);
  int_v (SDL_SCANCODE_RIGHT);
  int_v (SDL_SCANCODE_LEFT);
  int_v (SDL_SCANCODE_DOWN);
  int_v (SDL_SCANCODE_UP);
  int_v (SDL_SCANCODE_NUMLOCKCLEAR);
  int_v (SDL_SCANCODE_KP_DIVIDE);
  int_v (SDL_SCANCODE_KP_MULTIPLY);
  int_v (SDL_SCANCODE_KP_MINUS);
  int_v (SDL_SCANCODE_KP_PLUS);
  int_v (SDL_SCANCODE_KP_ENTER);
  int_v (SDL_SCANCODE_KP_1);
  int_v (SDL_SCANCODE_KP_2);
  int_v (SDL_SCANCODE_KP_3);
  int_v (SDL_SCANCODE_KP_4);
  int_v (SDL_SCANCODE_KP_5);
  int_v (SDL_SCANCODE_KP_6);
  int_v (SDL_SCANCODE_KP_7);
  int_v (SDL_SCANCODE_KP_8);
  int_v (SDL_SCANCODE_KP_9);
  int_v (SDL_SCANCODE_KP_0);
  int_v (SDL_SCANCODE_KP_PERIOD);
  int_v (SDL_SCANCODE_NONUSBACKSLASH);
  int_v (SDL_SCANCODE_APPLICATION);
  int_v (SDL_SCANCODE_KP_EQUALS);
  int_v (SDL_SCANCODE_F13);
  int_v (SDL_SCANCODE_F14);
  int_v (SDL_SCANCODE_F15);
  int_v (SDL_SCANCODE_F16);
  int_v (SDL_SCANCODE_F17);
  int_v (SDL_SCANCODE_F18);
  int_v (SDL_SCANCODE_F19);
  int_v (SDL_SCANCODE_F20);
  int_v (SDL_SCANCODE_F21);
  int_v (SDL_SCANCODE_F22);
  int_v (SDL_SCANCODE_F23);
  int_v (SDL_SCANCODE_F24);
  int_v (SDL_SCANCODE_EXECUTE);
  int_v (SDL_SCANCODE_HELP);
  int_v (SDL_SCANCODE_MENU);
  int_v (SDL_SCANCODE_SELECT);
  int_v (SDL_SCANCODE_STOP);
  int_v (SDL_SCANCODE_AGAIN);
  int_v (SDL_SCANCODE_UNDO);
  int_v (SDL_SCANCODE_CUT);
  int_v (SDL_SCANCODE_COPY);
  int_v (SDL_SCANCODE_PASTE);
  int_v (SDL_SCANCODE_FIND);
  int_v (SDL_SCANCODE_MUTE);
  int_v (SDL_SCANCODE_VOLUMEUP);
  int_v (SDL_SCANCODE_VOLUMEDOWN);
  int_v (SDL_SCANCODE_KP_COMMA);
  int_v (SDL_SCANCODE_KP_EQUALSAS400);
  int_v (SDL_SCANCODE_INTERNATIONAL1);
  int_v (SDL_SCANCODE_INTERNATIONAL2);
  int_v (SDL_SCANCODE_INTERNATIONAL3);
  int_v (SDL_SCANCODE_INTERNATIONAL4);
  int_v (SDL_SCANCODE_INTERNATIONAL5);
  int_v (SDL_SCANCODE_INTERNATIONAL6);
  int_v (SDL_SCANCODE_INTERNATIONAL7);
  int_v (SDL_SCANCODE_INTERNATIONAL8);
  int_v (SDL_SCANCODE_INTERNATIONAL9);
  int_v (SDL_SCANCODE_LANG1);
  int_v (SDL_SCANCODE_LANG2);
  int_v (SDL_SCANCODE_LANG3);
  int_v (SDL_SCANCODE_LANG4);
  int_v (SDL_SCANCODE_LANG5);
  int_v (SDL_SCANCODE_LANG6);
  int_v (SDL_SCANCODE_LANG7);
  int_v (SDL_SCANCODE_LANG8);
  int_v (SDL_SCANCODE_LANG9);
  int_v (SDL_SCANCODE_ALTERASE);
  int_v (SDL_SCANCODE_SYSREQ);
  int_v (SDL_SCANCODE_CANCEL);
  int_v (SDL_SCANCODE_CLEAR);
  int_v (SDL_SCANCODE_PRIOR);
  int_v (SDL_SCANCODE_RETURN2);
  int_v (SDL_SCANCODE_SEPARATOR);
  int_v (SDL_SCANCODE_OUT);
  int_v (SDL_SCANCODE_OPER);
  int_v (SDL_SCANCODE_CLEARAGAIN);
  int_v (SDL_SCANCODE_CRSEL);
  int_v (SDL_SCANCODE_EXSEL);
  int_v (SDL_SCANCODE_KP_00);
  int_v (SDL_SCANCODE_KP_000);
  int_v (SDL_SCANCODE_THOUSANDSSEPARATOR);
  int_v (SDL_SCANCODE_DECIMALSEPARATOR);
  int_v (SDL_SCANCODE_CURRENCYUNIT);
  int_v (SDL_SCANCODE_CURRENCYSUBUNIT);
  int_v (SDL_SCANCODE_KP_LEFTPAREN);
  int_v (SDL_SCANCODE_KP_RIGHTPAREN);
  int_v (SDL_SCANCODE_KP_LEFTBRACE);
  int_v (SDL_SCANCODE_KP_RIGHTBRACE);
  int_v (SDL_SCANCODE_KP_TAB);
  int_v (SDL_SCANCODE_KP_BACKSPACE);
  int_v (SDL_SCANCODE_KP_A);
  int_v (SDL_SCANCODE_KP_B);
  int_v (SDL_SCANCODE_KP_C);
  int_v (SDL_SCANCODE_KP_D);
  int_v (SDL_SCANCODE_KP_E);
  int_v (SDL_SCANCODE_KP_F);
  int_v (SDL_SCANCODE_KP_XOR);
  int_v (SDL_SCANCODE_KP_POWER);
  int_v (SDL_SCANCODE_KP_PERCENT);
  int_v (SDL_SCANCODE_KP_LESS);
  int_v (SDL_SCANCODE_KP_GREATER);
  int_v (SDL_SCANCODE_KP_AMPERSAND);
  int_v (SDL_SCANCODE_KP_DBLAMPERSAND);
  int_v (SDL_SCANCODE_KP_VERTICALBAR);
  int_v (SDL_SCANCODE_KP_DBLVERTICALBAR);
  int_v (SDL_SCANCODE_KP_COLON);
  int_v (SDL_SCANCODE_KP_HASH);
  int_v (SDL_SCANCODE_KP_SPACE);
  int_v (SDL_SCANCODE_KP_AT);
  int_v (SDL_SCANCODE_KP_EXCLAM);
  int_v (SDL_SCANCODE_KP_MEMSTORE);
  int_v (SDL_SCANCODE_KP_MEMRECALL);
  int_v (SDL_SCANCODE_KP_MEMCLEAR);
  int_v (SDL_SCANCODE_KP_MEMADD);
  int_v (SDL_SCANCODE_KP_MEMSUBTRACT);
  int_v (SDL_SCANCODE_KP_MEMMULTIPLY);
  int_v (SDL_SCANCODE_KP_MEMDIVIDE);
  int_v (SDL_SCANCODE_KP_PLUSMINUS);
  int_v (SDL_SCANCODE_KP_CLEAR);
  int_v (SDL_SCANCODE_KP_CLEARENTRY);
  int_v (SDL_SCANCODE_KP_BINARY);
  int_v (SDL_SCANCODE_KP_OCTAL);
  int_v (SDL_SCANCODE_KP_DECIMAL);
  int_v (SDL_SCANCODE_KP_HEXADECIMAL);
  int_v (SDL_SCANCODE_LCTRL);
  int_v (SDL_SCANCODE_LSHIFT);
  int_v (SDL_SCANCODE_LALT);
  int_v (SDL_SCANCODE_LGUI);
  int_v (SDL_SCANCODE_RCTRL);
  int_v (SDL_SCANCODE_RSHIFT);
  int_v (SDL_SCANCODE_RALT);
  int_v (SDL_SCANCODE_RGUI);
  int_v (SDL_SCANCODE_MODE);
  int_v (SDL_SCANCODE_AUDIONEXT);
  int_v (SDL_SCANCODE_AUDIOPREV);
  int_v (SDL_SCANCODE_AUDIOSTOP);
  int_v (SDL_SCANCODE_AUDIOPLAY);
  int_v (SDL_SCANCODE_AUDIOMUTE);
  int_v (SDL_SCANCODE_MEDIASELECT);
  int_v (SDL_SCANCODE_WWW);
  int_v (SDL_SCANCODE_MAIL);
  int_v (SDL_SCANCODE_CALCULATOR);
  int_v (SDL_SCANCODE_COMPUTER);
  int_v (SDL_SCANCODE_AC_SEARCH);
  int_v (SDL_SCANCODE_AC_HOME);
  int_v (SDL_SCANCODE_AC_BACK);
  int_v (SDL_SCANCODE_AC_FORWARD);
  int_v (SDL_SCANCODE_AC_STOP);
  int_v (SDL_SCANCODE_AC_REFRESH);
  int_v (SDL_SCANCODE_AC_BOOKMARKS);
  int_v (SDL_SCANCODE_BRIGHTNESSDOWN);
  int_v (SDL_SCANCODE_BRIGHTNESSUP);
  int_v (SDL_SCANCODE_DISPLAYSWITCH);
  int_v (SDL_SCANCODE_KBDILLUMTOGGLE);
  int_v (SDL_SCANCODE_KBDILLUMDOWN);
  int_v (SDL_SCANCODE_KBDILLUMUP);
  int_v (SDL_SCANCODE_EJECT);
  int_v (SDL_SCANCODE_SLEEP);
  int_v (SDL_SCANCODE_APP1);
  int_v (SDL_SCANCODE_APP2);
  int_v (SDL_NUM_SCANCODES);

  int_vx (SDLK_SCANCODE_MASK);
  int_vx (SDLK_UNKNOWN);
  int_vx (SDLK_RETURN);
  int_vx (SDLK_ESCAPE);
  int_vx (SDLK_BACKSPACE);
  int_vx (SDLK_TAB);
  int_vx (SDLK_SPACE);
  int_vx (SDLK_EXCLAIM);
  int_vx (SDLK_QUOTEDBL);
  int_vx (SDLK_HASH);
  int_vx (SDLK_PERCENT);
  int_vx (SDLK_DOLLAR);
  int_vx (SDLK_AMPERSAND);
  int_vx (SDLK_QUOTE);
  int_vx (SDLK_LEFTPAREN);
  int_vx (SDLK_RIGHTPAREN);
  int_vx (SDLK_ASTERISK);
  int_vx (SDLK_PLUS);
  int_vx (SDLK_COMMA);
  int_vx (SDLK_MINUS);
  int_vx (SDLK_PERIOD);
  int_vx (SDLK_SLASH);
  int_vx (SDLK_0);
  int_vx (SDLK_1);
  int_vx (SDLK_2);
  int_vx (SDLK_3);
  int_vx (SDLK_4);
  int_vx (SDLK_5);
  int_vx (SDLK_6);
  int_vx (SDLK_7);
  int_vx (SDLK_8);
  int_vx (SDLK_9);
  int_vx (SDLK_COLON);
  int_vx (SDLK_SEMICOLON);
  int_vx (SDLK_LESS);
  int_vx (SDLK_EQUALS);
  int_vx (SDLK_GREATER);
  int_vx (SDLK_QUESTION);
  int_vx (SDLK_AT);
  int_vx (SDLK_LEFTBRACKET);
  int_vx (SDLK_BACKSLASH);
  int_vx (SDLK_RIGHTBRACKET);
  int_vx (SDLK_CARET);
  int_vx (SDLK_UNDERSCORE);
  int_vx (SDLK_BACKQUOTE);
  int_vx (SDLK_a);
  int_vx (SDLK_b);
  int_vx (SDLK_c);
  int_vx (SDLK_d);
  int_vx (SDLK_e);
  int_vx (SDLK_f);
  int_vx (SDLK_g);
  int_vx (SDLK_h);
  int_vx (SDLK_i);
  int_vx (SDLK_j);
  int_vx (SDLK_k);
  int_vx (SDLK_l);
  int_vx (SDLK_m);
  int_vx (SDLK_n);
  int_vx (SDLK_o);
  int_vx (SDLK_p);
  int_vx (SDLK_q);
  int_vx (SDLK_r);
  int_vx (SDLK_s);
  int_vx (SDLK_t);
  int_vx (SDLK_u);
  int_vx (SDLK_v);
  int_vx (SDLK_w);
  int_vx (SDLK_x);
  int_vx (SDLK_y);
  int_vx (SDLK_z);
  int_vx (SDLK_CAPSLOCK);
  int_vx (SDLK_F1);
  int_vx (SDLK_F2);
  int_vx (SDLK_F3);
  int_vx (SDLK_F4);
  int_vx (SDLK_F5);
  int_vx (SDLK_F6);
  int_vx (SDLK_F7);
  int_vx (SDLK_F8);
  int_vx (SDLK_F9);
  int_vx (SDLK_F10);
  int_vx (SDLK_F11);
  int_vx (SDLK_F12);
  int_vx (SDLK_PRINTSCREEN);
  int_vx (SDLK_SCROLLLOCK);
  int_vx (SDLK_PAUSE);
  int_vx (SDLK_INSERT);
  int_vx (SDLK_HOME);
  int_vx (SDLK_PAGEUP);
  int_vx (SDLK_DELETE);
  int_vx (SDLK_END);
  int_vx (SDLK_PAGEDOWN);
  int_vx (SDLK_RIGHT);
  int_vx (SDLK_LEFT);
  int_vx (SDLK_DOWN);
  int_vx (SDLK_UP);
  int_vx (SDLK_NUMLOCKCLEAR);
  int_vx (SDLK_KP_DIVIDE);
  int_vx (SDLK_KP_MULTIPLY);
  int_vx (SDLK_KP_MINUS);
  int_vx (SDLK_KP_PLUS);
  int_vx (SDLK_KP_ENTER);
  int_vx (SDLK_KP_1);
  int_vx (SDLK_KP_2);
  int_vx (SDLK_KP_3);
  int_vx (SDLK_KP_4);
  int_vx (SDLK_KP_5);
  int_vx (SDLK_KP_6);
  int_vx (SDLK_KP_7);
  int_vx (SDLK_KP_8);
  int_vx (SDLK_KP_9);
  int_vx (SDLK_KP_0);
  int_vx (SDLK_KP_PERIOD);
  int_vx (SDLK_APPLICATION);
  int_vx (SDLK_POWER);
  int_vx (SDLK_KP_EQUALS);
  int_vx (SDLK_F13);
  int_vx (SDLK_F14);
  int_vx (SDLK_F15);
  int_vx (SDLK_F16);
  int_vx (SDLK_F17);
  int_vx (SDLK_F18);
  int_vx (SDLK_F19);
  int_vx (SDLK_F20);
  int_vx (SDLK_F21);
  int_vx (SDLK_F22);
  int_vx (SDLK_F23);
  int_vx (SDLK_F24);
  int_vx (SDLK_EXECUTE);
  int_vx (SDLK_HELP);
  int_vx (SDLK_MENU);
  int_vx (SDLK_SELECT);
  int_vx (SDLK_STOP);
  int_vx (SDLK_AGAIN);
  int_vx (SDLK_UNDO);
  int_vx (SDLK_CUT);
  int_vx (SDLK_COPY);
  int_vx (SDLK_PASTE);
  int_vx (SDLK_FIND);
  int_vx (SDLK_MUTE);
  int_vx (SDLK_VOLUMEUP);
  int_vx (SDLK_VOLUMEDOWN);
  int_vx (SDLK_KP_COMMA);
  int_vx (SDLK_KP_EQUALSAS400);
  int_vx (SDLK_ALTERASE);
  int_vx (SDLK_SYSREQ);
  int_vx (SDLK_CANCEL);
  int_vx (SDLK_CLEAR);
  int_vx (SDLK_PRIOR);
  int_vx (SDLK_RETURN2);
  int_vx (SDLK_SEPARATOR);
  int_vx (SDLK_OUT);
  int_vx (SDLK_OPER);
  int_vx (SDLK_CLEARAGAIN);
  int_vx (SDLK_CRSEL);
  int_vx (SDLK_EXSEL);
  int_vx (SDLK_KP_00);
  int_vx (SDLK_KP_000);
  int_vx (SDLK_THOUSANDSSEPARATOR);
  int_vx (SDLK_DECIMALSEPARATOR);
  int_vx (SDLK_CURRENCYUNIT);
  int_vx (SDLK_CURRENCYSUBUNIT);
  int_vx (SDLK_KP_LEFTPAREN);
  int_vx (SDLK_KP_RIGHTPAREN);
  int_vx (SDLK_KP_LEFTBRACE);
  int_vx (SDLK_KP_RIGHTBRACE);
  int_vx (SDLK_KP_TAB);
  int_vx (SDLK_KP_BACKSPACE);
  int_vx (SDLK_KP_A);
  int_vx (SDLK_KP_B);
  int_vx (SDLK_KP_C);
  int_vx (SDLK_KP_D);
  int_vx (SDLK_KP_E);
  int_vx (SDLK_KP_F);
  int_vx (SDLK_KP_XOR);
  int_vx (SDLK_KP_POWER);
  int_vx (SDLK_KP_PERCENT);
  int_vx (SDLK_KP_LESS);
  int_vx (SDLK_KP_GREATER);
  int_vx (SDLK_KP_AMPERSAND);
  int_vx (SDLK_KP_DBLAMPERSAND);
  int_vx (SDLK_KP_VERTICALBAR);
  int_vx (SDLK_KP_DBLVERTICALBAR);
  int_vx (SDLK_KP_COLON);
  int_vx (SDLK_KP_HASH);
  int_vx (SDLK_KP_SPACE);
  int_vx (SDLK_KP_AT);
  int_vx (SDLK_KP_EXCLAM);
  int_vx (SDLK_KP_MEMSTORE);
  int_vx (SDLK_KP_MEMRECALL);
  int_vx (SDLK_KP_MEMCLEAR);
  int_vx (SDLK_KP_MEMADD);
  int_vx (SDLK_KP_MEMSUBTRACT);
  int_vx (SDLK_KP_MEMMULTIPLY);
  int_vx (SDLK_KP_MEMDIVIDE);
  int_vx (SDLK_KP_PLUSMINUS);
  int_vx (SDLK_KP_CLEAR);
  int_vx (SDLK_KP_CLEARENTRY);
  int_vx (SDLK_KP_BINARY);
  int_vx (SDLK_KP_OCTAL);
  int_vx (SDLK_KP_DECIMAL);
  int_vx (SDLK_KP_HEXADECIMAL);
  int_vx (SDLK_LCTRL);
  int_vx (SDLK_LSHIFT);
  int_vx (SDLK_LALT);
  int_vx (SDLK_LGUI);
  int_vx (SDLK_RCTRL);
  int_vx (SDLK_RSHIFT);
  int_vx (SDLK_RALT);
  int_vx (SDLK_RGUI);
  int_vx (SDLK_MODE);
  int_vx (SDLK_AUDIONEXT);
  int_vx (SDLK_AUDIOPREV);
  int_vx (SDLK_AUDIOSTOP);
  int_vx (SDLK_AUDIOPLAY);
  int_vx (SDLK_AUDIOMUTE);
  int_vx (SDLK_MEDIASELECT);
  int_vx (SDLK_WWW);
  int_vx (SDLK_MAIL);
  int_vx (SDLK_CALCULATOR);
  int_vx (SDLK_COMPUTER);
  int_vx (SDLK_AC_SEARCH);
  int_vx (SDLK_AC_HOME);
  int_vx (SDLK_AC_BACK);
  int_vx (SDLK_AC_FORWARD);
  int_vx (SDLK_AC_STOP);
  int_vx (SDLK_AC_REFRESH);
  int_vx (SDLK_AC_BOOKMARKS);
  int_vx (SDLK_BRIGHTNESSDOWN);
  int_vx (SDLK_BRIGHTNESSUP);
  int_vx (SDLK_DISPLAYSWITCH);
  int_vx (SDLK_KBDILLUMTOGGLE);
  int_vx (SDLK_KBDILLUMDOWN);
  int_vx (SDLK_KBDILLUMUP);
  int_vx (SDLK_EJECT);
  int_vx (SDLK_SLEEP);

  int_vx (KMOD_NONE);
  int_vx (KMOD_LSHIFT);
  int_vx (KMOD_RSHIFT);
  int_vx (KMOD_LCTRL);
  int_vx (KMOD_RCTRL);
  int_vx (KMOD_LALT);
  int_vx (KMOD_RALT);
  int_vx (KMOD_LGUI);
  int_vx (KMOD_RGUI);
  int_vx (KMOD_NUM);
  int_vx (KMOD_CAPS);
  int_vx (KMOD_MODE);
  int_vx (KMOD_RESERVED);
  int_vx (KMOD_CTRL);
  int_vx (KMOD_SHIFT);
  int_vx (KMOD_ALT);
  int_vx (KMOD_GUI);

  /* Mouse */

  int_v (SDL_SYSTEM_CURSOR_ARROW);
  int_v (SDL_SYSTEM_CURSOR_IBEAM);
  int_v (SDL_SYSTEM_CURSOR_WAIT);
  int_v (SDL_SYSTEM_CURSOR_CROSSHAIR);
  int_v (SDL_SYSTEM_CURSOR_WAITARROW);
  int_v (SDL_SYSTEM_CURSOR_SIZENWSE);
  int_v (SDL_SYSTEM_CURSOR_SIZENESW);
  int_v (SDL_SYSTEM_CURSOR_SIZEWE);
  int_v (SDL_SYSTEM_CURSOR_SIZENS);
  int_v (SDL_SYSTEM_CURSOR_SIZEALL);
  int_v (SDL_SYSTEM_CURSOR_NO);
  int_v (SDL_SYSTEM_CURSOR_HAND);

  int_v (SDL_BUTTON_LEFT);
  int_v (SDL_BUTTON_MIDDLE);
  int_v (SDL_BUTTON_RIGHT);
  int_v (SDL_BUTTON_X1);
  int_v (SDL_BUTTON_X2);

  int_v (SDL_BUTTON_LMASK);
  int_v (SDL_BUTTON_MMASK);
  int_v (SDL_BUTTON_RMASK);
  int_v (SDL_BUTTON_X1MASK);
  int_v (SDL_BUTTON_X2MASK);

  /* Touch */

  int32_v (SDL_TOUCH_MOUSEID);

  /* Joystick */

  int_v (SDL_HAT_CENTERED);
  int_v (SDL_HAT_UP);
  int_v (SDL_HAT_RIGHT);
  int_v (SDL_HAT_DOWN);
  int_v (SDL_HAT_LEFT);
  int_v (SDL_HAT_RIGHTUP);
  int_v (SDL_HAT_RIGHTDOWN);
  int_v (SDL_HAT_LEFTUP);
  int_v (SDL_HAT_LEFTDOWN);

  int_v (SDL_JOYSTICK_POWER_UNKNOWN);
  int_v (SDL_JOYSTICK_POWER_LOW);
  int_v (SDL_JOYSTICK_POWER_MEDIUM);
  int_v (SDL_JOYSTICK_POWER_FULL);
  int_v (SDL_JOYSTICK_POWER_MAX);
  int_v (SDL_JOYSTICK_POWER_WIRED);

  int_v (SDL_JOYSTICK_TYPE_UNKNOWN);
  int_v (SDL_JOYSTICK_TYPE_GAMECONTROLLER);
  int_v (SDL_JOYSTICK_TYPE_WHEEL);
  int_v (SDL_JOYSTICK_TYPE_ARCADE_STICK);
  int_v (SDL_JOYSTICK_TYPE_FLIGHT_STICK);
  int_v (SDL_JOYSTICK_TYPE_DANCE_PAD);
  int_v (SDL_JOYSTICK_TYPE_GUITAR);
  int_v (SDL_JOYSTICK_TYPE_DRUM_KIT);
  int_v (SDL_JOYSTICK_TYPE_ARCADE_PAD);
  int_v (SDL_JOYSTICK_TYPE_THROTTLE);
  /* Game controller */

  int_v (SDL_CONTROLLER_BINDTYPE_NONE);
  int_v (SDL_CONTROLLER_BINDTYPE_BUTTON);
  int_v (SDL_CONTROLLER_BINDTYPE_AXIS);
  int_v (SDL_CONTROLLER_BINDTYPE_HAT);

  int_v (SDL_CONTROLLER_AXIS_INVALID);
  int_v (SDL_CONTROLLER_AXIS_LEFTX);
  int_v (SDL_CONTROLLER_AXIS_LEFTY);
  int_v (SDL_CONTROLLER_AXIS_RIGHTX);
  int_v (SDL_CONTROLLER_AXIS_RIGHTY);
  int_v (SDL_CONTROLLER_AXIS_TRIGGERLEFT);
  int_v (SDL_CONTROLLER_AXIS_TRIGGERRIGHT);
  int_v (SDL_CONTROLLER_AXIS_MAX);

  int_v (SDL_CONTROLLER_BUTTON_INVALID);
  int_v (SDL_CONTROLLER_BUTTON_A);
  int_v (SDL_CONTROLLER_BUTTON_B);
  int_v (SDL_CONTROLLER_BUTTON_X);
  int_v (SDL_CONTROLLER_BUTTON_Y);
  int_v (SDL_CONTROLLER_BUTTON_BACK);
  int_v (SDL_CONTROLLER_BUTTON_GUIDE);
  int_v (SDL_CONTROLLER_BUTTON_START);
  int_v (SDL_CONTROLLER_BUTTON_LEFTSTICK);
  int_v (SDL_CONTROLLER_BUTTON_RIGHTSTICK);
  int_v (SDL_CONTROLLER_BUTTON_LEFTSHOULDER);
  int_v (SDL_CONTROLLER_BUTTON_RIGHTSHOULDER);
  int_v (SDL_CONTROLLER_BUTTON_DPAD_UP);
  int_v (SDL_CONTROLLER_BUTTON_DPAD_DOWN);
  int_v (SDL_CONTROLLER_BUTTON_DPAD_LEFT);
  int_v (SDL_CONTROLLER_BUTTON_DPAD_RIGHT);
  int_v (SDL_CONTROLLER_BUTTON_MAX);

  assert (sizeof(SDL_GameControllerButtonBind) == 12);

  /* Event */

  int_v (SDL_QUERY);
  int_v (SDL_DISABLE);
  int_v (SDL_ENABLE);
  int_v (SDL_PRESSED);
  int_v (SDL_RELEASED);

  int_v (SDL_FIRSTEVENT);
  int_v (SDL_QUIT);
  int_v (SDL_APP_TERMINATING);
  int_v (SDL_APP_LOWMEMORY);
  int_v (SDL_APP_WILLENTERBACKGROUND);
  int_v (SDL_APP_DIDENTERBACKGROUND);
  int_v (SDL_APP_WILLENTERFOREGROUND);
  int_v (SDL_APP_DIDENTERFOREGROUND);
  int_v (SDL_DISPLAYEVENT);
  int_v (SDL_WINDOWEVENT);
  int_v (SDL_SYSWMEVENT);
  int_v (SDL_KEYDOWN);
  int_v (SDL_KEYUP);
  int_v (SDL_TEXTEDITING);
  int_v (SDL_TEXTINPUT);
  int_v (SDL_KEYMAPCHANGED);
  int_v (SDL_MOUSEMOTION);
  int_v (SDL_MOUSEBUTTONDOWN);
  int_v (SDL_MOUSEBUTTONUP);
  int_v (SDL_MOUSEWHEEL);
  int_v (SDL_MOUSEWHEEL_NORMAL);
  int_v (SDL_MOUSEWHEEL_FLIPPED);
  int_v (SDL_JOYAXISMOTION);
  int_v (SDL_JOYBALLMOTION);
  int_v (SDL_JOYHATMOTION);
  int_v (SDL_JOYBUTTONDOWN);
  int_v (SDL_JOYBUTTONUP);
  int_v (SDL_JOYDEVICEADDED);
  int_v (SDL_JOYDEVICEREMOVED);
  int_v (SDL_CONTROLLERAXISMOTION);
  int_v (SDL_CONTROLLERBUTTONDOWN);
  int_v (SDL_CONTROLLERBUTTONUP);
  int_v (SDL_CONTROLLERDEVICEADDED);
  int_v (SDL_CONTROLLERDEVICEREMOVED);
  int_v (SDL_CONTROLLERDEVICEREMAPPED);
  int_v (SDL_FINGERDOWN);
  int_v (SDL_FINGERUP);
  int_v (SDL_FINGERMOTION);
  int_v (SDL_DOLLARGESTURE);
  int_v (SDL_DOLLARRECORD);
  int_v (SDL_MULTIGESTURE);
  int_v (SDL_CLIPBOARDUPDATE);
  int_v (SDL_DROPFILE);
  int_v (SDL_DROPTEXT);
  int_v (SDL_DROPBEGIN);
  int_v (SDL_DROPCOMPLETE);
  int_v (SDL_AUDIODEVICEADDED);
  int_v (SDL_AUDIODEVICEREMOVED);
  int_v (SDL_SENSORUPDATE);
  int_v (SDL_RENDER_TARGETS_RESET);
  int_v (SDL_RENDER_DEVICE_RESET);
  int_v (SDL_USEREVENT);
  int_v (SDL_LASTEVENT);

  int tsdl_sdl_event_size = sizeof (SDL_Event);
  int_v (tsdl_sdl_event_size);

  int_v (SDL_TEXTEDITINGEVENT_TEXT_SIZE);
  int_v (SDL_TEXTINPUTEVENT_TEXT_SIZE);

  /* SDL_WindowEventID */

  int_v (SDL_WINDOWEVENT_SHOWN);
  int_v (SDL_WINDOWEVENT_HIDDEN);
  int_v (SDL_WINDOWEVENT_EXPOSED);
  int_v (SDL_WINDOWEVENT_MOVED);
  int_v (SDL_WINDOWEVENT_RESIZED);
  int_v (SDL_WINDOWEVENT_SIZE_CHANGED);
  int_v (SDL_WINDOWEVENT_MINIMIZED);
  int_v (SDL_WINDOWEVENT_MAXIMIZED);
  int_v (SDL_WINDOWEVENT_RESTORED);
  int_v (SDL_WINDOWEVENT_ENTER);
  int_v (SDL_WINDOWEVENT_LEAVE);
  int_v (SDL_WINDOWEVENT_FOCUS_GAINED);
  int_v (SDL_WINDOWEVENT_FOCUS_LOST);
  int_v (SDL_WINDOWEVENT_CLOSE);
  int_v (SDL_WINDOWEVENT_TAKE_FOCUS);
  int_v (SDL_WINDOWEVENT_HIT_TEST);

  /* Haptic */

  assert (SDL_HAPTIC_INFINITY == 4294967295U);

  int_v (SDL_HAPTIC_CONSTANT);
  int_v (SDL_HAPTIC_SINE);
  int_v (SDL_HAPTIC_LEFTRIGHT);
  int_v (SDL_HAPTIC_TRIANGLE);
  int_v (SDL_HAPTIC_SAWTOOTHUP);
  int_v (SDL_HAPTIC_SAWTOOTHDOWN);
  int_v (SDL_HAPTIC_RAMP);
  int_v (SDL_HAPTIC_SPRING);
  int_v (SDL_HAPTIC_DAMPER);
  int_v (SDL_HAPTIC_INERTIA);
  int_v (SDL_HAPTIC_FRICTION);
  int_v (SDL_HAPTIC_CUSTOM);
  int_v (SDL_HAPTIC_GAIN);
  int_v (SDL_HAPTIC_AUTOCENTER);
  int_v (SDL_HAPTIC_STATUS);
  int_v (SDL_HAPTIC_PAUSE);

  int_v (SDL_HAPTIC_POLAR);
  int_v (SDL_HAPTIC_CARTESIAN);
  int_v (SDL_HAPTIC_SPHERICAL);

  /* Audio */

  int_v (SDL_AUDIO_STOPPED);
  int_v (SDL_AUDIO_PLAYING);
  int_v (SDL_AUDIO_PAUSED);

  assert (sizeof (SDL_AudioFormat) == 2);

  int_v (AUDIO_S8);
  int_v (AUDIO_U8);
  int_v (AUDIO_S16LSB);
  int_v (AUDIO_S16MSB);
  int_v (AUDIO_S16SYS);
  int_v (AUDIO_S16);
  int_v (AUDIO_U16LSB);
  int_v (AUDIO_U16MSB);
  int_v (AUDIO_U16SYS);
  int_v (AUDIO_U16);
  int_v (AUDIO_S32LSB);
  int_v (AUDIO_S32MSB);
  int_v (AUDIO_S32SYS);
  int_v (AUDIO_S32);
  int_v (AUDIO_F32LSB);
  int_v (AUDIO_F32MSB);
  int_v (AUDIO_F32SYS);
  int_v (AUDIO_F32);

  int_v (SDL_AUDIO_ALLOW_FREQUENCY_CHANGE);
  int_v (SDL_AUDIO_ALLOW_FORMAT_CHANGE);
  int_v (SDL_AUDIO_ALLOW_CHANNELS_CHANGE);
  int_v (SDL_AUDIO_ALLOW_ANY_CHANGE);

  /* Power management */

  int_v (SDL_POWERSTATE_UNKNOWN);
  int_v (SDL_POWERSTATE_ON_BATTERY);
  int_v (SDL_POWERSTATE_NO_BATTERY);
  int_v (SDL_POWERSTATE_CHARGING);
  int_v (SDL_POWERSTATE_CHARGED);
}

CAMLprim value output_consts (value fname)
{
  const char *outf = String_val (fname);
  FILE *fd;
  if (strlen(outf) == 0) { fd = stdout; }
  else
    {
      fd = fopen (outf, "w");
      if (!fd) { perror(outf); exit (1); }
    }

  consts(fd);
  fflush(fd);
  if (fd != stdout) { fclose (fd); }
  return Val_unit;
}
