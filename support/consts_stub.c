/*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*/

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include "SDL.h"

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

void int32_v (FILE *fd, char *symb, int32_t value)
{ let (fd, symb); fprintf (fd, " = 0x%Xl\n", value); }

void consts (FILE *fd)
{

#define int_v(e) int_v(fd, "" # e, (int)e)
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
  str_v (SDL_HINT_ORIENTATIONS);
  str_v (SDL_HINT_RENDER_DRIVER);
  str_v (SDL_HINT_RENDER_OPENGL_SHADERS);
  str_v (SDL_HINT_RENDER_SCALE_QUALITY);
  str_v (SDL_HINT_RENDER_VSYNC);
  
  int_v (SDL_HINT_DEFAULT);
  int_v (SDL_HINT_NORMAL);
  int_v (SDL_HINT_OVERRIDE);

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

  int_v (SDL_WINDOWPOS_CENTERED);
  int_v (SDL_WINDOWPOS_UNDEFINED);

  /* SDL_GLcontextFlag */

  int_v (SDL_GL_CONTEXT_DEBUG_FLAG);
  int_v (SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG);
  int_v (SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG);
  int_v (SDL_GL_CONTEXT_RESET_ISOLATION_FLAG);

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

  int32_v (SDLK_UNKNOWN);
  int32_v (SDLK_RETURN);
  int32_v (SDLK_ESCAPE);
  int32_v (SDLK_BACKSPACE);
  int32_v (SDLK_TAB);
  int32_v (SDLK_SPACE);
  int32_v (SDLK_EXCLAIM);
  int32_v (SDLK_QUOTEDBL);
  int32_v (SDLK_HASH);
  int32_v (SDLK_PERCENT);
  int32_v (SDLK_DOLLAR);
  int32_v (SDLK_AMPERSAND);
  int32_v (SDLK_QUOTE);
  int32_v (SDLK_LEFTPAREN);
  int32_v (SDLK_RIGHTPAREN);
  int32_v (SDLK_ASTERISK);
  int32_v (SDLK_PLUS);
  int32_v (SDLK_COMMA);
  int32_v (SDLK_MINUS);
  int32_v (SDLK_PERIOD);
  int32_v (SDLK_SLASH);
  int32_v (SDLK_0);
  int32_v (SDLK_1);
  int32_v (SDLK_2);
  int32_v (SDLK_3);
  int32_v (SDLK_4);
  int32_v (SDLK_5);
  int32_v (SDLK_6);
  int32_v (SDLK_7);
  int32_v (SDLK_8);
  int32_v (SDLK_9);
  int32_v (SDLK_COLON);
  int32_v (SDLK_SEMICOLON);
  int32_v (SDLK_LESS);
  int32_v (SDLK_EQUALS);
  int32_v (SDLK_GREATER);
  int32_v (SDLK_QUESTION);
  int32_v (SDLK_AT);
  int32_v (SDLK_LEFTBRACKET);
  int32_v (SDLK_BACKSLASH);
  int32_v (SDLK_RIGHTBRACKET);
  int32_v (SDLK_CARET);
  int32_v (SDLK_UNDERSCORE);
  int32_v (SDLK_BACKQUOTE);
  int32_v (SDLK_a);
  int32_v (SDLK_b);
  int32_v (SDLK_c);
  int32_v (SDLK_d);
  int32_v (SDLK_e);
  int32_v (SDLK_f);
  int32_v (SDLK_g);
  int32_v (SDLK_h);
  int32_v (SDLK_i);
  int32_v (SDLK_j);
  int32_v (SDLK_k);
  int32_v (SDLK_l);
  int32_v (SDLK_m);
  int32_v (SDLK_n);
  int32_v (SDLK_o);
  int32_v (SDLK_p);
  int32_v (SDLK_q);
  int32_v (SDLK_r);
  int32_v (SDLK_s);
  int32_v (SDLK_t);
  int32_v (SDLK_u);
  int32_v (SDLK_v);
  int32_v (SDLK_w);
  int32_v (SDLK_x);
  int32_v (SDLK_y);
  int32_v (SDLK_z);
  int32_v (SDLK_CAPSLOCK);
  int32_v (SDLK_F1);
  int32_v (SDLK_F2);
  int32_v (SDLK_F3);
  int32_v (SDLK_F4);
  int32_v (SDLK_F5);
  int32_v (SDLK_F6);
  int32_v (SDLK_F7);
  int32_v (SDLK_F8);
  int32_v (SDLK_F9);
  int32_v (SDLK_F10);
  int32_v (SDLK_F11);
  int32_v (SDLK_F12);
  int32_v (SDLK_PRINTSCREEN);
  int32_v (SDLK_SCROLLLOCK);
  int32_v (SDLK_PAUSE);
  int32_v (SDLK_INSERT);
  int32_v (SDLK_HOME);
  int32_v (SDLK_PAGEUP);
  int32_v (SDLK_DELETE);
  int32_v (SDLK_END);
  int32_v (SDLK_PAGEDOWN);
  int32_v (SDLK_RIGHT);
  int32_v (SDLK_LEFT);
  int32_v (SDLK_DOWN);
  int32_v (SDLK_UP);
  int32_v (SDLK_NUMLOCKCLEAR);
  int32_v (SDLK_KP_DIVIDE);
  int32_v (SDLK_KP_MULTIPLY);
  int32_v (SDLK_KP_MINUS);
  int32_v (SDLK_KP_PLUS);
  int32_v (SDLK_KP_ENTER);
  int32_v (SDLK_KP_1);
  int32_v (SDLK_KP_2);
  int32_v (SDLK_KP_3);
  int32_v (SDLK_KP_4);
  int32_v (SDLK_KP_5);
  int32_v (SDLK_KP_6);
  int32_v (SDLK_KP_7);
  int32_v (SDLK_KP_8);
  int32_v (SDLK_KP_9);
  int32_v (SDLK_KP_0);
  int32_v (SDLK_KP_PERIOD);
  int32_v (SDLK_APPLICATION);
  int32_v (SDLK_POWER);
  int32_v (SDLK_KP_EQUALS);
  int32_v (SDLK_F13);
  int32_v (SDLK_F14);
  int32_v (SDLK_F15);
  int32_v (SDLK_F16);
  int32_v (SDLK_F17);
  int32_v (SDLK_F18);
  int32_v (SDLK_F19);
  int32_v (SDLK_F20);
  int32_v (SDLK_F21);
  int32_v (SDLK_F22);
  int32_v (SDLK_F23);
  int32_v (SDLK_F24);
  int32_v (SDLK_EXECUTE);
  int32_v (SDLK_HELP);
  int32_v (SDLK_MENU);
  int32_v (SDLK_SELECT);
  int32_v (SDLK_STOP);
  int32_v (SDLK_AGAIN);
  int32_v (SDLK_UNDO);
  int32_v (SDLK_CUT);
  int32_v (SDLK_COPY);
  int32_v (SDLK_PASTE);
  int32_v (SDLK_FIND);
  int32_v (SDLK_MUTE);
  int32_v (SDLK_VOLUMEUP);
  int32_v (SDLK_VOLUMEDOWN);
  int32_v (SDLK_KP_COMMA);
  int32_v (SDLK_KP_EQUALSAS400);
  int32_v (SDLK_ALTERASE);
  int32_v (SDLK_SYSREQ);
  int32_v (SDLK_CANCEL);
  int32_v (SDLK_CLEAR);
  int32_v (SDLK_PRIOR);
  int32_v (SDLK_RETURN2);
  int32_v (SDLK_SEPARATOR);
  int32_v (SDLK_OUT);
  int32_v (SDLK_OPER);
  int32_v (SDLK_CLEARAGAIN);
  int32_v (SDLK_CRSEL);
  int32_v (SDLK_EXSEL);
  int32_v (SDLK_KP_00);
  int32_v (SDLK_KP_000);
  int32_v (SDLK_THOUSANDSSEPARATOR);
  int32_v (SDLK_DECIMALSEPARATOR);
  int32_v (SDLK_CURRENCYUNIT);
  int32_v (SDLK_CURRENCYSUBUNIT);
  int32_v (SDLK_KP_LEFTPAREN);
  int32_v (SDLK_KP_RIGHTPAREN);
  int32_v (SDLK_KP_LEFTBRACE);
  int32_v (SDLK_KP_RIGHTBRACE);
  int32_v (SDLK_KP_TAB);
  int32_v (SDLK_KP_BACKSPACE);
  int32_v (SDLK_KP_A);
  int32_v (SDLK_KP_B);
  int32_v (SDLK_KP_C);
  int32_v (SDLK_KP_D);
  int32_v (SDLK_KP_E);
  int32_v (SDLK_KP_F);
  int32_v (SDLK_KP_XOR);
  int32_v (SDLK_KP_POWER);
  int32_v (SDLK_KP_PERCENT);
  int32_v (SDLK_KP_LESS);
  int32_v (SDLK_KP_GREATER);
  int32_v (SDLK_KP_AMPERSAND);
  int32_v (SDLK_KP_DBLAMPERSAND);
  int32_v (SDLK_KP_VERTICALBAR);
  int32_v (SDLK_KP_DBLVERTICALBAR);
  int32_v (SDLK_KP_COLON);
  int32_v (SDLK_KP_HASH);
  int32_v (SDLK_KP_SPACE);
  int32_v (SDLK_KP_AT);
  int32_v (SDLK_KP_EXCLAM);
  int32_v (SDLK_KP_MEMSTORE);
  int32_v (SDLK_KP_MEMRECALL);
  int32_v (SDLK_KP_MEMCLEAR);
  int32_v (SDLK_KP_MEMADD);
  int32_v (SDLK_KP_MEMSUBTRACT);
  int32_v (SDLK_KP_MEMMULTIPLY);
  int32_v (SDLK_KP_MEMDIVIDE);
  int32_v (SDLK_KP_PLUSMINUS);
  int32_v (SDLK_KP_CLEAR);
  int32_v (SDLK_KP_CLEARENTRY);
  int32_v (SDLK_KP_BINARY);
  int32_v (SDLK_KP_OCTAL);
  int32_v (SDLK_KP_DECIMAL);
  int32_v (SDLK_KP_HEXADECIMAL);
  int32_v (SDLK_LCTRL);
  int32_v (SDLK_LSHIFT);
  int32_v (SDLK_LALT);
  int32_v (SDLK_LGUI);
  int32_v (SDLK_RCTRL);
  int32_v (SDLK_RSHIFT);
  int32_v (SDLK_RALT);
  int32_v (SDLK_RGUI);
  int32_v (SDLK_MODE);
  int32_v (SDLK_AUDIONEXT);
  int32_v (SDLK_AUDIOPREV);
  int32_v (SDLK_AUDIOSTOP);
  int32_v (SDLK_AUDIOPLAY);
  int32_v (SDLK_AUDIOMUTE);
  int32_v (SDLK_MEDIASELECT);
  int32_v (SDLK_WWW);
  int32_v (SDLK_MAIL);
  int32_v (SDLK_CALCULATOR);
  int32_v (SDLK_COMPUTER);
  int32_v (SDLK_AC_SEARCH);
  int32_v (SDLK_AC_HOME);
  int32_v (SDLK_AC_BACK);
  int32_v (SDLK_AC_FORWARD);
  int32_v (SDLK_AC_STOP);
  int32_v (SDLK_AC_REFRESH);
  int32_v (SDLK_AC_BOOKMARKS);
  int32_v (SDLK_BRIGHTNESSDOWN);
  int32_v (SDLK_BRIGHTNESSUP);
  int32_v (SDLK_DISPLAYSWITCH);
  int32_v (SDLK_KBDILLUMTOGGLE);
  int32_v (SDLK_KBDILLUMDOWN);
  int32_v (SDLK_KBDILLUMUP);
  int32_v (SDLK_EJECT);
  int32_v (SDLK_SLEEP);

  int_v (KMOD_NONE);
  int_v (KMOD_LSHIFT);
  int_v (KMOD_RSHIFT);
  int_v (KMOD_LCTRL);
  int_v (KMOD_RCTRL);
  int_v (KMOD_LALT);
  int_v (KMOD_RALT);
  int_v (KMOD_LGUI);
  int_v (KMOD_RGUI);
  int_v (KMOD_NUM);
  int_v (KMOD_CAPS);
  int_v (KMOD_MODE);
  int_v (KMOD_RESERVED);
  int_v (KMOD_CTRL);
  int_v (KMOD_SHIFT);
  int_v (KMOD_ALT);
  int_v (KMOD_GUI);

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

  int_v (SDL_BUTTON_LMASK);
  int_v (SDL_BUTTON_MMASK);
  int_v (SDL_BUTTON_RMASK);
  int_v (SDL_BUTTON_X1MASK);
  int_v (SDL_BUTTON_X2MASK);

  /* Touch */ 

  int_v (SDL_TOUCH_MOUSEID);

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
  int_v (SDL_WINDOWEVENT);
  int_v (SDL_SYSWMEVENT);
  int_v (SDL_KEYDOWN);
  int_v (SDL_KEYUP);
  int_v (SDL_TEXTEDITING);
  int_v (SDL_TEXTINPUT);
  int_v (SDL_MOUSEMOTION);
  int_v (SDL_MOUSEBUTTONDOWN);
  int_v (SDL_MOUSEBUTTONUP);
  int_v (SDL_MOUSEWHEEL);
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
  int_v (AUDIO_S16LSB);
  int_v (AUDIO_U16LSB);
  int_v (AUDIO_U16MSB);
  int_v (AUDIO_U16SYS);
  int_v (AUDIO_U16);
  int_v (AUDIO_U16LSB);
  int_v (AUDIO_S32LSB);
  int_v (AUDIO_S32MSB);
  int_v (AUDIO_S32SYS);
  int_v (AUDIO_S32);
  int_v (AUDIO_S32LSB);
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
  char *outf = String_val (fname);
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

/*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*/
