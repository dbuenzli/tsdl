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

  str_v (SDL_HINT_FRAMEBUFFER_ACCELERATION);
  str_v (SDL_HINT_IDLE_TIMER_DISABLED);
  str_v (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH);
  str_v (SDL_HINT_ORIENTATIONS);
  str_v (SDL_HINT_RENDER_DRIVER);
  str_v (SDL_HINT_RENDER_OPENGL_SHADERS);
  str_v (SDL_HINT_RENDER_LOGICAL_SIZE_MODE);
  str_v (SDL_HINT_RENDER_SCALE_QUALITY);
  str_v (SDL_HINT_RENDER_VSYNC);

  str_v (SDL_HINT_NO_SIGNAL_HANDLERS);
  str_v (SDL_HINT_THREAD_STACK_SIZE);
  str_v (SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN);

  str_v (SDL_HINT_AUDIO_RESAMPLING_MODE);
  str_v (SDL_HINT_MOUSE_NORMAL_SPEED_SCALE);
  str_v (SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE);
  str_v (SDL_HINT_TOUCH_MOUSE_EVENTS);
  str_v (SDL_HINT_MOUSE_TOUCH_EVENTS);

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
