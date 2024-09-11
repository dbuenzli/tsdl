/*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "SDL.h"
#include <caml/mlvalues.h>

#ifdef _MSC_VER
/* MSVC requires at least one extern function from SDL2 to be used.
   If not, MSVC will not link the SDL2 import library and subsequently the
   SDL2 DLL will not be implicitly linked (aka. load-time dynamic linking)
   to the TSDL stub DLL. That can be verified with Unix `ldd` or MSVC
   `dumpbin /imports`.
   ctypes-foreign, in particular ctypes_win32_dlsym_rtld_default(), requires
   that all foreign DLLs are already mapped into the process address space.
   Implicit linking is the simplest way to do that.
 */
void tsdl_nop (void) { SDL_WasInit(0); return; }
#else
void tsdl_nop (void) { return; }
#endif

CAMLprim value ocaml_tsdl_log_message (value c, value p, value m)
{
  /* XXX we assume users know what they are logging and
     avoid the caml_string_is_c_safe (m) scan for now.  */
  SDL_LogMessage (Int_val (c), Int_val (p), "%s", String_val (m));
  return Val_unit;
}
