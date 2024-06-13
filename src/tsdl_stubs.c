/*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

#include "SDL.h"

#ifdef _MSC_VER
#define _DLLAPI __declspec(dllexport)
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
#define _DLLAPI
void tsdl_nop (void) { return; }
#endif

/* Fixed arity function, libffi does not support varargs. */
_DLLAPI void caml_tsdl_log1arg (char* fmt, char* arg1) {
   /* varargs */
   SDL_Log(fmt, arg1);
}

/*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers

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
  ---------------------------------------------------------------------------*/
