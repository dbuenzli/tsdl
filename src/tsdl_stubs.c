/*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "SDL.h"
#include <caml/mlvalues.h>

CAMLprim value ocaml_tsdl_log_message (value c, value p, value m)
{
  /* XXX we assume users know what they are logging and
     avoid the caml_string_is_c_safe (m) scan for now.  */
  SDL_LogMessage (Int_val (c), Int_val (p), "%s", String_val (m));
  return Val_unit;
}
