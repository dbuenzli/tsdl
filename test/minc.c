/*
   Minimal C SDL example. This code is in the public domain.
   Compile with:
   gcc -o minc minc.c `sdl2-config --cflags --libs`
*/

#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include "SDL.h"

int main(int argc, char** argv)
{
  assert (SDL_Init(SDL_INIT_VIDEO) == 0);

  SDL_Window *w =
    SDL_CreateWindow ("SDL OpenGL (C)",
                      SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                      640, 480,
                      SDL_WINDOW_OPENGL);
  assert (w);

  SDL_Delay (3000);

  SDL_DestroyWindow (w);
  SDL_Quit ();
  return 0;
}
