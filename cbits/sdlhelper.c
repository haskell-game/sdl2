#include <string.h>
#include "sdlhelper.h"

void SDLHelper_JoystickGetDeviceGUID (int device_index, SDL_JoystickGUID *guid)
{
  SDL_JoystickGUID t = SDL_JoystickGetDeviceGUID (device_index);
  memcpy (guid, &t, sizeof (*guid));
}

void SDLHelper_JoystickGetGUID (SDL_Joystick *joystick, SDL_JoystickGUID *guid)
{
  SDL_JoystickGUID t = SDL_JoystickGetGUID (joystick);
  memcpy (guid, &t, sizeof (*guid));
}

void SDLHelper_JoystickGetGUIDFromString (const char *pchGUID, SDL_JoystickGUID *guid)
{
  SDL_JoystickGUID t = SDL_JoystickGetGUIDFromString (pchGUID);
  memcpy (guid, &t, sizeof (*guid));
}

void SDLHelper_JoystickGetGUIDString (const SDL_JoystickGUID *guid, char *gszGUID, int cbGUID)
{
  SDL_JoystickGetGUIDString (*guid, gszGUID, cbGUID);
}

void SDLHelper_GameControllerGetBindForAxis (SDL_GameController *gamecontroller, SDL_GameControllerAxis axis, SDL_GameControllerButtonBind *bind)
{
  SDL_GameControllerButtonBind t = SDL_GameControllerGetBindForAxis (gamecontroller, axis);
  memcpy (bind, &t, sizeof (*bind));
}

void SDLHelper_GameControllerGetBindForButton (SDL_GameController *gamecontroller, SDL_GameControllerButton button, SDL_GameControllerButtonBind *bind)
{
  SDL_GameControllerButtonBind t = SDL_GameControllerGetBindForButton (gamecontroller, button);
  memcpy (bind, &t, sizeof (*bind));
}

char *SDLHelper_GameControllerMappingForGUID (const SDL_JoystickGUID *guid)
{
  return SDL_GameControllerMappingForGUID (*guid);
}

void SDLHelper_LogMessage (int category, SDL_LogPriority priority, const char *str)
{
  SDL_LogMessage (category, priority, "%s", str);
}

int SDLHelper_RWclose (SDL_RWops *ctx)
{
  return SDL_RWclose (ctx);
}

size_t SDLHelper_RWread (SDL_RWops *ctx, void *ptr, size_t size, size_t maxnum)
{
  return SDL_RWread (ctx, ptr, size, maxnum);
}

Sint64 SDLHelper_RWseek (SDL_RWops *ctx, Sint64 offset, int whence)
{
  return SDL_RWseek (ctx, offset, whence);
}

Sint64 SDLHelper_RWtell (SDL_RWops *ctx)
{
  return SDL_RWtell (ctx);
}

size_t SDLHelper_RWwrite (SDL_RWops *ctx, const void *ptr, size_t size, size_t num)
{
  return SDL_RWwrite (ctx, ptr, size, num);
}

int SDLHelper_SetError (const char *str)
{
  return SDL_SetError ("%s", str);
}

int SDLHelper_RenderFillRectEx(SDL_Renderer*   renderer, int x, int y, int w, int h)
{
  SDL_Rect rect;
  rect.x=x;
  rect.y=y;
  rect.w=w;
  rect.h=h;
  return SDL_RenderFillRect(renderer,&rect);
}
