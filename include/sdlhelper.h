#ifndef _HS_SDL2_HELPER_H_
#define _HS_SDL2_HELPER_H_

#include <stddef.h>
#include "SDL.h"

int SDLHelper_GetEventBufferSize(void);
SDL_Event *SDLHelper_GetEventBuffer(void);
void SDLHelper_JoystickGetDeviceGUID (int device_index, SDL_JoystickGUID *guid);
void SDLHelper_JoystickGetGUID (SDL_Joystick *joystick, SDL_JoystickGUID *guid);
void SDLHelper_JoystickGetGUIDFromString (const char *pchGUID, SDL_JoystickGUID *guid);
void SDLHelper_JoystickGetGUIDString (const SDL_JoystickGUID *guid, char *gszGUID, int cbGUID);

void SDLHelper_GameControllerGetBindForAxis (SDL_GameController *gamecontroller, SDL_GameControllerAxis axis, SDL_GameControllerButtonBind *bind);
void SDLHelper_GameControllerGetBindForButton (SDL_GameController *gamecontroller, SDL_GameControllerButton button, SDL_GameControllerButtonBind *bind);
char *SDLHelper_GameControllerMappingForGUID (const SDL_JoystickGUID *guid);

void SDLHelper_LogMessage (int category, SDL_LogPriority priority, const char *str);

int SDLHelper_RWclose (SDL_RWops *ctx);
size_t SDLHelper_RWread (SDL_RWops *ctx, void *ptr, size_t size, size_t maxnum);
Sint64 SDLHelper_RWseek (SDL_RWops *ctx, Sint64 offset, int whence);
Sint64 SDLHelper_RWtell (SDL_RWops *ctx);
size_t SDLHelper_RWwrite (SDL_RWops *ctx, const void *ptr, size_t size, size_t num);

int SDLHelper_SetError(const char *str);

#endif
