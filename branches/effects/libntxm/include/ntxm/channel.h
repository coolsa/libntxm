#ifndef __CHANNEL_H__
#define __CHANNEL_H__
#include "song.h"
struct ChannelState {
  Instrument *inst; // the instrument being played
  Sample *smp;      // the sample being played.
  unsigned no;      // the channel number (for registers and stuff).
  Cell cell;        // a copy of the pattern's cell
  u16 srcsmp_cache; // see playNote/bendNote.
  u8 note, lastparam;

  // -=- effect-specific variables -=-
  s16 actual_finetune;                   // for Exx, Fxx and Hxx
  s16 vibpos, vibdepth, vibspeed;        // for Hxx
  s16 slidetune_speed;  u8 target_note;  // for Gxx
};

#endif
