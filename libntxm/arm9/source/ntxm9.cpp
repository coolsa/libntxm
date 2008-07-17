// libNTXM9 - XM Player Library for the Nintendo DS
// Copyright (C) 2005-2007 Tobias Weyand (0xtob)
//                         me@nitrotracker.tobw.net
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

#include "ntxm/ntxm9.h"
#include "demokit.h"
#include "command.h"

NTXM9::NTXM9()
	:xm_transport(0), song(0)
{
	xm_transport = new XMTransport();
	CommandInit();
}

NTXM9::~NTXM9()
{
	delete xm_transport;
	
	if(song != 0)
		delete song;
}

int NTXM9::getoops(int* p, int *r, int *c)
{
  return song->getoops(p, r, c);
}

u16 NTXM9::load(DataReader *file)
{
  if (song) delete song;
  u16 err = xm_transport->load(file, &song);
  CommandSetSong(song);
  return err;
}

const char *NTXM9::getError(u16 error_id)
{
  return xm_transport->getError(error_id);
}

void NTXM9::play(bool repeat)
{
  if(song == 0)
    return;
  
  CommandStartPlay(0, 0, repeat);
}

void NTXM9::stop(void)
{
  if(song == 0)
    return;
  
  CommandStopPlay();
}
