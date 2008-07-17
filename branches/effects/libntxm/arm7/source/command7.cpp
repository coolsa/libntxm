// libNTXM - XM Player Library for the Nintendo DS
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

/*
  Functions for the ARM7 to process the commands from the ARM9.Based
  on code from the MOD player example posted to the GBADEV forums.
  Chris Double (chris.double@double.co.nz)
  http://www.double.co.nz/nintendo_ds
*/
#include <nds.h>
#include <stdarg.h>

#include "command.h"
#include "ntxm/ntxm7.h"
#include "ntxm/player.h"
#include "ntxm/linear_freq_table.h"

extern NTXM7 *ntxm7;

static void RecvCommandSetSong(SetSongCommand *c) {
	ntxm7->setSong((Song*)c->ptr);
}

static void RecvCommandStartPlay(StartPlayCommand *c) {
	ntxm7->play(c->loop);
}

static void RecvCommandStopPlay(StopPlayCommand *c) {
	ntxm7->stop();
}

void CommandUpdateRow(u16 row)
{
	Command* command = &commandControl->command[commandControl->currentCommand];
	command->destination = DST_ARM9;
	command->commandType = UPDATE_ROW;
	
	UpdateRowCommand *c = &command->updateRow;
	c->row = row;
	
	commandControl->currentCommand++;
	commandControl->currentCommand %= MAX_COMMANDS;
}

void CommandUpdatePotPos(u16 potpos)
{
	Command* command = &commandControl->command[commandControl->currentCommand];
	command->destination = DST_ARM9;
	command->commandType = UPDATE_POTPOS;
	
	UpdatePotPosCommand *c = &command->updatePotPos;
	c->potpos = potpos;
	
	commandControl->currentCommand++;
	commandControl->currentCommand %= MAX_COMMANDS;
}

void CommandNotifyStop(void)
{
	Command* command = &commandControl->command[commandControl->currentCommand];
	command->destination = DST_ARM9;
	command->commandType = NOTIFY_STOP;
	
	commandControl->currentCommand++;
	commandControl->currentCommand %= MAX_COMMANDS;
}

void CommandSampleFinish(void)
{
	Command* command = &commandControl->command[commandControl->currentCommand];
	command->destination = DST_ARM9;
	command->commandType = SAMPLE_FINISH;
	
	commandControl->currentCommand++;
	commandControl->currentCommand %= MAX_COMMANDS;
}

void CommandProcessCommands(void)
{
	static int currentCommand = 0;
	while(currentCommand != commandControl->currentCommand) {
		Command* command = &commandControl->command[currentCommand];
		
		if(command->destination == DST_ARM7) {
		
			switch(command->commandType) {
				case SET_SONG:
					RecvCommandSetSong(&command->setSong);
					break;
				case START_PLAY:
					RecvCommandStartPlay(&command->startPlay);
					break;
				case STOP_PLAY:
					RecvCommandStopPlay(&command->stopPlay);
					break;
				default:
					break;
			}
		
		}
		currentCommand++;
		currentCommand %= MAX_COMMANDS;
	}
}
