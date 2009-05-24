package ru.teslaprj.ranges;

import ru.teslaprj.scheme.Command;

public abstract class Range
{
	Command command;
	public Range( Command cmd )
	{
		command = cmd;
	}
	
	public Command getCommand()
	{
		return command;
	}
}
