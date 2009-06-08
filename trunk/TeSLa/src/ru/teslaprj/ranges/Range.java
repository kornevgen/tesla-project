package ru.teslaprj.ranges;

import ru.teslaprj.scheme.MemoryCommand;

public abstract class Range
{
	MemoryCommand command;
	public Range( MemoryCommand cmd )
	{
		command = cmd;
	}
	
	public MemoryCommand getCommand()
	{
		return command;
	}
	
	Ranges ranges;
	public void setContext( Ranges ranges )
	{
		this.ranges = ranges;
	}
	public Ranges getContext()
	{
		return ranges;
	}
	
	public abstract String print();
}
