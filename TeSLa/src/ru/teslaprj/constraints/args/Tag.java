package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;
import ru.teslaprj.scheme.Command;

public class Tag extends Argument
{
	private final String name;
	private final int level;
	private final Command command;
	
	public Tag( String name, int level, Command cmd )
	{
		this.name = name;
		this.level = level;
		this.command = cmd;
	}

	@Override
	public String getName()
	{
		return name;
	}
	
	public int getLevel()
	{
		return level;
	}

	public Command getCommand()
	{
		return command;
	}
}
