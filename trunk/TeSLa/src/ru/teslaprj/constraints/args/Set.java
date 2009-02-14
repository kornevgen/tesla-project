package ru.teslaprj.constraints.args;

import ru.teslaprj.Cache;
import ru.teslaprj.constraints.Argument;
import ru.teslaprj.scheme.Command;

public class Set extends Argument
{
	private final String name;
	private final Cache cache;
	private final Command cmd;
	
	public Set( String name, Cache cache, Command cmd )
	{
		this.name = name;
		this.cmd = cmd;
		this.cache = cache;
	}

	@Override
	public String getName()
	{
		return name;
	}

	public Cache getCache()
	{
		return cache;
	}

	public Command getCommand()
	{
		return cmd;
	}
}
