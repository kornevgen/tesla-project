package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;
import ru.teslaprj.scheme.Command;

public class VirtualAddress extends Argument
{
	private final String name;
	private final Command command;
	
	public VirtualAddress( String name, Command command )
	{
		this.name = name;
		this.command = command;
	}

	@Override
	public String getName()
	{
		return name;
	}
	
	public Command getCommand()
	{
		return command;
	}
}
