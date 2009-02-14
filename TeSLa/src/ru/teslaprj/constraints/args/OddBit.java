package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;

public class OddBit extends Argument
{
	private String name;
	
	public OddBit( String name )
	{
		this.name = name;
	}

	@Override
	public String getName()
	{
		return name;
	}

}
