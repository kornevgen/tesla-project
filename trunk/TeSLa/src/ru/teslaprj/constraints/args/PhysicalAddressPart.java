package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;

public class PhysicalAddressPart extends Argument
{
	private String name;
	private int end;
	private int start;
	
	public PhysicalAddressPart( String name, int end, int start )
	{
		this.name = name;
		this.end = end;
		this.start = start;
	}

	@Override
	public String getName()
	{
		return name;
	}

	public int getEndBit()
	{
		return end;
	}
	
	public int getStartBit()
	{
		return start;
	}
}
