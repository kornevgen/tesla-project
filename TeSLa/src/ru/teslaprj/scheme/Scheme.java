package ru.teslaprj.scheme;

import java.util.ArrayList;
import java.util.List;


public class Scheme
{
	private List<Command> commands = new ArrayList<Command>();
	private List<Definition> defs = new ArrayList<Definition>();
	private List<Assert> asserts = new ArrayList<Assert>();
	
	public void addCommand( Command command )
	{
		commands.add( command );
	}
	
	public void addDefinition( Definition def )
	{
		defs.add( def );
	}
	
	public void addAssert( Assert assert1 )
	{
		asserts.add( assert1 );
	}
	
	public String toString()
	{
		//TODO
		return "sorry, not implemented yet";
	}
}
