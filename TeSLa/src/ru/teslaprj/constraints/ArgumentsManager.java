package ru.teslaprj.constraints;

import java.util.HashMap;
import java.util.Map;

import ru.teslaprj.Cache;
import ru.teslaprj.constraints.args.Set;
import ru.teslaprj.scheme.Command;

public class ArgumentsManager
{
	private Map<String, Argument> args = new HashMap<String, Argument>();
	
	public void addArgument( Argument arg )
	{
		if ( args.containsKey(arg.getName()) )
		{
			throw new Error("name " + arg.getName() + " is already used");
		}
		args.put( arg.getName(), arg );
	}
	
	public Argument getArgument( String name )
	{
		return args.get( name );
	}
	
	public Set getSet( Command cmd, Cache cache )
	{
		for( Argument arg : args.values() )
		{
			if ( arg instanceof Set )
			{
				Set set = (Set)arg;
				if ( set.getCommand().equals( cmd ) && set.getCache().equals(cache) )
					return set;
			}
		}
		return null;
	}
}
