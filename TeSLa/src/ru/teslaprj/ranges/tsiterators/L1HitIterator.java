package ru.teslaprj.ranges.tsiterators;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.Cache.CACHETYPE;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.ts.EvictingL1Hit;
import ru.teslaprj.ranges.ts.InitialL1Hit;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.CacheTestSituation;

public class L1HitIterator extends L1Iterator
{
	final Set<MemoryCommand> evictings = new HashSet<MemoryCommand>();

	public L1HitIterator( Cache level, CacheTestSituation testSituation)
	{
		super(level, testSituation);
		assert level.getLevel() == 1 && level.getType() == CACHETYPE.DATA;

		// build all previous evicting in L1 commands
		MemoryCommand cmd = getTestSituation().getCommand();
		for( Command cmd1 : cmd.getScheme().getCommands() )
		{
			if ( cmd1 == cmd )
				break;
			if ( cmd1 instanceof MemoryCommand &&
					((MemoryCommand)cmd1).getCacheSituation(level) instanceof CacheMiss )
			{
				evictings.add( (MemoryCommand)cmd1 );
			}
		}
	}

	int number = 0;
	
	@Override
	public boolean hasNext()
	{
		if ( evictings.isEmpty() )
			return number < 1;
		else 
			return number < 2;
	}

	@Override
	public L1Range next()
	{
		switch( number )
		{
		case 0:
			number = 1;
			return new InitialL1Hit( getTestSituation().getCommand());
		case 1:
			number = 2;
			return new EvictingL1Hit( getTestSituation().getCommand(), evictings );
		default:
			return null;
		}
	}

	@Override
	public void remove() {
	}

}
