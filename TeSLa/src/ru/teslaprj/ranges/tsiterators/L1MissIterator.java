package ru.teslaprj.ranges.tsiterators;

import java.util.ArrayList;
import java.util.List;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.BlockRange;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.ts.InitialL1Miss;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.CacheTestSituation;

public class L1MissIterator extends L1Iterator {

	public L1MissIterator( Cache level, CacheTestSituation testSituation)
	{
		super( level, testSituation);
		w = level.getSectionNumber();
		
		MemoryCommand cmd1 = testSituation.getCommand();
		previousCommands = new ArrayList<MemoryCommand>();
		for( Command cmd : cmd1.getScheme().getCommands() )
		{
			if ( cmd == cmd1 )
				break;
			if ( cmd instanceof MemoryCommand )
				previousCommands.add( (MemoryCommand)cmd );
		}
	}

	int m = 0;
	final int w;
	BlockIterator blockIterator;
	final List<MemoryCommand> previousCommands;
	
	@Override
	public boolean hasNext()
	{
		return ( m == 0 || previousCommands.size() >= w - m ) && ( m < w || blockIterator.hasNext() );
	}

	@Override
	public L1Range next()
	{
		//TODO изменить направление изменения m, чтобы сначала просматривались более простые системы
		switch( m )
		{
		case 0:
			m = Math.max( w - previousCommands.size(), 1 );
			blockIterator = new BlockIterator( w-m, previousCommands );
			return new InitialL1Miss( getTestSituation().getCommand());
		default:
			if ( m <= w )
			{
				if ( blockIterator.hasNext() )
				{
					return new BlockRange.L1( getTestSituation().getCommand(), m, blockIterator.next() );
				}
				else
				{
					if ( m == w )
					{
						return null;
					}
					m++;
					blockIterator = new BlockIterator( w-m, previousCommands );
					return new BlockRange.L1( getTestSituation().getCommand(), m, blockIterator.next() );
				}
			}
			return null;
		}
	}

	@Override
	public void remove() {
	}

}
