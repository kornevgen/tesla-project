package ru.teslaprj.ranges.tsiterators;

import java.util.ArrayList;
import java.util.List;

import ru.teslaprj.ranges.BlockRange;
import ru.teslaprj.ranges.Range;
import ru.teslaprj.ranges.ts.InitialL1Miss;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class L1MissIterator extends CommonIterator {

	public L1MissIterator(int assoc, ProcedureTestSituation testSituation)
	{
		super(testSituation);
		w = assoc;
		
		Command cmd1 = testSituation.getCommand();
		previousCommands = new ArrayList<Command>();
		for( Command cmd : cmd1.getScheme().getCommands() )
		{
			if ( cmd == cmd1 )
				break;
			previousCommands.add( cmd );
		}
	}

	int m = 0;
	final int w;
	BlockIterator blockIterator;
	final List<Command> previousCommands;
	
	@Override
	public boolean hasNext()
	{
		return ( m == 0 || previousCommands.size() >= w - m ) && ( m < w || blockIterator.hasNext() );
	}

	@Override
	public Range next()
	{
		//TODO изменить направление изменения m, чтобы сначала просматривались более простые системы
		switch( m )
		{
		case 0:
			m = Math.max( w - previousCommands.size(), 1 );
			blockIterator = new BlockIterator( w-m, previousCommands );
			return new InitialL1Miss(ts.getCommand());
		default:
			if ( m <= w )
			{
				if ( blockIterator.hasNext() )
				{
					return new BlockRange( ts.getCommand(), m, blockIterator.next() );
				}
				else
				{
					if ( m == w )
					{
						return null;
					}
					m++;
					blockIterator = new BlockIterator( w-m, previousCommands );
					return new BlockRange( ts.getCommand(), m, blockIterator.next() );
				}
			}
			return null;
		}
	}

	@Override
	public void remove() {
	}

}
