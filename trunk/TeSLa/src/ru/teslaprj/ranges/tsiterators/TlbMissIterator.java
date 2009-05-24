package ru.teslaprj.ranges.tsiterators;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import ru.teslaprj.ranges.BlockRange;
import ru.teslaprj.ranges.Range;
import ru.teslaprj.ranges.ts.InitialL1Miss;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;
import ru.teslaprj.scheme.ts.TLBMiss;

public class TlbMissIterator extends CommonIterator {

	public TlbMissIterator(int assoc, ProcedureTestSituation testSituation)
	{
		super(testSituation);
		w = assoc;
		
		Command cmd1 = testSituation.getCommand();
		previousCommands = new ArrayList<Command>();
		misses = new ArrayList<Integer>();
		int i = 0;
		for( Command cmd : cmd1.getScheme().getCommands() )
		{
			if ( cmd == cmd1 )
				break;
			previousCommands.add( cmd );
			if ( cmd.getTLBSituation() instanceof TLBMiss )
				misses.add(i);
			i++;
		}
	}

	int m = 0;
	final int w;
	BlockIterator blockIterator;
	final List<Command> previousCommands;
	List<Integer> misses;
	
	@Override
	public boolean hasNext()
	{
		return m == 0 ||  misses.size() <= w - 1 && hasNext;
	}
	
	boolean hasNext = true;

	@Override
	public Range next()
	{
		try
		{
		//TODO изменить направление изменения m, чтобы сначала просматривались более простые системы
		switch( m )
		{
		case 0:
			m = Math.max( w - previousCommands.size(), 1 );
			blockIterator = new BlockIterator( w-m, previousCommands );
			return new InitialL1Miss(ts.getCommand());
		default:
			if ( m <= w - misses.size() )
			{
				if ( ! blockIterator.hasNext() )
				{
					if ( m == w )
					{
						return null;
					}
					m++;
					blockIterator = new BlockIterator( w-m, previousCommands );
				}
				int[] block = blockIterator.next(); // already sorted
				// if not all misses in block continue!!
				for( int m : misses )
				{
					if ( Arrays.binarySearch(block, m) < 0 )
						return next(); // этот блок содержит не все miss
				}
				return new BlockRange( ts.getCommand(), m, block );
			}
			return null;
		}
		}
		finally
		{
			if ( w - m == misses.size() )
				hasNext = false;
		}
	}

	@Override
	public void remove() {
	}

}
