package ru.teslaprj.ranges.tsiterators;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ru.teslaprj.ranges.BlockRange;
import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.ranges.ts.InitialTlbMiss;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.TLBMiss;
import ru.teslaprj.scheme.ts.TLBSituation;

public class TlbMissIterator extends TLBIterator {

	public TlbMissIterator(int assoc, TLBSituation testSituation)
	{
		super(testSituation);
		w = assoc;
		
		MemoryCommand cmd1 = testSituation.getCommand();
		int i = 0;
		for( Command cmd : cmd1.getScheme().getCommands() )
		{
			if ( cmd == cmd1 )
				break;
			if ( cmd instanceof MemoryCommand )
			{
				previousCommands.add( (MemoryCommand)cmd );
				if ( ((MemoryCommand)cmd).getTLBSituation() instanceof TLBMiss )
				{
					previousMisses.add((MemoryCommand)cmd);
					misses.add(i);
				}
				i++;
			}
		}
	}

	int m = 0;
	final int w;
	BlockIterator blockIterator;
	final List<MemoryCommand> previousCommands = new ArrayList<MemoryCommand>();
	final Set<MemoryCommand> previousMisses = new HashSet<MemoryCommand>();
	final List<Integer> misses = new ArrayList<Integer>();
	
	@Override
	public boolean hasNext()
	{
		return m == 0 ||  misses.size() <= w - 1 && hasNext;
	}
	
	boolean hasNext = true;

	@Override
	public TLBRange next()
	{
		try
		{
		//TODO изменить направление изменения m, чтобы сначала просматривались более простые системы
		switch( m )
		{
		case 0:
			m = Math.max( w - previousCommands.size(), 1 );
			blockIterator = new BlockIterator( w-m, previousCommands );
			return new InitialTlbMiss(getTestSituation().getCommand(), previousMisses );
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
				List<MemoryCommand> blockCommands = new ArrayList<MemoryCommand>();
				for( int b : block )
				{
					blockCommands.add( previousCommands.get(b) );
				}
				return new BlockRange.TLB( getTestSituation().getCommand(), m, blockCommands, previousCommands );
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
