package ru.teslaprj.ranges.tsiterators;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.ts.InitialL1Miss;
import ru.teslaprj.ranges.ts.UsefulL1Miss;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.CacheTestSituation;

public class L1MissIterator extends L1Iterator
{

	public L1MissIterator( Cache level, CacheTestSituation testSituation)
	{
		super( level, testSituation);
		w = level.getSectionNumber();

		MemoryCommand cmd1 = testSituation.getCommand();
		
		// search for the last miss before cmd1
		LastMissSearching:
		{
			List<Command> cmds = cmd1.getScheme().getCommands();
			for( int i = cmds.indexOf(cmd1) - 1; i >= 0; i-- )
			{
				Command lastMiss = cmds.get(i);
				if ( lastMiss instanceof MemoryCommand )
				{
					if ( ((MemoryCommand)lastMiss).getCacheSituation(level) instanceof CacheMiss )
					{
						// get previousCommands and calculate N
						for( Command c : cmds )
						{
							if ( c == lastMiss )
							{
								break;
							}
							if ( c instanceof MemoryCommand )
							{
								if ( ((MemoryCommand)c).getCacheSituation(level) instanceof CacheMiss )
								{
									previousMisses.add( (MemoryCommand)c );
								}
								else if ( ((MemoryCommand)c).getCacheSituation(level) instanceof CacheHit )
								{
									previousHits.add( (MemoryCommand)c );
								}
							}
						}
						
						int N = previousHits.size() + previousMisses.size();
						m = Math.max(1, w - N);
						
						allPreviousMisses.addAll( previousMisses );
						allPreviousMisses.add( (MemoryCommand)lastMiss );
						
						break LastMissSearching;
					}
				}
			}
			
			// last miss is not found
			m = w+1;
		}
	}

	private boolean initialVisited = false;
	private int m;	
	final int w;
	
	final Set<MemoryCommand> allPreviousMisses = new HashSet<MemoryCommand>();
	final Set<MemoryCommand> previousMisses = new HashSet<MemoryCommand>();
	final List<MemoryCommand> previousHits = new ArrayList<MemoryCommand>();
	
	@Override
	public boolean hasNext()
	{
		return !initialVisited || m <= w;
	}

	@Override
	public L1Range next()
	{
		if ( ! initialVisited )
		{
			initialVisited = true;
			return new InitialL1Miss( getTestSituation().getCommand(), allPreviousMisses );
		}
		
		return new UsefulL1Miss( 
				getTestSituation().getCommand(),
				m++, w,
				allPreviousMisses,
				previousMisses, previousHits );
	}

	@Override
	public void remove() {
	}

}
