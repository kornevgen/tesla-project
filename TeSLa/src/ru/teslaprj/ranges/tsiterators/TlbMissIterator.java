package ru.teslaprj.ranges.tsiterators;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.ranges.ts.InitialTlbMiss;
import ru.teslaprj.ranges.ts.UnusefulTlbMiss;
import ru.teslaprj.ranges.ts.UsefulTlbMiss;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.TLBHit;
import ru.teslaprj.scheme.ts.TLBMiss;
import ru.teslaprj.scheme.ts.TLBSituation;

public class TlbMissIterator extends TLBIterator
{
	public TlbMissIterator(int assoc, TLBSituation testSituation)
	{
		super(testSituation);
		w = assoc;
		
		// вычислить k и N
		// если их нельзя вычислить, то варианты "без полезностей" невозможны (только initial)
		MemoryCommand cmd = testSituation.getCommand();
		MainPreviousMissFinding:
		{
			for( int i = cmd.getScheme().getCommands().indexOf(cmd) - 1; i >= 0; i-- )
			{
				Command cc = cmd.getScheme().getCommands().get(i); 
				if ( cc instanceof MemoryCommand 
						&& ((MemoryCommand)cc).getTLBSituation() instanceof TLBMiss )
				{
					Set<MemoryCommand> previousMisses = new HashSet<MemoryCommand>();
					
					// main previous miss is found
					for( Command c : cmd.getScheme().getCommands() )
					{
						if ( c == cc )
							break;
						if ( c instanceof MemoryCommand )
						{
							if ( ((MemoryCommand)c).getTLBSituation() instanceof TLBHit )
								previousHits.add((MemoryCommand)c);
							else if ( ((MemoryCommand)c).getTLBSituation() instanceof TLBMiss )
								previousMisses.add((MemoryCommand)c);
						}
					}
					int k = previousMisses.size();
					int N = previousHits.size();
					wMinusK = w - k;

					if ( w - k <= 1 || N == 0 )
					{
						iterationsLeft = 2;
					}
					else if ( w-k-N < 1 )
					{
						iterationsLeft = w-k+1;
					}
					else
					{
						iterationsLeft = N+2;
					}
					
					allPreviousMisses.addAll( previousMisses );
					allPreviousMisses.add( (MemoryCommand) cc );
					
					break MainPreviousMissFinding;
				}
			}
			
			// main miss is not found
			iterationsLeft = 1;
			wMinusK = 0;//fake
		}
		
		// итератор выдает m из подмножества множества 1..w, определяемого k и N.
		// if w-k <= 1 -> всего один вариант: без полезностей m:1..w
		// else N = 0 -> всего один вариант: без полезностей m:w-k..w
		// else w-k-N < 1 -> w-k вариантов: без полезностей m:w-k..w & с полезностями m:1..w-k-1
		// else -> N+1 вариантов: без полезностей m:w-k..w & с полезностями m:w-k-N..w-k-1
		// ну и + initialMiss
	}

	final int w;
	private int iterationsLeft;
	final List<MemoryCommand> previousHits = new ArrayList<MemoryCommand>();
	final Set<MemoryCommand> allPreviousMisses = new HashSet<MemoryCommand>();
	private int wMinusK;
	// unuseful M : max(1, w-k) .. w
	// useful M : w-k - 1 downto iterationsLeft = 0  

	@Override
	public void remove() {
	}


	@Override
	public boolean hasNext() {
		return iterationsLeft > 0;
	}


	private boolean first = true;
	private boolean unuseful = true;
	
	@Override
	public TLBRange next()
	{
		if ( first )
		{
			first = false;
			iterationsLeft --;
			return new InitialTlbMiss(
					getTestSituation().getCommand(),
					allPreviousMisses );
		}
		else if ( unuseful )
		{
			unuseful = false;
			iterationsLeft -- ;
			return new UnusefulTlbMiss(
					getTestSituation().getCommand(),
					allPreviousMisses,
					Math.max(1, wMinusK), w );
		}
		
		int m = wMinusK - 1;
		
		wMinusK --;
		iterationsLeft --;
		
		return new UsefulTlbMiss(
				getTestSituation().getCommand(),
				allPreviousMisses,
				previousHits,
				m, w - allPreviousMisses.size() - 1 );
	}

}
