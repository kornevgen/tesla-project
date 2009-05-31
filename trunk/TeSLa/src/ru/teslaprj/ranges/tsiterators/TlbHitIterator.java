package ru.teslaprj.ranges.tsiterators;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.ranges.ts.EvictingTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbHit;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.TLBMiss;
import ru.teslaprj.scheme.ts.TLBSituation;

public class TlbHitIterator extends TLBIterator {

	public TlbHitIterator(TLBSituation testSituation) {
		super(testSituation);
	}

	int number = 0;
	
	@Override
	public boolean hasNext() {
		return number < 2;
	}

	@Override
	public TLBRange next()
	{
		switch( number )
		{
		case 0:
			number = 1;
			return new InitialTlbHit(getTestSituation().getCommand());
		case 1:
			number = 2;
			Set<MemoryCommand> evictings = new HashSet<MemoryCommand>();
			// build all previous evicting in TLB commands
			MemoryCommand cmd = getTestSituation().getCommand();
			for( Command cmd1 : cmd.getScheme().getCommands() )
			{
				if ( cmd1 == cmd )
					break;
				if ( cmd1 instanceof MemoryCommand
						&& ((MemoryCommand)cmd1).getTLBSituation() instanceof TLBMiss )
				{
					evictings.add( (MemoryCommand)cmd1 );
				}
			}
			return new EvictingTlbHit( cmd, evictings );
		default:
			return null;
		}
	}

	@Override
	public void remove() {
	}

}
