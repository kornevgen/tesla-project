package ru.teslaprj.ranges.tsiterators;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.Range;
import ru.teslaprj.ranges.ts.EvictingTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbHit;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class TlbHitIterator extends CommonIterator {

	public TlbHitIterator(ProcedureTestSituation testSituation) {
		super(testSituation);
	}

	int number = 0;
	
	@Override
	public boolean hasNext() {
		return number < 2;
	}

	@Override
	public Range next()
	{
		switch( number )
		{
		case 0:
			number = 1;
			return new InitialTlbHit(ts.getCommand());
		case 1:
			number = 2;
			Set<Command> evictings = new HashSet<Command>();
			// build all previous evicting in L1 commands
			Command cmd = ts.getCommand();
			for( Command cmd1 : cmd.getScheme().getCommands() )
			{
				if ( cmd1 == cmd )
					break;
				if ( cmd1.getCacheSituation(1) instanceof CacheMiss )
				{
					evictings.add( cmd1 );
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
