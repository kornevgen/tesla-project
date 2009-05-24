package ru.teslaprj.ranges.tsiterators;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.Range;
import ru.teslaprj.ranges.ts.EvictingL1Hit;
import ru.teslaprj.ranges.ts.InitialL1Hit;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class L1HitIterator extends CommonIterator {

	public L1HitIterator(ProcedureTestSituation testSituation) {
		super(testSituation);
	}

	int number = 0;
	
	@Override
	public boolean hasNext() {
		return number > 1;
	}

	@Override
	public Range next()
	{
		switch( number )
		{
		case 0:
			number = 1;
			return new InitialL1Hit(ts.getCommand());
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
			return new EvictingL1Hit( cmd, evictings );
		default:
			return null;
		}
	}

	@Override
	public void remove() {
	}

}
