package ru.teslaprj.scheme.ts;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.tsiterators.L1Iterator;
import ru.teslaprj.scheme.MemoryCommand;

public abstract class CacheTestSituation extends ProcedureTestSituation
{
	@Override
	public MemoryCommand getCommand() {
		return (MemoryCommand)super.getCommand();
	}

	public abstract L1Iterator iterator();
	
	Cache cache;
	
	public CacheTestSituation( Cache cache )
	{
		this.cache = cache;
	}
}
