package ru.teslaprj.ranges.tsiterators;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.ts.CacheTestSituation;

public abstract class L1Iterator extends CommonIterator<L1Range>
{
	Cache L1;
	
	public L1Iterator( Cache L1, CacheTestSituation testSituation)
	{
		super(testSituation);
		this.L1 = L1;
	}

	public CacheTestSituation getTestSituation()
	{
		return (CacheTestSituation)ts;
	}
	
}
