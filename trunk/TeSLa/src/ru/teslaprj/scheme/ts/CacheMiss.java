package ru.teslaprj.scheme.ts;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.tsiterators.L1Iterator;
import ru.teslaprj.ranges.tsiterators.L1MissIterator;

public class CacheMiss extends CacheTestSituation
{
	public CacheMiss( Cache cache ) {
		super( cache );
	}

	@Override
	public L1Iterator iterator()
	{
		switch( cache.getLevel() )
		{
		case 1:
			return new L1MissIterator( cache, this);
		default:
			throw new Error( "неизвестный уровень при создании итератора для L1Miss: " 
					+ cache.getLevel() + "-" + cache.getType() );
		}
	}
}
