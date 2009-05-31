package ru.teslaprj.scheme.ts;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.tsiterators.L1HitIterator;
import ru.teslaprj.ranges.tsiterators.L1Iterator;

public class CacheHit extends CacheTestSituation
{
	public CacheHit( Cache cache ) {
		super(cache);
	}

	@Override
	public L1Iterator iterator()
	{
		switch( cache.getLevel() )
		{
		case 1:
			return new L1HitIterator( cache, this );
		default:
			throw new Error( "создание итератора cacheHit с неизвестным уровнем " 
					+ cache.getLevel() + "-" + cache.getType() );
		}
	}
}
