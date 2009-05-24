package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.CommonIterator;
import ru.teslaprj.ranges.tsiterators.L1HitIterator;

public class CacheHit extends CacheTestSituation
{
	public CacheHit(int level) {
		super(level);
	}

	@Override
	public CommonIterator iterator()
	{
		switch( level )
		{
		case 1:
			return new L1HitIterator( this );
		default:
			throw new Error( "создание итератора cacheHit с неизвестным уровнем " + level );
		}
	}
}
