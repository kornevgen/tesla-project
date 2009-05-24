package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.CommonIterator;
import ru.teslaprj.ranges.tsiterators.L1MissIterator;

public class CacheMiss extends CacheTestSituation
{
	int assoc;
	public CacheMiss(int level, int assoc) {
		super(level);
		this.assoc = assoc;
	}

	@Override
	public CommonIterator iterator()
	{
		switch( level )
		{
		case 1:
			return new L1MissIterator(assoc, this);
		default:
			throw new Error( "неизвестный уровень при создании итератора для L1Miss: " + level );
		}
	}
}
