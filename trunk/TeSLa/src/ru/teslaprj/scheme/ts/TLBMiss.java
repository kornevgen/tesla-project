package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.CommonIterator;
import ru.teslaprj.ranges.tsiterators.TlbMissIterator;

public class TLBMiss extends TLBExists
{
	final int assoc;
	public TLBMiss( int assoc )
	{
		this.assoc = assoc;
	}
	
	@Override
	public CommonIterator iterator( ) {
		return new TlbMissIterator( assoc, this ) ;
	}
}
