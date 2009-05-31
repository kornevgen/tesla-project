package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.TLBIterator;
import ru.teslaprj.ranges.tsiterators.TlbMissIterator;

public class TLBMiss extends TLBExists
{
	final int assoc;
	public TLBMiss( int assoc )
	{
		this.assoc = assoc;
	}
	
	@Override
	public TLBIterator iterator( ) {
		return new TlbMissIterator( assoc, this ) ;
	}
}
