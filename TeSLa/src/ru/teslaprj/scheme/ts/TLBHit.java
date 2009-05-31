package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.TLBIterator;
import ru.teslaprj.ranges.tsiterators.TlbHitIterator;

public class TLBHit extends TLBExists
{
	@Override
	public TLBIterator iterator() {
		return new TlbHitIterator(this);
	}
}
