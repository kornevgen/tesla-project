package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.CommonIterator;
import ru.teslaprj.ranges.tsiterators.TlbHitIterator;

public class TLBHit extends TLBExists
{
	@Override
	public CommonIterator iterator() {
		return new TlbHitIterator(this);
	}
}
