package ru.teslaprj.ranges.tsiterators;

import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.scheme.ts.TLBSituation;

public abstract class TLBIterator extends CommonIterator<TLBRange> {

	public TLBIterator(TLBSituation testSituation) {
		super(testSituation);
	}

	public TLBSituation getTestSituation()
	{
		return (TLBSituation)ts;
	}
}
