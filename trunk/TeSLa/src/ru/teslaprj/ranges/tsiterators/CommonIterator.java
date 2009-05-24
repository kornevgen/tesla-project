package ru.teslaprj.ranges.tsiterators;

import java.util.Iterator;

import ru.teslaprj.ranges.Range;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public abstract class CommonIterator implements Iterator<Range>
{
	ProcedureTestSituation ts;
	
	public CommonIterator( ProcedureTestSituation testSituation )
	{
		ts = testSituation;
	}
	
	public ProcedureTestSituation getTestSituation()
	{
		return ts;
	}
}
