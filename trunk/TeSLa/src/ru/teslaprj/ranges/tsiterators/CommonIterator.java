package ru.teslaprj.ranges.tsiterators;

import java.util.Iterator;

import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public abstract class CommonIterator<E> implements Iterator<E>
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
