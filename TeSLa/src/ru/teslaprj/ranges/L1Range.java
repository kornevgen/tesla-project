package ru.teslaprj.ranges;

import ru.teslaprj.scheme.MemoryCommand;

public abstract class L1Range extends Range implements TLBRangesVisitor
{

	public L1Range(MemoryCommand cmd) {
		super(cmd);
	}

	
	public void visit( TLBRange range )
	{
		range.visit( this );
	}

}
