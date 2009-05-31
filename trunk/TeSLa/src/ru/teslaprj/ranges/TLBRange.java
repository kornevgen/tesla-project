package ru.teslaprj.ranges;

import ru.teslaprj.scheme.MemoryCommand;

public abstract class TLBRange extends Range
{

	public TLBRange(MemoryCommand cmd) {
		super(cmd);
	}
	
	public abstract void visit( L1Range r );

}
