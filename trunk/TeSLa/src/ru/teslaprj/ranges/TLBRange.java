package ru.teslaprj.ranges;

import ru.teslaprj.scheme.MemoryCommand;

public abstract class TLBRange extends Range
{

	public TLBRange(MemoryCommand cmd) {
		super(cmd);
	}
	
	// stage 0
	public abstract void visit( L1Range r ) throws Inconsistent;

	// stage 1
	public abstract void visit1(L1Range r) throws Inconsistent;
}
