package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.TLBIterator;
import ru.teslaprj.scheme.MemoryCommand;

public abstract class TLBSituation extends ProcedureTestSituation
{
	@Override
	public MemoryCommand getCommand() {
		return (MemoryCommand)super.getCommand();
	}

	public abstract TLBIterator iterator();
}
