package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.scheme.MemoryCommand;

public class InitialTlbMiss extends TLBRange
{
	Set<MemoryCommand> ev;
	public InitialTlbMiss(MemoryCommand cmd, Set<MemoryCommand> evictings)
	{
		super(cmd);
		ev = evictings;
	}

	@Override
	public void visit(L1Range r)
	{
		r.visitInitialTlbMiss(this);
	}

	public Set<MemoryCommand> getEvictings()
	{
		return ev;
	}
}
