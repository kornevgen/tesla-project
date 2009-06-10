package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.scheme.MemoryCommand;

public class UnusefulTlbMiss extends TLBRange
{
	final int minM;
	final int maxM;
	final Set<MemoryCommand> evictings;

	public Set<MemoryCommand> getEvictings() {
		return evictings;
	}
	public int getMinimumM() {
		return minM;
	}
	public int getMaximumM() {
		return maxM;
	}

	public UnusefulTlbMiss(
			MemoryCommand cmd, Set<MemoryCommand> previousMisses,
			int minM, int maxM)
	{
		super( cmd );
		this.maxM = maxM;
		this.minM = minM;
		evictings = previousMisses;
	}

	@Override
	public void visit(L1Range r) throws Inconsistent
	{
		r.visitUnusefulTlbMiss(this);
	}

	@Override
	public String print() {
		return "TLBMiss( " + getCommand().getTagset() + " ) unuseful with m:" + minM + ".." + maxM;
	}

}
