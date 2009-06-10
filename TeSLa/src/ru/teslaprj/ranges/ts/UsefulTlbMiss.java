package ru.teslaprj.ranges.ts;

import java.util.List;
import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.scheme.MemoryCommand;

public class UsefulTlbMiss extends TLBRange {

	final int m;
	final Set<MemoryCommand> previousMisses;
	final List<MemoryCommand> previousHits;
	final int wMinusK;
	
	public UsefulTlbMiss(
			MemoryCommand cmd,
			Set<MemoryCommand> previousMisses,
			List<MemoryCommand> previousHits,
			int m, int wMinusK )
	{
		super( cmd );
		this.m = m;
		this.previousHits = previousHits;
		this.previousMisses = previousMisses;
		this.wMinusK = wMinusK;
	}

	@Override
	public void visit(L1Range r) throws Inconsistent {
		r.visitUsefulTlbMiss(this);
	}

	@Override
	public String print() {
		return "TLBMiss( " + getCommand().getTagset() + " ) useful with m=" + m;
	}

	public int getM() {
		return m;
	}

	public List<MemoryCommand> getHits() {
		return previousHits;
	}

	public Set<MemoryCommand> getEvictings() {
		return previousMisses;
	}

	public int getWminusK()
	{
		return wMinusK;
	}

}
