package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.BlockRange.TLB;
import ru.teslaprj.scheme.MemoryCommand;

public class EvictingL1Hit extends L1Range {

	public EvictingL1Hit(MemoryCommand cmd, Set<MemoryCommand> evictings)
	{
		super(cmd);
	}

	@Override
	public void visitBlockTlbMiss(TLB range) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitInitialTlbHit(InitialTlbHit range) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitInitialTlbMiss(InitialTlbMiss range) {
		// TODO Auto-generated method stub
		
	}

}
