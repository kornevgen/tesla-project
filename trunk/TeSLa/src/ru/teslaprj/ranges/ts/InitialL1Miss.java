package ru.teslaprj.ranges.ts;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.BlockRange.TLB;
import ru.teslaprj.scheme.MemoryCommand;

public class InitialL1Miss extends L1Range
{
	public InitialL1Miss(MemoryCommand cmd)
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

	@Override
	public String print()
	{
		return "L1Miss( " + getCommand().getTagset() + " ) from the initial";
	}

}
