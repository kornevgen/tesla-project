package ru.teslaprj.ranges;

import ru.teslaprj.ranges.ts.EvictingTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbMiss;
import ru.teslaprj.ranges.ts.UnusefulTlbMiss;
import ru.teslaprj.ranges.ts.UsefulTlbMiss;
import ru.teslaprj.scheme.MemoryCommand;

public interface BlockRange
{
	public static class L1 extends L1Range implements BlockRange 
	{
		final int m;
		public L1(MemoryCommand cmd, int m, int[] commandIndexes) {
			super(cmd);
			this.m = m;
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
			return "L1Miss( " + getCommand().getTagset() + " )-Block with m=" + m;
		}

		@Override
		public void visitUnusefulTlbMiss(UnusefulTlbMiss range) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void visitUsefulTlbMiss(UsefulTlbMiss range) {
			// TODO Auto-generated method stub
			
		}		
	}
}
