package ru.teslaprj.ranges;

import java.util.List;

import ru.teslaprj.scheme.MemoryCommand;

public interface BlockRange
{
	public static class L1 extends L1Range implements BlockRange 
	{
		public L1(MemoryCommand cmd, int m, int[] commandIndexes) {
			super(cmd);
		}		
	}
	public static class TLB extends TLBRange implements BlockRange
	{
		final int m;
		final List<MemoryCommand> block;
		final List<MemoryCommand> previous;
		
		public List<MemoryCommand> getPreviousCommands() {
			return previous;
		}

		public List<MemoryCommand> getBlock() {
			return block;
		}

		public int getTagIndex()
		{
			return m;
		}

		public TLB
				( MemoryCommand cmd
				, int m
				, List<MemoryCommand> block
				, List<MemoryCommand> previousCommands
				)
		{
			super(cmd);
			this.m = m;
			this.block = block;
			previous = previousCommands;
		}

		@Override
		public void visit(L1Range r)
		{
			r.visitBlockTlbMiss(this);
		}
	}
}
