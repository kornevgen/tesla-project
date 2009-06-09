package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class EvictingL1Hit extends L1Range
{
	final Set<MemoryCommand> evictings;
	
	public EvictingL1Hit(MemoryCommand cmd, Set<MemoryCommand> evictings)
	{
		super(cmd);
		this.evictings = evictings;
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
		StringBuffer result = new StringBuffer( "L1Hit( " ).append( getCommand().getTagset() )
				.append( " ) with evicting(#" );
		for( MemoryCommand cmd : evictings )
		{
			result.append( ", " ).append( cmd.getTagset() );
		}
		return result.append(")").toString();
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
