package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
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
	public void visit(L1Range r) throws Inconsistent
	{
		r.visitInitialTlbMiss(this);
	}

	public Set<MemoryCommand> getEvictings()
	{
		return ev;
	}

	@Override
	public String print()
	{
		StringBuffer result = new StringBuffer( "TLBMiss( " )
		.append( getCommand().getTagset() ).append( " ) from initial DTLB with evictings(#" );
		for( MemoryCommand cmd : ev )
		{
			result.append( ", ^" ).append( cmd.getTagset() ).append("^");
		}
		return result.append(")").toString();
	}

	@Override
	public void visit1(L1Range r) throws Inconsistent {
		r.visit1InitialTlbMiss(this);
	}
}
