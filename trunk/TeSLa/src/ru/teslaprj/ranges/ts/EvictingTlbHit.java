package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.TLBRange;
import ru.teslaprj.scheme.MemoryCommand;

public class EvictingTlbHit extends TLBRange
{
	final Set<MemoryCommand> evics;
	
	public EvictingTlbHit(MemoryCommand cmd, Set<MemoryCommand> evictings)
	{
		super(cmd);
		evics = evictings; 
	}

	@Override
	public void visit(L1Range r) throws Inconsistent
	{
		r.visitEvictingTlbHit(this);
	}

	public Set<MemoryCommand> getEvictings()
	{
		return evics;
	}

	@Override
	public String print()
	{
		StringBuffer result = new StringBuffer( "TLBHit( " ).append( getCommand().getTagset() )
				.append( " ) with evicting(#" );
		for( MemoryCommand cmd : evics )
		{
			result.append( ", ^" ).append( cmd.getTagset() ).append("^");
		}
		return result.append(")").toString();
	}
}
