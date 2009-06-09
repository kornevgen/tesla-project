package ru.teslaprj.ranges.ts;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class InitialL1Miss extends L1Range
{
	public InitialL1Miss(MemoryCommand cmd)
	{
		super(cmd);
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitInitialTlbHit(InitialTlbHit range)
	{
		for( long l : getContext().getLinterM() )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))").toString() );
		}
		
		for( MemoryCommand cmd : evictings )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append( " ").append( cmd.getTagset() )
				.append(")").toString() );
		}
		
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long mu : getContext().getMfull() )
		{
			constraint.append(" (= (getPfn ").append( getCommand().getTagset() )
				.append( ") (mk-bv " ).append( getContext().getPfnLength() )
				.append(" " ).append( mu ).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
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

	@Override
	public void visitUnusefulTlbMiss(UnusefulTlbMiss range) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitUsefulTlbMiss(UsefulTlbMiss range) {
		// TODO Auto-generated method stub
		
	}

}
