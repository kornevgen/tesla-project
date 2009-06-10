package ru.teslaprj.ranges.ts;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

/** ts \in {ts_1, ..., ts_n} where ts_i is a tagset of previous miss */
public class EvictingL1Hit extends L1Range
{
	final Set<MemoryCommand> evictings;
	
	public EvictingL1Hit(MemoryCommand cmd, Set<MemoryCommand> evictings)
	{
		super(cmd);
		this.evictings = evictings;
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range)
	{
		StringBuffer constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : range.evics )
		{
			constraint.append("(= (getPfn ").append( getCommand().getTagset() ).append(") " +
					"(getPfn " ).append(cmd.getTagset())
				.append("))");
		}
		getContext().postAssert( constraint.append(")" ).toString() );
	
		constraint = new StringBuffer( "(or false ");
		for( MemoryCommand cmd : evictings )
		{
			constraint.append("(= ").append( getCommand().getTagset() ).append(" ")
			.append( cmd.getTagset() ).append(")");
		}		
		getContext().postAssert( constraint.append(")").toString() );
	}

	@Override
	public void visitInitialTlbHit(InitialTlbHit range)
	{
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : getContext().getMfull() )
		{
			constraint.append("(= (getPfn ").append( getCommand().getTagset() ).append(") " +
					"(mk-bv " ).append( getContext().getPfnLength() ).append(" " ).append(l)
				.append("))");
		}
		getContext().postAssert( constraint.append(")").toString());
		
		constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : evictings )
		{
			constraint.append("(= ").append( getCommand().getTagset() ).append(" ")
			.append( cmd.getTagset() ).append(")");
		}		
		getContext().postAssert( constraint.append(")").toString() );
	}

	@Override
	public void visitInitialTlbMiss(InitialTlbMiss range) throws Inconsistent
	{
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>(evictings);
		evs.removeAll(range.ev);		
		if ( evs.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : getContext().getPFNminusMfull() )
		{
			constraint.append("(= (getPfn ").append( getCommand().getTagset() ).append(") " +
					"(mk-bv " ).append( getContext().getPfnLength() ).append(" " ).append(l)
				.append("))");
		}
		getContext().postAssert( constraint.append(")").toString());
		
		constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : evs )
		{
			constraint.append("(= ").append( getCommand().getTagset() ).append(" ")
			.append( cmd.getTagset() ).append(")");
		}		
		getContext().postAssert( constraint.append("))").toString() );
		
		for( MemoryCommand cmd : range.ev )
		{
			getContext().postAssert( new StringBuffer("(/= ")
				.append("(getPfn ").append( getCommand().getTagset() ).append(") ")
				.append("(getPfn ").append( cmd.getTagset() ).append(")")
				.append(")").toString() );
		}
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
	public void visitUnusefulTlbMiss(UnusefulTlbMiss range) throws Inconsistent
	{
		Set<Long> domain = getContext().getMremains( range.minM - 1);
		if ( domain.isEmpty() )
			throw new Inconsistent();
		
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>(evictings);
		evs.removeAll(range.getEvictings());
		if ( evs.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : domain )
		{
			constraint.append("(= (getPfn ").append( getCommand().getTagset() ).append(") " +
					"(mk-bv " ).append( getContext().getPfnLength() ).append(" " ).append(l)
				.append("))");
		}
		getContext().postAssert( constraint.append(")").toString());
		
		constraint = new StringBuffer("(or ");
		for( MemoryCommand cmd : evs )
		{
			constraint.append("(= ").append( getCommand().getTagset() ).append(" ")
			.append( cmd.getTagset() ).append(")");
		}		
		getContext().postAssert( constraint.append("))").toString() );
		
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer("(/= ")
				.append("(getPfn ").append( getCommand().getTagset() ).append(") ")
				.append("(getPfn ").append( cmd.getTagset() ).append(")")
				.append(")").toString() );
		}
	}

	@Override
	public void visitUsefulTlbMiss(UsefulTlbMiss range) throws Inconsistent
	{
		Set<Long> mmm = getContext().getM( range.m - 1 );
		if ( mmm.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.getEvictings() );		
		if ( evs.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		
		StringBuffer constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : evictings )
		{
			constraint.append("(= ").append( getCommand().getTagset() ).append(" ")
			.append( cmd.getTagset() ).append(")");
		}		
		getContext().postAssert( constraint.append("))").toString() );
		
		constraint = new StringBuffer("(or false ");
		for( long mu : mmm )
		{
			constraint.append("(= (getPfn " )
					.append( getCommand().getTagset() ).append(") (mk-bv " )
					.append( getContext().getPfnLength() ).append( mu )
					.append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : evs )
		{
			constraint.append("(= ").append( getCommand().getTagset() ).append(" ")
			.append( cmd.getTagset() ).append(")");
		}		
		getContext().postAssert( constraint.append(")").toString() );
		
				
		// вводим новые переменные
		//(define b_i::(subrange 0 1) (ite ( condition ) 1 0))
		//(assert (>= w-m-k (+ b_i)))
		Set<String> viewedHitTagsets = new HashSet<String>();
		StringBuffer flagsSum = new StringBuffer();
		Set<Long> remainsM = getContext().getMremains( range.getM() - 1 );
		for( MemoryCommand hit : range.getHits() )
		{
			String flag = "b" + getContext().getUniqueNumber();
			flagsSum.append( " " ).append( flag );
			
			StringBuffer useful = new StringBuffer();
			if ( viewedHitTagsets.isEmpty() )
			{
				useful.append("(or false ");
				for( long mu : remainsM )
				{
					useful.append( "(= (getPfn ").append( getCommand().getTagset() ).append(")")
						.append(" (mk-bv ").append( getContext().getPfnLength() ).append( " " )
						.append(mu).append(") )");
				}
				useful.append(")");
			}
			else
			{
				useful.append("(and (or false ");
				for( long mu : remainsM )
				{
					useful.append( "(= (getPfn ").append( getCommand().getTagset() ).append(")")
						.append(" (mk-bv ").append( getContext().getPfnLength() ).append( " " )
						.append(mu).append(") )");
				}
				useful.append(")");
				for( String ts : viewedHitTagsets )
				{
					useful.append( "(/= (getPfn " ).append( getCommand().getTagset() ).append(")")
					.append(" (getPfn " ).append( ts ).append("))");
				}
				useful.append(")");
			}
			
			getContext().postDefine(flag, "(subrange 0 1)", "(ite " + useful.toString() + " 1 0 )" );
			
			viewedHitTagsets.add( hit.getTagset() );
		}
		getContext().postAssert( new StringBuffer("(>= (+ ").append( flagsSum ).append(") ")
				.append( range.getWminusK() - range.getM() ).append(")").toString() );
		
		// ^ts^ \notin { ^ts1^, ^ts2^, ... } 
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer("(/= ")
				.append("(getPfn ").append( getCommand().getTagset() ).append(") ")
				.append("(getPfn ").append( cmd.getTagset() ).append(")")
				.append(")").toString() );
		}
	}

}
