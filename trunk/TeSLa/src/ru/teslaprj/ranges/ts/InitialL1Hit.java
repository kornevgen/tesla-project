package ru.teslaprj.ranges.ts;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class InitialL1Hit extends L1Range
{

	public InitialL1Hit(MemoryCommand cmd) {
		super(cmd);
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterPFN() )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
		
		constraint = new StringBuffer().append("(or false ");
		for( MemoryCommand cmd : range.getEvictings() )
		{
			constraint.append( "(= " )
				.append( "(getPfn " ).append( getCommand().getTagset() ).append( ")" )
				.append( "(getPfn " ).append( cmd.getTagset() ).append( ")" )
				.append(")");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
	}

	@Override
	public void visitInitialTlbHit(InitialTlbHit range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterM() )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
	}

	@Override
	public void visitInitialTlbMiss(InitialTlbMiss range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterPFNminusM() )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
		
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( "(getPfn " ).append( getCommand().getTagset() ).append( ")" )
				.append( "(getPfn " ).append( cmd.getTagset() ).append( ")" )
				.append(")").toString() );
		}
	}

	@Override
	public String print()
	{
		return "L1Hit( " + getCommand().getTagset() + " ) to the initial";
	}

	@Override
	public void visitUnusefulTlbMiss(UnusefulTlbMiss range)
	{
		// ts \in L \inter [{M<m1>, M<m1+1>,..., M<m2>}]
		Set<Long> domain = getContext().getLinterMrange( range.getMinimumM(), range.getMaximumM() );

		if ( domain.isEmpty() )
		{
			getContext().postAssert("false");
			return;
		}
		
		StringBuffer constraint = new StringBuffer("(or false " );
		for( long l : domain )
		{
			constraint.append( "(= " ).append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		// ^ts^ \notin { ^ts1^, ^ts2^, ... } 
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
			.append("(getPfn " ).append( getCommand().getTagset() ).append(")")
			.append("(getPfn " ).append( cmd.getTagset() ).append(")")
			.append(")").toString() );				
		}
	}

	@Override
	public void visitUsefulTlbMiss(UsefulTlbMiss range)
	{
		Set<Long> domain = getContext().getLinterMremains( range.getM() - 1 );
		if ( domain.isEmpty() )
		{
			getContext().postAssert("false");
			return;
		}
		
		StringBuffer constraint = new StringBuffer("(or false " );
		for( long l : domain )
		{
			constraint.append( "(= " ).append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		// ������ ����� ����������
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
			getContext().postAssert( new StringBuffer( "(/= " )
			.append("(getPfn " ).append( getCommand().getTagset() ).append(")")
			.append("(getPfn " ).append( cmd.getTagset() ).append(")")
			.append(")").toString() );				
		}
	}
}
