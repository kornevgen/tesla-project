package ru.teslaprj.ranges.ts;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class InitialL1Hit extends L1Range
{

	public InitialL1Hit(MemoryCommand cmd) {
		super(cmd);
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range) throws Inconsistent
	{
		Set<Long> domain = getContext().getLinterPFN();
		if ( domain.isEmpty() )
			throw new Inconsistent();
		
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : domain )
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
	public void visitInitialTlbHit(InitialTlbHit range) throws Inconsistent
	{
		Set<Long> domain = getContext().getLinterM();
		if ( domain.isEmpty() )
			throw new Inconsistent();
		
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : domain )
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
	public void visitInitialTlbMiss(InitialTlbMiss range) throws Inconsistent
	{
		Set<Long> domain = getContext().getLinterPFNminusM();
		if ( domain.isEmpty() )
			throw new Inconsistent();
		
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : domain )
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
		throws Inconsistent
	{
		// ts \in L \inter [{M<m1>, M<m1+1>,..., M<m2>}]
		Set<Long> domain = getContext().getLinterMrange( range.getMinimumM(), range.getMaximumM() );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
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
	public void visitUsefulTlbMiss(UsefulTlbMiss range) throws Inconsistent
	{
		Set<Long> domain = getContext().getLinterM( range.getM() - 1 );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		StringBuffer constraint = new StringBuffer("(or false " );
		for( long l : domain )
		{
			constraint.append( "(= " ).append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))");
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
			getContext().postAssert( new StringBuffer( "(/= " )
			.append("(getPfn " ).append( getCommand().getTagset() ).append(")")
			.append("(getPfn " ).append( cmd.getTagset() ).append(")")
			.append(")").toString() );				
		}
	}

	@Override
	public void visit1EvictingTlbHit(EvictingTlbHit range) throws Inconsistent
	{
		if ( range.evics.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		getContext().postDefine(getCommand().getTagset(), "Tagset", "");
		getContext().postDefine(getCommand().getValueOfTagset(), "tagset", "");
		
		// pfntype constraints
		Set<Integer> allowedPfntypes = new HashSet<Integer>();
		allowedPfntypes.add(2);
		if ( ! getContext().getLinterM().isEmpty() )
		{
			allowedPfntypes.add(0);
		}
		if ( ! getContext().getLinterPFNminusM().isEmpty() )
		{
			allowedPfntypes.add(1);
		}
		StringBuffer constraint = new StringBuffer("(or false ");
		for( int pfnType : allowedPfntypes )
		{
			constraint.append("(= (pfntype ").append( getCommand().getTagset() )
			.append(") ").append(pfnType).append(")");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		// constraints on previous evictings from TLB 
		constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : range.evics )
		{
			constraint.append("(and true ");
			constraint.append("(= (pfntype " ).append( getCommand().getTagset() )
				.append(") (pfntype ").append( cmd.getTagset() ).append("))");
			constraint.append("(or false ");
			if ( allowedPfntypes.contains(2) )
			{
				constraint.append("(and (= (pfntype ").append(cmd.getTagset())
				.append(") 2) (pfneq " ).append(cmd.getTagset())
				.append(" ").append(getCommand().getTagset()).append("))");
			}
			if ( allowedPfntypes.contains(0) || allowedPfntypes.contains(1))
			{
				constraint.append("(and (< (pfntype ").append(cmd.getTagset())
				.append(") 2) (getPfn " ).append(getCommand().getValueOfTagset())
				.append(") (getPfn ").append(cmd.getValueOfTagset()).append("))");
			}
			constraint.append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
	}

	@Override
	public void visit1InitialTlbHit(InitialTlbHit range) throws Inconsistent {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visit1InitialTlbMiss(InitialTlbMiss range) throws Inconsistent
	{
		getContext().postDefine(getCommand().getTagset(), "Tagset", "");
		getContext().postDefine(getCommand().getValueOfTagset(), "tagset", "");
		
		// pfntype constraints
		Set<Integer> allowedPfntypes = new HashSet<Integer>();
		allowedPfntypes.add(2);
		if ( ! getContext().getLinterPFNminusM().isEmpty() )
		{
			allowedPfntypes.add(1);
		}
		StringBuffer constraint = new StringBuffer("(or false ");
		for( int pfnType : allowedPfntypes )
		{
			constraint.append("(= (pfntype ").append( getCommand().getTagset() )
			.append(") ").append(pfnType).append(")");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		// constraints on previous evictings from TLB
		for( MemoryCommand cmd : range.getEvictings() )
		{
			constraint = new StringBuffer("(=> (= (pfntype ")
			.append( getCommand().getTagset() ).append(") (pfntype ")
			.append( cmd.getTagset() ).append(")) (or false ")
			.append( "(and (= (pfntype ").append( getCommand().getTagset() )
			.append(") 2) (not (pfneq ").append( getCommand().getTagset() )
			.append(" " ).append( cmd.getTagset()).append(")))")
			.append( "(and (= (pfntype ").append( getCommand().getTagset() )
			.append(") 1) (/= (getPfn ").append( getCommand().getValueOfTagset() )
			.append(") (getPfn ").append(cmd.getValueOfTagset()).append(")))")
			.append("))");
			
			getContext().postAssert( constraint.toString() );
		}
	}

	@Override
	public void visit1UnusefulTlbMiss(UnusefulTlbMiss range)
			throws Inconsistent {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visit1UsefulTlbMiss(UsefulTlbMiss range) throws Inconsistent {
		// TODO Auto-generated method stub
		
	}
}
