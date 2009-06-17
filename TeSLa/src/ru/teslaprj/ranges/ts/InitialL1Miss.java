package ru.teslaprj.ranges.ts;

import java.util.HashSet;
import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class InitialL1Miss extends L1Range
{
	final Set<MemoryCommand> evictings;
	
	public InitialL1Miss(MemoryCommand cmd, Set<MemoryCommand> allPreviousMisses)
	{
		super(cmd);
		evictings = allPreviousMisses;
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range)
	{
		for( long l : getContext().getLinterPFN() )
		{
			getContext().postAssert( new StringBuffer("(/= ")
				.append( getCommand().getTagset() )
				.append(" (mk-bv ").append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))").toString() );
		}
		
		for( MemoryCommand cmd : evictings )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append( " ").append( cmd.getTagset() )
				.append(")").toString() );
		}
		
		StringBuffer constraint = new StringBuffer( "(or false ");
		for( MemoryCommand cmd : range.evics )
		{
			constraint.append(" (= (getPfn ").append( getCommand().getTagset() )
			.append(") (getPfn " ).append( cmd.getTagset() ).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
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
	public void visitInitialTlbMiss(InitialTlbMiss range)
	{
		for( long l : getContext().getLinterPFNminusM() )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))").toString() );
		}

		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.ev );
		for( MemoryCommand cmd : evs )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append( " ").append( cmd.getTagset() )
				.append(")").toString() );
		}
		
		StringBuffer constraint = new StringBuffer( "(or false " );
		for( long l : getContext().getPFNminusMfull() )
		{
			constraint.append("(= (getPfn ").append( getCommand().getTagset() )
				.append(") (mk-bv ").append( getContext().getPfnLength() )
				.append( " " ).append(l).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		for( MemoryCommand cmd : range.ev )
		{
			getContext().postAssert( new StringBuffer("(/= (getPfn ")
				.append( getCommand().getTagset() ).append(") (getPfn ")
				.append( cmd.getTagset() ).append("))").toString() );
		}
	}

	@Override
	public String print()
	{
		return "L1Miss( " + getCommand().getTagset() + " ) from the initial";
	}

	@Override
	public void visitUnusefulTlbMiss(UnusefulTlbMiss range)
	{
		for( long l : getContext().getLinterMrange(range.minM, range.maxM) )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))").toString() );
		}

		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.getEvictings() );
		for( MemoryCommand cmd : evs )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append( " ").append( cmd.getTagset() )
				.append(")").toString() );
		}
		
		StringBuffer constraint = new StringBuffer( "(or false " );
		for( long l : getContext().getMremains(range.minM - 1) )
		{
			constraint.append("(= (getPfn ").append( getCommand().getTagset() )
				.append(") (mk-bv ").append( getContext().getPfnLength() )
				.append( " " ).append(l).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
		
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer("(/= (getPfn ")
				.append( getCommand().getTagset() ).append(") (getPfn ")
				.append( cmd.getTagset() ).append("))").toString() );
		}
	}

	@Override
	public void visitUsefulTlbMiss(UsefulTlbMiss range)
	{
		for( long l : getContext().getLinterM(range.m - 1) )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" " ).append(l).append("))").toString() );
		}

		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.getEvictings() );
		for( MemoryCommand cmd : evs )
		{
			getContext().postAssert( new StringBuffer("(/= " )
				.append( getCommand().getTagset() )
				.append( " ").append( cmd.getTagset() )
				.append(")").toString() );
		}
		
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long mu : getContext().getM( range.m - 1 ) )
		{
			constraint.append("(= (getPfn " )
					.append( getCommand().getTagset() ).append(") (mk-bv " )
					.append( getContext().getPfnLength() ).append( mu )
					.append("))");
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

	@Override
	public void visit1EvictingTlbHit(EvictingTlbHit range) throws Inconsistent {
		// TODO Auto-generated method stub
		
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
		
		StringBuffer constraint = new StringBuffer("(or ")
		.append( "(and (= 1 (pfntype ").append(getCommand().getTagset()).append("))")
		.append("(value_ts ").append(getCommand().getTagset()).append(" ")
		.append( getCommand().getValueOfTagset() ).append(")");
		for( long l : getContext().getLinterM() )
		{
			constraint.append("(/= ").append(getCommand().getValueOfTagset() )
			.append(" (mk-bv ").append( getContext().getTagsetLength() )
			.append(" ").append(l).append("))");
		}
		constraint.append("(or ");
		for( long l : getContext().getPFNminusMfull() )
		{
			constraint.append("(= (getPfn ").append(getCommand().getValueOfTagset())
			.append(") (mk-bv ").append( getContext().getPfnLength() )
			.append(" ").append(l).append("))");
		}
		constraint.append("))")
		.append("(and (= 3 (pfntype ").append(getCommand().getTagset()).append("))")
		.append("))");
		
		getContext().postAssert( constraint.toString() );
		
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.ev );
		if ( ! evs.isEmpty() )
		{
			constraint = new StringBuffer("(or false ");
			for( MemoryCommand cmd : evs )
			{
				constraint.append("(/= ").append(getCommand().getTagset())
				.append(" ").append( cmd.getTagset() ).append(")");
				//TODO надо ли тут дополнительно рассматривать случай
				// (= (pfntype ts) 1 ) и неравество v_ts и v_ts1 ?
			}
			constraint.append(")");
			
			getContext().postAssert( constraint.toString() );
		}
		
		// constraints on previous evictings from TLB
		for( MemoryCommand cmd : range.getEvictings() )
		{
			constraint = new StringBuffer("(=> (= (pfntype ")
			.append( getCommand().getTagset() ).append(") (pfntype ")
			.append( cmd.getTagset() ).append(")) (or false ")
			.append( "(and (= (pfntype ").append( getCommand().getTagset() )
			.append(") 3) (not (pfneq ").append( getCommand().getTagset() )
			.append(" " ).append( cmd.getTagset()).append(")))")
			.append( "(and (= (pfntype ").append( getCommand().getTagset() )
			.append(") 1) (value_ts ").append( getCommand().getTagset() )
			.append(" " ).append( getCommand().getValueOfTagset() ).append(")")
			.append(" (/= (getPfn ").append( getCommand().getValueOfTagset() )
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
