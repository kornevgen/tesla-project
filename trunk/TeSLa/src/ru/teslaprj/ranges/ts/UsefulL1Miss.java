package ru.teslaprj.ranges.ts;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class UsefulL1Miss extends L1Range
{
	final int m;
	final int w;
	final Set<MemoryCommand> evictings;
	final Set<MemoryCommand> previousMisses;
	final List<MemoryCommand> previousHits;

	public UsefulL1Miss(
			MemoryCommand cmd,
			int m, int w,
			Set<MemoryCommand> allPreviousMisses,
			Set<MemoryCommand> previousMisses,
			List<MemoryCommand> previousHits)
	{
		super( cmd );
		this.m = m;
		this.evictings = allPreviousMisses;
		this.previousMisses = previousMisses;
		this.previousHits = previousHits;
		this.w = w;
	}

	@Override
	public String print() {
		return "L1Miss( " + getCommand().getTagset() + " ) useful with m=" + m;
	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range) throws Inconsistent
	{
		Map<Long, Set<Long>> domain = getContext().getLindexInterPFN( m - 1 );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		/// A. ts \notin evictings
		for( MemoryCommand cmd : evictings )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( getCommand().getTagset() ).append(" " )
				.append( cmd.getTagset() ).append(")").toString() );
		}
		
		/// B. usefulness constraints: defines, ts value, sum constraint
		// introduce new flags for usefulnesses
		StringBuffer flagsSum = new StringBuffer();
		for( MemoryCommand cmd : previousMisses )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			// `condition' is R(y) = R(x)
			String condition = new StringBuffer("(= (getRegion1 ")
				.append( getCommand().getTagset() ).append(") (getRegion1 " )
				.append( cmd.getTagset() ).append("))").toString();
			getContext().postDefine(flagName, "(subrange 0 1)", "(ite " + condition + " 1 0)");
			flagsSum.append(" " ).append( flagName );
		}
		Map<MemoryCommand, String> hitFlags = new HashMap<MemoryCommand, String>();
		for( MemoryCommand cmd : previousHits )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			getContext().postDefine(flagName, "(subrange 0 1)", "");
			flagsSum.append(" " ).append( flagName );
			hitFlags.put(cmd, flagName);
		}
		
		//disjunction about u_xi
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : domain.keySet() )
		{
			constraint.append("(and (= ").append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" ").append(l).append("))");
			
			Set<MemoryCommand> viewedHits = new HashSet<MemoryCommand>();
			for( MemoryCommand cmd : previousHits )
			{
				constraint.append("(= ").append( hitFlags.get(cmd) ).append(" (ite ");
				
				if ( viewedHits.isEmpty() )
				{
					constraint.append("(or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
				}
				else
				{
					constraint.append("(and (or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
					for( MemoryCommand c : viewedHits )
					{
						constraint.append("(/= ").append( getCommand().getTagset() )
							.append( " " ).append( c.getTagset() ).append(")");
					}
					constraint.append(")");
				}
				
				constraint.append(" 1 0))");
				viewedHits.add(cmd);
			}
		}
		
		getContext().postAssert( constraint.append(")").toString() );
		
		getContext().postAssert( new StringBuffer( "(>= (+ 0 " ).append( flagsSum ).append(") ")
				.append( w - m ).append(" )").toString() );
		
		///C. ^ts^ \in evictings tagsets
		constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : range.evics )
		{
			constraint.append("(= (getPfn " ).append( getCommand().getTagset() )
			.append(") (getPfn " ).append( cmd.getTagset() ).append("))");
		}
		getContext().postAssert( constraint.append(")").toString() );
	}

	@Override
	public void visitInitialTlbHit(InitialTlbHit range) throws Inconsistent
	{
		Map<Long, Set<Long>> domain = getContext().getLindexInterM( m - 1 );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		/// A. ts \notin evictings
		for( MemoryCommand cmd : evictings )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( getCommand().getTagset() ).append(" " )
				.append( cmd.getTagset() ).append(")").toString() );
		}
		
		/// B. usefulness constraints: defines, ts value, sum constraint
		// introduce new flags for usefulnesses
		StringBuffer flagsSum = new StringBuffer();
		for( MemoryCommand cmd : previousMisses )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			// `condition' is R(y) = R(x)
			String condition = new StringBuffer("(= (getRegion1 ")
				.append( getCommand().getTagset() ).append(") (getRegion1 " )
				.append( cmd.getTagset() ).append("))").toString();
			getContext().postDefine(flagName, "(subrange 0 1)", "(ite " + condition + " 1 0)");
			flagsSum.append(" " ).append( flagName );
		}
		Map<MemoryCommand, String> hitFlags = new HashMap<MemoryCommand, String>();
		for( MemoryCommand cmd : previousHits )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			getContext().postDefine(flagName, "(subrange 0 1)", "");
			flagsSum.append(" " ).append( flagName );
			hitFlags.put(cmd, flagName);
		}
		
		//disjunction about u_xi
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : domain.keySet() )
		{
			constraint.append("(and (= ").append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" ").append(l).append("))");
			
			Set<MemoryCommand> viewedHits = new HashSet<MemoryCommand>();
			for( MemoryCommand cmd : previousHits )
			{
				constraint.append("(= ").append( hitFlags.get(cmd) ).append(" (ite ");
				
				if ( viewedHits.isEmpty() )
				{
					constraint.append("(or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
				}
				else
				{
					constraint.append("(and (or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
					for( MemoryCommand c : viewedHits )
					{
						constraint.append("(/= ").append( getCommand().getTagset() )
							.append( " " ).append( c.getTagset() ).append(")");
					}
					constraint.append(")");
				}
				
				constraint.append(" 1 0))");
				viewedHits.add(cmd);
			}
		}
		
		getContext().postAssert( constraint.append(")").toString() );
		
		getContext().postAssert( new StringBuffer( "(>= (+ 0 " ).append( flagsSum ).append(") ")
				.append( w - m ).append(" )").toString() );
	}

	@Override
	public void visitInitialTlbMiss(InitialTlbMiss range) throws Inconsistent
	{
		Map<Long, Set<Long>> domain = getContext().getLindexInterPFNminusM( m - 1 );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		/// A. ts \notin evictings
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.getEvictings() );
		for( MemoryCommand cmd : evs )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( getCommand().getTagset() ).append(" " )
				.append( cmd.getTagset() ).append(")").toString() );
		}
		
		/// B. usefulness constraints: defines, ts value, sum constraint
		// introduce new flags for usefulnesses
		StringBuffer flagsSum = new StringBuffer();
		for( MemoryCommand cmd : previousMisses )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			// `condition' is R(y) = R(x)
			String condition = new StringBuffer("(= (getRegion1 ")
				.append( getCommand().getTagset() ).append(") (getRegion1 " )
				.append( cmd.getTagset() ).append("))").toString();
			getContext().postDefine(flagName, "(subrange 0 1)", "(ite " + condition + " 1 0)");
			flagsSum.append(" " ).append( flagName );
		}
		Map<MemoryCommand, String> hitFlags = new HashMap<MemoryCommand, String>();
		for( MemoryCommand cmd : previousHits )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			getContext().postDefine(flagName, "(subrange 0 1)", "");
			flagsSum.append(" " ).append( flagName );
			hitFlags.put(cmd, flagName);
		}
		
		//disjunction about u_xi
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : domain.keySet() )
		{
			constraint.append("(and (= ").append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" ").append(l).append("))");
			
			Set<MemoryCommand> viewedHits = new HashSet<MemoryCommand>();
			for( MemoryCommand cmd : previousHits )
			{
				constraint.append("(= ").append( hitFlags.get(cmd) ).append(" (ite ");
				
				if ( viewedHits.isEmpty() )
				{
					constraint.append("(or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
				}
				else
				{
					constraint.append("(and (or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
					for( MemoryCommand c : viewedHits )
					{
						constraint.append("(/= ").append( getCommand().getTagset() )
							.append( " " ).append( c.getTagset() ).append(")");
					}
					constraint.append(")");
				}
				
				constraint.append(" 1 0))");
				viewedHits.add(cmd);
			}
		}
		
		getContext().postAssert( constraint.append(")").toString() );
		
		getContext().postAssert( new StringBuffer( "(>= (+ 0 " ).append( flagsSum ).append(") ")
				.append( w - m ).append(" )").toString() );
		
		///C. ^ts^ \notin evictings tagsets
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer("(/= (getPfn " )
				.append( getCommand().getTagset() ).append(") (getPfn " )
				.append( cmd.getTagset() ).append("))").toString() );
		}
	}

	@Override
	public void visitUnusefulTlbMiss(UnusefulTlbMiss range) throws Inconsistent
	{
		Map<Long, Set<Long>> domain = getContext().getLindexInterMrange( m - 1, range.minM - 1, range.maxM - 1 );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		/// A. ts \notin evictings
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.getEvictings() );
		for( MemoryCommand cmd : evs )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( getCommand().getTagset() ).append(" " )
				.append( cmd.getTagset() ).append(")").toString() );
		}
		
		/// B. usefulness constraints: defines, ts value, sum constraint
		// introduce new flags for usefulnesses
		StringBuffer flagsSum = new StringBuffer();
		for( MemoryCommand cmd : previousMisses )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			// `condition' is R(y) = R(x)
			String condition = new StringBuffer("(= (getRegion1 ")
				.append( getCommand().getTagset() ).append(") (getRegion1 " )
				.append( cmd.getTagset() ).append("))").toString();
			getContext().postDefine(flagName, "(subrange 0 1)", "(ite " + condition + " 1 0)");
			flagsSum.append(" " ).append( flagName );
		}
		Map<MemoryCommand, String> hitFlags = new HashMap<MemoryCommand, String>();
		for( MemoryCommand cmd : previousHits )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			getContext().postDefine(flagName, "(subrange 0 1)", "");
			flagsSum.append(" " ).append( flagName );
			hitFlags.put(cmd, flagName);
		}
		
		//disjunction about u_xi
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : domain.keySet() )
		{
			constraint.append("(and (= ").append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" ").append(l).append("))");
			
			Set<MemoryCommand> viewedHits = new HashSet<MemoryCommand>();
			for( MemoryCommand cmd : previousHits )
			{
				constraint.append("(= ").append( hitFlags.get(cmd) ).append(" (ite ");
				
				if ( viewedHits.isEmpty() )
				{
					constraint.append("(or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
				}
				else
				{
					constraint.append("(and (or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
					for( MemoryCommand c : viewedHits )
					{
						constraint.append("(/= ").append( getCommand().getTagset() )
							.append( " " ).append( c.getTagset() ).append(")");
					}
					constraint.append(")");
				}
				
				constraint.append(" 1 0))");
				viewedHits.add(cmd);
			}
		}
		
		getContext().postAssert( constraint.append(")").toString() );
		
		getContext().postAssert( new StringBuffer( "(>= (+ 0 " ).append( flagsSum ).append(") ")
				.append( w - m ).append(" )").toString() );
		
		///C. ^ts^ \notin evictings tagsets
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer("(/= (getPfn " )
				.append( getCommand().getTagset() ).append(") (getPfn " )
				.append( cmd.getTagset() ).append("))").toString() );
		}
	}

	@Override
	public void visitUsefulTlbMiss(UsefulTlbMiss range) throws Inconsistent
	{
		Map<Long, Set<Long>> domain = getContext().getLindexInterMindex( m - 1, range.m - 1 );
		if ( domain.isEmpty() )
		{
			throw new Inconsistent();
		}
		
		/// A. ts \notin evictings
		Set<MemoryCommand> evs = new HashSet<MemoryCommand>( evictings );
		evs.removeAll( range.getEvictings() );
		for( MemoryCommand cmd : evs )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( getCommand().getTagset() ).append(" " )
				.append( cmd.getTagset() ).append(")").toString() );
		}
		
		/// B. usefulness constraints: defines, ts value, sum constraint
		// introduce new flags for usefulnesses
		StringBuffer flagsSum = new StringBuffer();
		for( MemoryCommand cmd : previousMisses )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			// `condition' is R(y) = R(x)
			String condition = new StringBuffer("(= (getRegion1 ")
				.append( getCommand().getTagset() ).append(") (getRegion1 " )
				.append( cmd.getTagset() ).append("))").toString();
			getContext().postDefine(flagName, "(subrange 0 1)", "(ite " + condition + " 1 0)");
			flagsSum.append(" " ).append( flagName );
		}
		Map<MemoryCommand, String> hitFlags = new HashMap<MemoryCommand, String>();
		for( MemoryCommand cmd : previousHits )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			getContext().postDefine(flagName, "(subrange 0 1)", "");
			flagsSum.append(" " ).append( flagName );
			hitFlags.put(cmd, flagName);
		}
		
		//disjunction about u_xi
		StringBuffer constraint = new StringBuffer("(or false ");
		for( long l : domain.keySet() )
		{
			constraint.append("(and (= ").append( getCommand().getTagset() )
				.append(" (mk-bv " ).append( getContext().getTagsetLength() )
				.append(" ").append(l).append("))");
			
			Set<MemoryCommand> viewedHits = new HashSet<MemoryCommand>();
			for( MemoryCommand cmd : previousHits )
			{
				constraint.append("(= ").append( hitFlags.get(cmd) ).append(" (ite ");
				
				if ( viewedHits.isEmpty() )
				{
					constraint.append("(or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
				}
				else
				{
					constraint.append("(and (or false ");
					for( long lm : domain.get(l) )
					{
						constraint.append("(= ").append( getCommand().getTagset() )
						.append(" ").append(lm).append(")");
					}
					constraint.append(")");
					for( MemoryCommand c : viewedHits )
					{
						constraint.append("(/= ").append( getCommand().getTagset() )
							.append( " " ).append( c.getTagset() ).append(")");
					}
					constraint.append(")");
				}
				
				constraint.append(" 1 0))");
				viewedHits.add(cmd);
			}
		}
		
		getContext().postAssert( constraint.append(")").toString() );
		
		getContext().postAssert( new StringBuffer( "(>= (+ 0 " ).append( flagsSum ).append(") ")
				.append( w - m ).append(" )").toString() );
		
		///C. ^ts^ \notin evictings tagsets
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer("(/= (getPfn " )
				.append( getCommand().getTagset() ).append(") (getPfn " )
				.append( cmd.getTagset() ).append("))").toString() );
		}
		
		///D. usefulness constraints for TLB
		// ������ ����� ����������
		//(define b_i::(subrange 0 1) (ite ( condition ) 1 0))
		//(assert (>= w-m-k (+ b_i)))
		Set<String> viewedHitTagsets = new HashSet<String>();
		flagsSum = new StringBuffer();
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

	}

}