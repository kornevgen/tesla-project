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
		Map<Long, Set<Long>> domain = getContext().getLindexInterPFNindex( m - 1, range.m - 1 );
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
		// вводим новые переменные
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

	@Override
	public void visit1EvictingTlbHit(EvictingTlbHit range) throws Inconsistent
	{
		if ( range.getEvictings().isEmpty() )
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
		
		// evictings from L1
		for( MemoryCommand cmd: evictings )
		{
			getContext().postAssert( new StringBuffer("(/= ")
			.append( getCommand().getTagset() ).append(" ")
			.append(cmd.getTagset()).append(")").toString() );
		}
		
		StringBuffer flagsSum = new StringBuffer();
		for( MemoryCommand cmd : previousMisses )
		{
			//TODO
		}
		Map<MemoryCommand, String> hitFlags = new HashMap<MemoryCommand, String>();
		for( MemoryCommand cmd : previousHits )
		{
			String flagName = "b" + getContext().getUniqueNumber();
			getContext().postDefine(flagName, "(subrange 0 1)", "");
			flagsSum.append(" " ).append( flagName );
			hitFlags.put(cmd, flagName);
		}
		

		
		// constraints on previous evictings from TLB 
		constraint = new StringBuffer("(or false ");
		for( MemoryCommand cmd : range.getEvictings() )
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

		// usefulness constraints
		if ( allowedPfntypes.contains(0) )
		{
			constraint = new StringBuffer("(=> (= (pfntype ")
			.append( getCommand().getTagset() ).append(") 0) ");

			//TODO

			getContext().postAssert( constraint.append(")").toString() );
		}
		if ( allowedPfntypes.contains(1) )
		{
			constraint = new StringBuffer("(=> (= (pfntype ")
			.append( getCommand().getTagset() ).append(") 1) ");

			//TODO

			getContext().postAssert( constraint.append(")").toString() );
		}
		if ( allowedPfntypes.contains(2) )
		{
			constraint = new StringBuffer("(=> (= (pfntype ")
			.append( getCommand().getTagset() ).append(") 2) (and (section " )
			.append( getCommand().getTagset() ).append( " " ).append(m-1)
			.append(") ");
			
			Set<MemoryCommand> viewedHits = new HashSet<MemoryCommand>();
			for( MemoryCommand cmd : previousHits )
			{
				constraint.append("(or false ");
				// cmd : 0
				constraint.append("(and (= (pfntype ").append(cmd.getTagset()).append(") 0)")
				.append("(= ").append( hitFlags.get(cmd) ).append(" (ite (and true ");
				for ( MemoryCommand h : viewedHits )
				{
					constraint.append("(/= ").append(getCommand().getTagset()).append(" ")
					.append(h.getTagset()).append(")");
				}
				
				constraint.append("(regioneq ").append(getCommand().getTagset())
				.append(" ").append(cmd.getTagset()).append(")");
				
				constraint.append("(or false ");
				for( long l : getContext().getLinterMrange(m, w-1) )
				{
					constraint.append("(= ").append(cmd.getValueOfTagset())
					.append(" ").append(l).append(")");
				}
				constraint.append(")");
				constraint.append(") 1 0) ))");
				
				// cmd : 1
				constraint.append("(and (= (pfntype ").append(cmd.getTagset()).append(") 1)")
				.append("(= ").append( hitFlags.get(cmd) ).append(" (ite (and true ");
				for ( MemoryCommand h : viewedHits )
				{
					constraint.append("(/= ").append(getCommand().getTagset()).append(" ")
					.append(h.getTagset()).append(")");
				}
				
				constraint.append("(regioneq ").append(getCommand().getTagset())
				.append(" ").append(cmd.getTagset()).append(")");
				
				constraint.append("(or false ");
				for( Set<Long> ls : getContext().getLinterPFNminusMrange(m, w-1).values() )
				for( long l : ls )
				{
					constraint.append("(= ").append(cmd.getValueOfTagset())
					.append(" ").append(l).append(")");
				}
				constraint.append(")");
				
				constraint.append(") 1 0) ))");

				// cmd : 2
				constraint.append("(and (= (pfntype ").append(cmd.getTagset()).append(") 2)")
				.append("(= ").append( hitFlags.get(cmd) ).append(" (ite (and true ");
				for ( MemoryCommand h : viewedHits )
				{
					constraint.append("(/= ").append(getCommand().getTagset()).append(" ")
					.append(h.getTagset()).append(")");
				}
				
				constraint.append("(regioneq ").append(getCommand().getTagset())
				.append(" ").append(cmd.getTagset()).append(")");

				constraint.append("(among-section ").append( cmd.getTagset() )
				.append(" ").append( m ).append(" " ).append( w-1 ).append(")");
				
				constraint.append(") 1 0) ))");
				
				constraint.append(")");
				
				viewedHits.add(cmd);
			}

			getContext().postAssert( constraint.append("))").toString() );			
		}
		
		getContext().postAssert( new StringBuffer( "(>= (+ 0 " ).append( flagsSum ).append(") ")
				.append( w - m ).append(" )").toString() );	
	}

	@Override
	public void visit1InitialTlbHit(InitialTlbHit range) throws Inconsistent {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visit1InitialTlbMiss(InitialTlbMiss range) throws Inconsistent {
		// TODO Auto-generated method stub
		
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
