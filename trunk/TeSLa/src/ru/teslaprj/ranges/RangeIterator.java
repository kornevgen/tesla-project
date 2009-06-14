package ru.teslaprj.ranges;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.TLB;
import ru.teslaprj.Cache.CACHETYPE;
import ru.teslaprj.ranges.tsiterators.L1Iterator;
import ru.teslaprj.ranges.tsiterators.TLBIterator;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;
import ru.teslaprj.scheme.ts.TLBSituation;

public class RangeIterator implements Iterator<Ranges>
{
	L1Iterator[] l1Iterators;
	TLBIterator[] tlbIterators;
	L1Range[] l1values;
	TLBRange[] tlbvalues;
	boolean hasNext = true;
	
	final TLB tlb;
	final List<Cache> caches;
	
	@Override
	public boolean hasNext() {
		return hasNext;
	}

	@Override
	public Ranges next()
	{
		try
		{
			return new Ranges( l1values, tlbvalues, getDataL1( caches ),tlb );
		}
		finally
		{
			step:
			{
			for( int i = 0; i < l1Iterators.length; i++ )
			{
				L1Iterator c = l1Iterators[i];
				if ( c.hasNext() )
				{
					l1values[i] = c.next();
					break step;
				}
				else
				{
					l1Iterators[i] = c.getTestSituation().iterator();
					l1values[i] = l1Iterators[i].next();
				}
			}
			for( int i = 0; i < tlbIterators.length; i++ )
			{
				TLBIterator c = tlbIterators[i];
				if ( c.hasNext() )
				{
					tlbvalues[i] = c.next();
					break step;
				}
				else
				{
					tlbIterators[i] = c.getTestSituation().iterator();
					tlbvalues[i] = tlbIterators[i].next();
				}
			}
			hasNext = false;
			}
		}
	}

	private static Cache getDataL1(List<Cache> caches)
	{
		for( Cache c : caches )
		{
			if ( c.getLevel() == 1  && c.getType() == CACHETYPE.DATA )
				return c;
		}
		throw new Error("Data-L1 is not found");
	}

	@Override
	public void remove()
	{
	}
	

	public RangeIterator(
			TLB tlb, 
			List<Cache> cacheState, 
			Scheme scheme  )
	{
		List<L1Iterator> l1its = new ArrayList<L1Iterator>();
		List<TLBIterator> tlbits = new ArrayList<TLBIterator>();
		
		Set<ProcedureTestSituation> viewedTestSituations = new  HashSet<ProcedureTestSituation>();
		
		for( Command cmd : scheme.getCommands() )
		{
			if ( cmd instanceof MemoryCommand )
			{
				MemoryCommand c = (MemoryCommand)cmd;
		
				for( Cache cache : cacheState )
				{
					if ( c.hasCacheSituation(cache) )
					{
						CacheTestSituation ts = c.getCacheSituation(cache);
						if ( viewedTestSituations.contains(ts) )
							throw new IllegalArgumentException("test situation objects must be unique");
						viewedTestSituations.add(ts);
						l1its.add( ts.iterator() );
					}
				}
				
				if ( c.hasTLBSituation() )
				{
					TLBSituation ts = c.getTLBSituation();
					if ( viewedTestSituations.contains(ts) )
						throw new IllegalArgumentException("test situation objects must be unique");
					viewedTestSituations.add(ts);
					tlbits.add( c.getTLBSituation().iterator() );
				}
			}
		}
		
		l1Iterators = l1its.toArray(new L1Iterator[]{});
		tlbIterators = tlbits.toArray(new TLBIterator[]{});
		
		l1values = new L1Range[l1Iterators.length];
		for( int i = 0; i < l1Iterators.length; i++ )
		{
			l1values[i] = l1Iterators[i].next();
		}
		
		tlbvalues = new TLBRange[tlbIterators.length];
		for( int i = 0; i < tlbIterators.length; i++ )
		{
			tlbvalues[i] = tlbIterators[i].next();
		}
		
		this.tlb = tlb;
		this.caches = cacheState;
	}


}
