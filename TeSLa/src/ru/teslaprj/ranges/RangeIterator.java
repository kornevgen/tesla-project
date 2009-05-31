package ru.teslaprj.ranges;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import ru.teslaprj.Cache;
import ru.teslaprj.ranges.tsiterators.L1Iterator;
import ru.teslaprj.ranges.tsiterators.TLBIterator;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.Scheme;

public class RangeIterator implements Iterator<Ranges>
{
	L1Iterator[] l1Iterators;
	TLBIterator[] tlbIterators;
	L1Range[] l1values;
	TLBRange[] tlbvalues;
	boolean hasNext = true;
	
	@Override
	public boolean hasNext() {
		return hasNext;
	}

	@Override
	public Ranges next()
	{
		try
		{
			return new Ranges( l1values, tlbvalues );
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

	@Override
	public void remove() {
	}
	
	

	public RangeIterator( //TLB tlb, 
			List<Cache> cacheState, 
			Scheme scheme  )
	{
		List<L1Iterator> l1its = new ArrayList<L1Iterator>();
		List<TLBIterator> tlbits = new ArrayList<TLBIterator>();
		
		for( Command cmd : scheme.getCommands() )
		{
			if ( cmd instanceof MemoryCommand )
			{
				MemoryCommand c = (MemoryCommand)cmd;
		
				for( Cache cache : cacheState )
				{
					if ( c.hasCacheSituation(cache) )
					{
						l1its.add( c.getCacheSituation(cache).iterator() );
					}
				}
				
				if ( c.hasTLBSituation() )
				{
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
	}


}
