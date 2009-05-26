package ru.teslaprj.ranges;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import ru.teslaprj.Cache;
import ru.teslaprj.TLB;
import ru.teslaprj.ranges.tsiterators.CommonIterator;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.Scheme;

public class RangeIterator implements Iterator<Ranges>
{
	CommonIterator[] l1Iterators;
	CommonIterator[] tlbIterators;
	Range[] l1values;
	Range[] tlbvalues;
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
				CommonIterator c = l1Iterators[i];
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
				CommonIterator c = tlbIterators[i];
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
	
	

	public RangeIterator( //TLB tlb, List<Cache> cacheState, 
			Scheme scheme  )
	{
		List<CommonIterator> l1its = new ArrayList<CommonIterator>();
		List<CommonIterator> tlbits = new ArrayList<CommonIterator>();
		
		for( Command cmd : scheme.getCommands() )
		{
			if ( cmd.hasCacheSituation() )
			{
				l1its.add( cmd.getCacheSituation(1).iterator() );
			}
			
			if ( cmd.hasTLBSituation() )
			{
				tlbits.add( cmd.getTLBSituation().iterator() );
			}
		}
		
		l1Iterators = l1its.toArray(new CommonIterator[]{});
		tlbIterators = tlbits.toArray(new CommonIterator[]{});
		
		l1values = new Range[l1Iterators.length];
		for( int i = 0; i < l1Iterators.length; i++ )
		{
			l1values[i] = l1Iterators[i].next();
		}
		
		tlbvalues = new Range[tlbIterators.length];
		for( int i = 0; i < tlbIterators.length; i++ )
		{
			tlbvalues[i] = tlbIterators[i].next();
		}
	}


}
