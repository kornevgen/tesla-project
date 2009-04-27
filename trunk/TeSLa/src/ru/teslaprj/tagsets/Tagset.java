package ru.teslaprj.tagsets;

import java.util.List;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.TLB;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.TLBHit;
import ru.teslaprj.scheme.ts.TLBMiss;
import ru.teslaprj.scheme.ts.TLBSituation;

public class Tagset
{
	DomainConstraint constraint;
	
	public Tagset( 
			TLBSituation tlbSituation,
			CacheTestSituation l1Situation,
			CacheTestSituation l2Situation,
			List<Tagset> previousTagsets,
			TLB tlb,
			Cache l1,
			Cache l2
		)
		throws EmptyDomain
	{
		/**
		 * на основе previousTagsets составляем ограничение,
		 * вычисляем область значений на основе ограничения и tlb, l1, l2
		 * если область значений пустая, генерируем исключение
		 * промахи добавляют свои new Tagset - на них тоже ограничение и область значений
		 */
		
		if ( tlbSituation instanceof TLBHit )
		{
			if ( l1Situation instanceof CacheHit )
			{
				constraint = new TlbHitL1Hit(previousTagsets, tlb, l1);
			}
			else if ( l2Situation instanceof CacheHit ) // l1Situation instanceof CacheMiss
			{// не забыть про создание вытесняемых тегсетов :)
				constraint = new TlbHitL1MissL2Hit(previousTagsets, tlb, l1, l2);
			}
			else // l2Situation instanceof CacheMiss, l1Situation instanceof CacheMiss
			{
				constraint = new TlbHitL1MissL2Miss(previousTagsets, tlb, l1, l2);
			}
		}
		else if ( tlbSituation instanceof TLBMiss )
		{
			if ( l1Situation instanceof CacheHit )
			{
				constraint = new TlbMissL1Hit(previousTagsets, tlb, l1);
			}
			else if ( l2Situation instanceof CacheHit ) // l1Situation instanceof CacheMiss
			{
				constraint = new TlbMissL1MissL2Hit(previousTagsets, tlb, l1, l2);
			}
			else // l2Situation instanceof CacheMiss, l1Situation instanceof CacheMiss
			{
				constraint = new TlbMissL1MissL2Miss(previousTagsets, tlb, l1, l2);
			}
		}
		
		if ( constraint.getDomain().isEmpty() )
			throw new EmptyDomain();
	}

	static Set<Tagset> getCacheEvictedTagsets( List<Tagset> tagsets )
	{
		
	}

	static Set<Tagset> getCacheEvictingTagsets( List<Tagset> tagsets )
	{
		
	}

	static Set<Pfn> getMicroTLBEvictedPfns( List<Tagset> tagsets )
	{
		
	}
	
	static Set<Tagset> getMicroTLBEvictingTagsets( List<Tagset> tagsets )
	{
		
	}
	
	static Set<Long> getMinterL1( TLB tlb, Cache L1 )
	{
		
	}
}
