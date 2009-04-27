package ru.teslaprj.tagsets;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.TLB;

/**
 * ограничение TlbHitL1Hit(ts) выглядит следующим образом:
 * ts \notin forbidden /\
 * ts[..] \notin forbiddenPfns /\
 * (    ts \in constants 
 * 	 \/	(x:allowed.keys :- allowed(x) isn't empty) ts = x /\ ts[..] \in allowed(x)
 *   \/ (x:equalityPfn.keys :- equalityPfn(x) isn't empty) ts[..] = x[..] /\ ts \in equalityPfn(x)
 *   \/ (x:equalityPfn.keys, y:allowed.keys) ts = x /\ ts[..] = y[..] 
 * )
 */
public class TlbHitL1Hit implements DomainConstraint
{
	private Domain domain;
	
	private Set<Tagset> forbidden;
	private Set<Pfn> forbiddenPfns;
	private Set<Long> constants;
	private Map<Tagset, Set<Long>> equalityPfn;
	private Map<Tagset, Set<Long>> allowed;
	
	public TlbHitL1Hit(List<Tagset> previousTagsets, TLB tlb, Cache l1)
	{
		forbidden = Tagset.getCacheEvictedTagsets(previousTagsets);
		forbiddenPfns = Tagset.getMicroTLBEvictedPfns(previousTagsets);
		constants = Tagset.getMinterL1(tlb, l1);
		
		//allowed = {x +> M \inter values(x[..])}
		Set<Tagset> evicting = Tagset.getCacheEvictingTagsets(previousTagsets);
		allowed = new HashMap<Tagset, Set<Long>>();
		for( Tagset ts : evicting )
		{
			Domain dts = ts.constraint.getDomain();
			allowed.put( ts, dts.intersectWithPfns(tlb.getMicroPfns()) );
		}
		
		//equalityPfn = {x +> L1 \inter values(x[..])}
		Set<Tagset> tlbEvicting = Tagset.getMicroTLBEvictingTagsets(previousTagsets);
		equalityPfn = new HashMap<Tagset, Set<Long>>();
		for( Tagset ts : tlbEvicting )
		{
			Domain dts = ts.constraint.getDomain();
			equalityPfn.put( ts, dts.getPfnDomain().intersectWithTagsets(l1.getTagsets()));
		}
		
		//calculate this.domain
		Domain micro = new Domain( tlb.getMicroPfns(), new HashSet<Long>() );
		// [M] \inter L1
		Set<Long> cs = new HashSet<Long>( constants );
		// L1 \inter C^
		for( Set<Long> tss : equalityPfn.values() )
		{
			cs.addAll(tss);
		}
		Domain c = new Domain( cs );
		// [M] \inter A
		for( Tagset ts : evicting )
		{
			Domain dts = ts.constraint.getDomain();
			c = c.getUnionWith( dts.getIntersectWith(micro) );
		}
		// C^ \inter A
		Domain d1 = new Domain();
		for( Tagset ts : evicting )
		{
			d1 = d1.getUnionWith(ts.constraint.getDomain());
		}
		Domain d2 = new Domain();
		for( Tagset ts : tlbEvicting )
		{
			d2 = d2.getUnionWith( ts.constraint.getDomain().getPfnDomain() );
		}
		
		domain = c.getUnionWith( d1.getIntersectWith(d2) );
	}

	@Override
	public Domain getDomain() {
		// TODO Auto-generated method stub
		return null;
	}

}
