package ru.teslaprj.tagsets;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * общий вид домена переменной x:
 *    (c:tagsetConstants) x = c
 * \/ ((c1+>c2):pfnConstants)(cc1:c1,cc2:c2) x[..] = cc1 /\ x ~= cc2
 * 
 * домен "находится в нормальной форме", если
 * 1) нет лишних tagsetConstants (которые противоречат forbiddenConstants
 * 		и которые вложены в pfnConstants)
 * и 2) ключи pfnConstants не пересекаются
 * 
 * процедура нормализации: (т.к. это map, то невозможны одинаковые ключи)
 * 1) разбиение ключей на непересекающиеся области: (если есть общая часть, выделяем отдельно)
 * {a,b}->{x}
 * {a,c}->{y}
 * ==>
 * {a}->{x,y}
 * {b}->{x}
 * {c}->{y}
 * 2) сбор ключей с одинаковыми значениями: (непересекаемость не портится, если делать линейно)
 * {a,b,c}->{x,y}
 * {r,t}->{x,y}
 * ===>
 * {a,b,c,r,t}->{x,y}
 * 3) удаление лишних констант:
 * {A,B}
 * {a}->{x}
 * A[...]=a /\ A ~= x
 * ==>
 * {B}
 * {a}->{x}
 * 4) удаление пустых pfnConstaints:
 * {}->{x}
 * ==>
 * empty
 */
public class Domain
{
	public static int TagsetLengthMinusPfnLength;
	
	Set<Long> tagsetConstants;
	Map<Set<Long>, Set<Long>> pfnConstants;
	
	public Domain(Set<Long> constants)
	{
		tagsetConstants = constants;
		pfnConstants = new HashMap<Set<Long>, Set<Long>>();
	}

	public Domain(Set<Long> microPfns, Set<Long> hashSet)
	{
		tagsetConstants = new HashSet<Long>();
		pfnConstants = new HashMap<Set<Long>, Set<Long>>();
		pfnConstants.put( microPfns, hashSet );
	}

	public Domain()
	{
		this( new HashSet<Long>() );
	}

	/**
	 * get all possible values for highest bits (pfn bits)
	 */
	public Set<Long> getPfns()
	{
		// union pfns from pfnConstants with highest bits of tagsetConstants
		Set<Long> result = new HashSet<Long>();
		for( Set<Long> pfns : pfnConstants.keySet() )
		{
			result.addAll( pfns );
		}
		for( Long ts : tagsetConstants )
		{
			result.add( ts / (int)Math.pow(2, TagsetLengthMinusPfnLength ) );
		}
		return result;
	}
		
	/**
	 * this \\union d
	 */
	public Domain getUnionWith( final Domain d )
	{
		Set<Long> c = new HashSet<Long>( tagsetConstants );
		c.addAll( d.tagsetConstants );
		Map< Set<Long>, Set<Long> > p = new HashMap<Set<Long>, Set<Long>>( pfnConstants );
		p.putAll( d.pfnConstants );
		Domain result = new Domain( c );
		result.pfnConstants = p;
		result.normalize();
		return result;
	}
	
	/**
	 * this \inter d
	 * @param d
	 * @return
	 */
	public Domain getIntersectWith( final Domain d )
	{
		Set<Long> constants = new HashSet<Long>();
		for( Long c1 : tagsetConstants )
		{
			if ( d.tagsetConstants.contains(c1) )
				constants.add(c1);
			else
			{
				Long c1_pfn = c1 / (int)Math.pow(2, TagsetLengthMinusPfnLength);
				for( Set<Long> pfns : d.pfnConstants.keySet() )
				{
					if ( pfns.contains(c1_pfn) && ! d.pfnConstants.get(pfns).contains(c1) )
					{
						constants.add(c1);
						break;
					}
				}
			}
		}
		for( Long c1 : d.tagsetConstants )
		{
			Long c1_pfn = c1 / (int)Math.pow(2, TagsetLengthMinusPfnLength);
			for( Set<Long> pfns : pfnConstants.keySet() )
			{
				if ( pfns.contains(c1_pfn) && ! pfnConstants.get(pfns).contains(c1) )
				{
					constants.add(c1);
					break;
				}
			}
		}
		
		Map< Set<Long>, Set<Long> > pfn = new HashMap<Set<Long>, Set<Long>>();
		for( Set<Long> pfns1 : pfnConstants.keySet() )
		{
			for( Set<Long> pfns2 : d.pfnConstants.keySet() )
			{
				Set<Long> p = new HashSet<Long>(pfns1);
				pfns1.retainAll( pfns2 );
				Set<Long> f = new HashSet<Long>( pfnConstants.get(pfns1) );
				f.addAll( pfnConstants.get(pfns2) );
				pfn.put( p, f );
			}
		}
		
		Domain result = new Domain( constants );
		result.pfnConstants = pfn;
		result.normalize();
		return result;
	}
	
	public boolean isEmpty()
	{
		if ( ! tagsetConstants.isEmpty() )
			return false;
		
		for( Set<Long> pfns : pfnConstants.keySet() )
		{
			if ( ! pfns.isEmpty() )
				return false;
		}
		return true;
	}

	public Set<Long> getTagsetConstants()
	{
		return tagsetConstants;
	}
	
	/**
	 * процедура нормализации
	 */
	protected void normalize()
	{
		final class PairFinder
		{
			Set<Long> first;
			Set<Long> second;
			
			boolean hasIntersection( Collection< Set<Long> > sets )
			{
				for( Set<Long> s1 : sets )
				{
					for( Set<Long> s2 : sets )
					{
						if ( s1 == s2 )
							continue;
						Set<Long> ss1 = new HashSet<Long>( s1 );
						ss1.retainAll(s2);
						if ( ! ss1.isEmpty() )
						{
							first = s1;
							second = s2;
							return true;
						}
					}
				}
				return false;
			}
		}
		/**
		 * 1) разбиение ключей на непересекающиеся области: (если есть общая часть, выделяем отдельно)
		 * {a,b}->{x}
		 * {a,c}->{y}
		 * ==>
		 * {a}->{x,y}
		 * {b}->{x}
		 * {c}->{y}
		 */
		PairFinder pf = new PairFinder();
		while( pf.hasIntersection( pfnConstants.values() ) )
		{
			Set<Long> f1 = pfnConstants.get( pf.first );
			Set<Long> f2 = pfnConstants.get( pf.second );
			//looking for pf.first \ pf.second, pf.first \inter pf.second and pf.second \ pf.first
			Set<Long> fMs = new HashSet<Long>(pf.first);
			fMs.removeAll(pf.second);
			Set<Long> fIs = new HashSet<Long>(pf.first);
			fIs.retainAll(pf.second);
			Set<Long> sMf = new HashSet<Long>( pf.second );
			sMf.removeAll(pf.first);
			
			Set<Long> fIs_value = new HashSet<Long>( f1 );
			fIs_value.addAll(f2);
			if ( pfnConstants.containsKey(fIs) )
			{
				fIs_value.addAll( pfnConstants.get(fIs) );
			}
			
			Set<Long> fMs_value = f1;
			if ( pfnConstants.containsKey(fMs) )
			{
				fMs_value = new HashSet<Long>( f1 );
				fMs_value.addAll( pfnConstants.get(fMs) );
			}
			
			Set<Long> sMf_value = f2;
			if ( pfnConstants.containsKey(sMf) )
			{
				sMf_value = new HashSet<Long>( f2 );
				sMf_value.addAll( pfnConstants.get(sMf) );
			}
			
			pfnConstants.remove(pf.first);
			pfnConstants.remove(pf.second);
			pfnConstants.put(fMs, fMs_value);
			pfnConstants.put(sMf, sMf_value);
			pfnConstants.put(fIs, fIs_value);
		}
		
		/**
		 * 2) сбор ключей с одинаковыми значениями: (непересекаемость не портится, если делать линейно)
		 * {a,b,c}->{x,y}
		 * {r,t}->{x,y}
		 * ===>
		 * {a,b,c,r,t}->{x,y}
		 */
		Map< Set<Long>, Set<Long> > newPfns = new HashMap<Set<Long>, Set<Long>>();
		nextp: for( Set<Long> p : pfnConstants.keySet() )
		{
			Set<Long> fc = pfnConstants.get(p);
			
			//looking for element with the same value
			for( Set<Long> ps : newPfns.keySet() )
			{
				if ( newPfns.get(ps).equals(fc) )
				{
					ps.addAll( p );
					continue nextp;
				}
			}
			newPfns.put(p, fc);
		}
		pfnConstants = newPfns;
		
		/**
		 * 3) удаление лишних констант:
		 * {A,B}
		 * {a}->{x}
		 * A[...]=a /\ A ~= x
		 * ==>
		 * {B}
		 * {a}->{x}
		 */
		Set<Long> ts = new HashSet<Long>();
		tagsets: for( Long tagset : tagsetConstants )
		{
			Long pfn = tagset / (int)Math.pow( 2, TagsetLengthMinusPfnLength );
			for( Set<Long> pfns : pfnConstants.keySet() )
			{
				if ( pfns.contains( pfn ) && ! pfnConstants.get(pfns).contains(tagset) )
					continue tagsets;
			}
			ts.add( tagset );
		}
		tagsetConstants = ts;
		
		/**
		 * 4) удаление пустых pfnConstaints:
		 * {}->{x}
		 * ==>
		 * empty
		 */
		Map< Set<Long>, Set<Long> > p = new HashMap<Set<Long>, Set<Long>>();
		for( Set<Long> pfns : pfnConstants.keySet() )
		{
			if ( ! pfns.isEmpty() )
			{
				p.put( pfns, pfnConstants.get( pfns ) );
			}
		}
		pfnConstants = p;
	}
}
