package ru.teslaprj.tagsets;

import java.util.HashSet;
import java.util.Set;

/**
 * общий вид домена переменной x:
 *    (c:tagsetConstants) x = c
 * \/ (c1:pfnConstants, c2: forbiddenConstants) x[..] = c1 /\ x ~= c2
 * 
 * домен находится в канонической форме, если
 * 1) нет лишних tagsetConstants (которые противоречат forbiddenConstants
 * 		и которые вложены в pfnConstants)
 * и 2) среди forbiddenConstants нет лишних (старшие биты не лежат в pfnConstants)
 */
public class Domain
{
	Set<Long> tagsetConstants;
	Set<Long> pfnConstants;
	Set<Long> forbiddenConstants;
	
	public Domain(Set<Long> constants)
	{
		tagsetConstants = constants;
		pfnConstants = new HashSet<Long>();
		forbiddenConstants = new HashSet<Long>();
	}

	public Domain(Set<Long> microPfns, Set<Long> hashSet)
	{
		tagsetConstants = new HashSet<Long>();
		pfnConstants = microPfns;
		forbiddenConstants = hashSet;
	}

	public Domain()
	{
		this( new HashSet<Long>() );
	}

	/**
	 * returns set of const pfns
	 */
	public Set<Long> intersectWithPfns( Set<Long> pfns )
	{
		
	}
	
	/**
	 * returns set of const tagsets
	 */
	public Set<Long> intersectWithTagsets( Set<Long> tagsets )
	{
		
	}
	
	/**
	 * if already pfn domain then nothing to do, otherwise get domain with the highest bits
	 */
	public Domain getPfnDomain()
	{
		
	}
	
	/**
	 * this \\union d
	 */
	public Domain getUnionWith( Domain d )
	{
		
	}
	
	/**
	 * this \inter d
	 * @param d
	 * @return
	 */
	public Domain getIntersectWith( Domain d )
	{
		
	}
	
	public boolean isEmpty()
	{
		return tagsetConstants.isEmpty() && pfnConstants.isEmpty();
	}
}
