package ru.teslaprj;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LRUCollector
{
	/** { "x1", "x2" } */
	private List<String> tags = new ArrayList<String>();
	
	/** { 1 +> "S1", 14 +> "S2" } keys - index, before that set is changed */
	private Map<Integer, String> setVars = new HashMap<Integer, String>();

	/** { 1 +> "X2v", 14 +> "X5v" } keys - index, before that tag is vytesn */
	private Map<Integer, String> vytesnTagsVars = new HashMap<Integer, String>();

	public int getHitTagIndex(String tag)
	{
		return tags.indexOf( tag );
	}

	public List<String> getReversedHitsFrom(int tagIndex)
	{
		List<String> result = new ArrayList<String>();
		for( int i = tagIndex; i >= 0; i-- )
		{
			result.add( tags.get(i) );		
		}
		return result;
	}
	
	public List<String> getAllHits()
	{
		return tags;
	}

	public String getSetVarBefore( int tagIndex )
	{
//		int tagIndex = getHitTagIndex( tag );
		int closest_index = 0;
		for( int index : setVars.keySet() )
		{
			if ( index < tagIndex )
			{
				if ( index > closest_index )
					closest_index = index;
			}
		}
		return setVars.get( closest_index );
	}

	public List<String> getTagsRange(int startIndex, int endIndex)
	{
		return tags.subList( startIndex, endIndex );
	}

	public List<String> getReversedVytTags()
	{
		Integer[] indexes = vytesnTagsVars.keySet().toArray( new Integer[]{} );
		Arrays.sort( indexes );
		List<String> reversedVytTags = new ArrayList<String>();
		for( int i = indexes.length - 1; i >= 0; i-- )
		{
			reversedVytTags.add( vytesnTagsVars.get( indexes[i] ) );
		}
		return reversedVytTags;
	}

	public void addHit(String tag)
	{
		tags.add( tag );
	}

	public void upgradeSet( String nextSetVar )
	{
		setVars.put( tags.size(), nextSetVar );
	}

	public void upgradeSet( String nextSetVar, String vytesnTag )
	{
		upgradeSet( nextSetVar );
		vytesnTagsVars.put( tags.size(), vytesnTag );
	}

	public int getIndexBeforeVytesn( String vytesnTag )
	{
		for( Integer index : vytesnTagsVars.keySet() )
		{
			if ( vytesnTagsVars.get( index ).equals( vytesnTag ) )
			{
				return index;
			}
		}
		return -1;
	}

	public boolean tagIsMiss(String tag)
	{
		return setVars.containsKey( tags.indexOf( tag ) );
	}
}
