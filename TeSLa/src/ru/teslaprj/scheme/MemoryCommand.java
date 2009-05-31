package ru.teslaprj.scheme;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ru.teslaprj.Cache;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.TLBSituation;

public class MemoryCommand extends Command implements MemoryAccessInstruction
{
	Map<Cache, CacheTestSituation> cacheSituations; 
	TLBSituation tlbSituation; 
	
	public MemoryCommand
			( Scheme scheme
			, String cop
			, List<String> args
			, String testSituation
			, Map<Cache, CacheTestSituation> cacheSituations
			, TLBSituation tlbSituation
			)
	throws CommandDefinitionError
	{
		super(scheme, cop, args, testSituation);

		this.cacheSituations = cacheSituations;
		this.tlbSituation = tlbSituation;
		if ( this.cacheSituations == null )
		{
			this.cacheSituations = new HashMap<Cache, CacheTestSituation>();
		}
		
		for( CacheTestSituation p : this.cacheSituations.values() )
		{
			p.setCommand(this);
		}
	}
	
	String tagset = null;
	
	static private int tagsetNumber = 0;
	static private final synchronized String getNewTagset()
	{
		return "ts" + (tagsetNumber++);
	}

	@Override
	public String getTagset()
	{
		if ( tagset == null )
		{
			tagset = getNewTagset();
		}
		return tagset;
	}

	@Override
	public CacheTestSituation getCacheSituation( Cache cacheLevel) {
		return cacheSituations.get(cacheLevel);
	}

	@Override
	public TLBSituation getTLBSituation() {
		return tlbSituation;
	}

	@Override
	public boolean hasCacheSituation( Cache cacheLevel ) {
		return cacheSituations.containsKey(cacheLevel);
	}

	@Override
	public boolean hasTLBSituation() {
		return tlbSituation != null;
	}

	@Override
	public boolean isLOAD() {
		// TODO это можно узнать только после разбора описания тестовой ситуации...
		return false;
	}

	@Override
	public boolean isSTORE() {
		// TODO Auto-generated method stub
		return false;
	}
	
}
