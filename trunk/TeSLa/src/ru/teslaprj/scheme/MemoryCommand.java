package ru.teslaprj.scheme;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ru.teslaprj.Cache;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.TLBSituation;

public class MemoryCommand extends Command implements MemoryAccessInstruction
{
	final Map<Cache, CacheTestSituation> cacheSituations; 
	final TLBSituation tlbSituation; 
	final boolean load;
	
	public MemoryCommand
			( Scheme scheme
			, String cop
			, List<String> args
			, String testSituation
			, Map<Cache, CacheTestSituation> cacheSituations
			, TLBSituation tlbSituation
			, boolean isLoad
			)
	throws CommandDefinitionError
	{
		super(scheme, cop, args, testSituation);

		this.tlbSituation = tlbSituation;
		if ( cacheSituations == null )
		{
			this.cacheSituations = new HashMap<Cache, CacheTestSituation>();
		}
		else
		{
			this.cacheSituations = cacheSituations;
		}
		
		for( CacheTestSituation p : this.cacheSituations.values() )
		{
			p.setCommand(this);
		}
		this.tlbSituation.setCommand(this);
		this.load = isLoad;
	}
	
	String tagset = null;
	
	static private int counter = 0;
	static private final synchronized String getNewTagset()
	{
		return "ts" + (counter++);
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
		return load;
	}

	@Override
	public boolean isSTORE() {
		// TODO это можно узнать только после разбора описания тестовой ситуации...
		return !load;
	}

	String virtualAddress = null;
	
	static private final synchronized String getNewVirtualAddress()
	{
		return "va" + (counter++);
	}

	@Override
	public String getVirtualAddress()
	{
		if ( virtualAddress == null )
		{
			virtualAddress = getNewVirtualAddress();
		}
		return virtualAddress;
	}

	@Override
	public String getResult() {
		return getArgs().get(0);
	}

	@Override
	public String getValueOfTagset() {
		return "v_" + getTagset();
	}
	
	//TODO эти методы лишние - это должно вычисляться автоматически
	private String value;
	public String getValue() { return value;}
	public void setValue( String value )
	{
		this.value = value;
	}
	
}
