package ru.teslaprj.scheme;

import java.util.ArrayList;
import java.util.List;

public class Command
{
	public Command
				( Scheme scheme
				, String cop
				, List<String> args
				, String testSituation
				)
		throws CommandDefinitionError
	{
		this.scheme = scheme;
		
		if ( args == null )
			this.args = new ArrayList<String>();
		else
			this.args = args;
		
		if ( cop == null )
			throw new CommandDefinitionError("`cop` is null");
		
		this.cop = cop;
		
		if ( testSituation == null )
			throw new CommandDefinitionError("`testSituation` is null");
		
		this.testSituation = testSituation;		
	}
	public String getCop() {
		return cop;
	}
	public List<String> getArgs() {
		return args;
	}
	public String getTestSituation() {
		return testSituation;
	}
	
//	public String getPhysicalAddress()
//	{
//		if ( ! testSituationParameters.containsKey("AddressTranslation") )
//			throw new Error( "uncompleted templates are not supported yet" );
//
//		for( ProcedureTestSituation ts : testSituationParameters.get("AddressTranslation") )
//		{
//			if ( ts instanceof TLBHit )
//				return ((TLBHit) ts).getPhysicalAddressVar();
//			else if ( ts instanceof TLBMiss )
//				return ((TLBMiss) ts).getPhysicalAddressVar();
//		}
//		
//		throw new Error( "uncompleted templates are not supported yet" );
//	}
	
//	public boolean isLOAD()
//	{
//		return testSituationParameters.containsKey( "LoadMemory" );
//	}
//
//	public boolean isSTORE()
//	{
//		return testSituationParameters.containsKey( "StoreMemory" );
//	}
//	
//	public TLBSituation getTLBSituation()
//	{
//		if ( ! testSituationParameters.containsKey("AddressTranslation") )
//			return null;
//		for( ProcedureTestSituation ts : testSituationParameters.get("AddressTranslation") )
//		{
//			if ( ts instanceof TLBSituation )
//				return (TLBSituation)ts;
//		}
//		return null;
//	}
//	
//	public boolean hasTLBSituation()
//	{
//		return testSituationParameters.containsKey("AddressTranslation");
//	}

	private Scheme scheme;
	private String cop;
	private List<String> args;
	private String testSituation;
//	private Map<String, Set<ProcedureTestSituation> > testSituationParameters;
	private int memValueSize = -1;
	
//	public boolean hasCacheSituation()
//	{
//		return testSituationParameters.containsKey( "LoadMemory" ) ||
//		testSituationParameters.containsKey("StoreMemory");
//	}
//	
//	public CacheTestSituation getCacheSituation( int cacheLevel )
//	{
//		if ( ! hasCacheSituation() )
//			return null;
//		Set<ProcedureTestSituation> pts;
//		if ( testSituationParameters.containsKey("LoadMemory") )
//			pts = testSituationParameters.get("LoadMemory");
//		else
//			pts = testSituationParameters.get("StoreMemory");
//		
//		for( ProcedureTestSituation ts : pts )
//		{
//			if ( ts instanceof CacheTestSituation )
//			{
//				if ( ((CacheTestSituation) ts).getLevel() == cacheLevel )
//					return (CacheTestSituation)ts;
//			}
//		}
//		return null;
//	}

	/**
	 * @return 3 +> DOUBLEWORD (64bits), 
	 *         2 +> WORD (32bits), 
	 *         1 +> HALFWORD (16bits),
	 *         0 +> BYTE (8bits) 
	 */
	public int getMemoryValueSize() {
		return memValueSize;
	}
	public void setMemValueSize(int memValueSize) {
		this.memValueSize = memValueSize;
	}
	
	@Override
	public String toString()
	{
		StringBuffer output = new StringBuffer( cop + " " );
		
		boolean isFirstArgument = true;
		
		for( String arg : args )
		{
			if ( isFirstArgument )
				isFirstArgument = false;
			else
				output.append( ", " );
			output.append( arg );
		}
		
		output.append( " @ " )
		.append( testSituation );
		
//		output.append( " ( ");
//		
//		isFirstArgument = true;
//		for( String ts : testSituationParameters.keySet() )
//		{
//			if ( isFirstArgument )
//				isFirstArgument = false;
//			else
//				output.append( ", " );
//			
//			output.append( ts ).append( "{ " );
//			for( ProcedureTestSituation pts : testSituationParameters.get(ts) )
//			{
//				//TODO сделать toString() по иерархии ProcedureTestSituation
//				output.append( pts.toString() ).append( " " );
//			}
//			output.append( " }" );
//		}
//		
//		output.append( " )" );
		
		return output.toString();
	}
	
	public Scheme getScheme()
	{
		return scheme;
	}
}
