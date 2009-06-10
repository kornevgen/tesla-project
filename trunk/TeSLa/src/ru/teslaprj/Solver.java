package ru.teslaprj;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Random;
import java.util.Set;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import ru.teslaprj.constraints.pfn.Argument;
import ru.teslaprj.constraints.pfn.Constant;
import ru.teslaprj.constraints.pfn.PFNsAreDifferent;
import ru.teslaprj.constraints.pfn.Variable;
import ru.teslaprj.ranges.Inconsistent;
import ru.teslaprj.ranges.RangeIterator;
import ru.teslaprj.ranges.Ranges;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.SchemeDefinitionError;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;
import ru.teslaprj.scheme.ts.TLBExists;
import ru.teslaprj.scheme.ts.TLBHit;
import ru.teslaprj.scheme.ts.TLBMiss;
import ru.teslaprj.scheme.ts.TLBRefill;
import ru.teslaprj.scheme.ts.TLBSituation;
import ru.teslaprj.syntax.BitLen;
import ru.teslaprj.syntax.LogicalVariable;
import ru.teslaprj.syntax.SemanticException;
import ru.teslaprj.syntax.TeSLaLexer;
import ru.teslaprj.syntax.TeSLaParser;
import ru.teslaprj.syntax.VarsController;
import ru.teslaprj.syntax.LogicalVariable.Status;

import com.parctechnologies.eclipse.CompoundTerm;
import com.parctechnologies.eclipse.EclipseEngine;
import com.parctechnologies.eclipse.EclipseEngineOptions;
import com.parctechnologies.eclipse.EclipseException;
import com.parctechnologies.eclipse.EmbeddedEclipse;
import com.parctechnologies.eclipse.Fail;

public class Solver
{
	public static final String eoln = System.getProperty("line.separator");
	
	final private File sourcePath;
	final private File libPath;

    // Object representing the Eclipse process
    private static EclipseEngine eclipse = null;


	/**
	 * @param sourcePath	директория с описаниями тестовых ситуаций
	 * @param libPath		директория с clp-модулями (numbers.ecl, predicates.ecl)
	 */
	public Solver( File sourcePath, File libPath )
	{
		this.sourcePath = sourcePath;
		this.libPath = libPath;
		
	}
	
	static
	{
		if ( ! System.getProperties().containsKey( "eclipse.directory" ) )
		{
			throw new MissingResourceException("variable 'eclipse.directory' is not set", "", "eclipse.directory");
		}
		
		if ( ! new File(System.getProperty("eclipse.directory") ).exists() )
		{
			throw new MissingResourceException( "folder 'eclipse.directory' doesn't exist", "", "eclipse.directory" );
		}
		
        // Create some default Eclipse options
        EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();

        // Connect the Eclipse's standard streams to the JVM's
        eclipseEngineOptions.setUseQueues(false);

        try
        {
	        // Initialise Eclipse
	        eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
        }
        catch( IOException e )
        {
        	throw new Error(e);
        }
        catch( EclipseException e )
        {
        	throw new Error(e);
        }
	}
	
	public class Unsuccessful extends Exception
	{
		private static final long serialVersionUID = 2483583682716798422L;		
	}
	
	public Verdict solve( Scheme scheme, List<Cache> cacheState, TLB tlb )
		throws SchemeDefinitionError, IOException, EclipseException, RecognitionException
	{
		// 0. check correctness of the `scheme`
		checkVarsKnowness( scheme );
				
		if ( cacheState == null )
		{
			cacheState = new ArrayList<Cache>();
		}
		
		if ( tlb != null )
		{
			for( Cache cache : cacheState )
			{
				if ( cache.getAddressBitLength() != tlb.getPABITS() )
				{
					throw new SemanticException( null, "Size of physical address in tlb must be equal with one in caches" );
				}
			}
		}
		
		RangeIterator it = new RangeIterator( tlb, cacheState, scheme );
		while( it.hasNext() )
		{
			Ranges ranges = it.next();
			
			try
			{
				if ( ! ranges.isConsistency() )
				{
					System.out.println("inconsistent :(");
					continue;
				}
			}
			catch(Inconsistent e )
			{
				System.out.println("inconsistent :(");
				continue;
			}
			
			System.out.println("consistent!");
			break;
			
//			File tmp;
//			CompoundTerm result = null;
//			try
//			{
//				String moduleName = createTempModuleName();
//				tmp = File.createTempFile(moduleName, ".ecl", libPath);
//				moduleName = tmp.getName();
//				moduleName = moduleName.substring(0, moduleName
//						.length() - 4);
//
//				// TODO построение прологовской программы
//				// из каждого тегсета транслируется его ограничение
//				writeFile(tmp, newTranslate(scheme, moduleName, cacheState, tlb) );
//
//				// run ECLiPSe and get results
//				result = callECLiPSe(tmp, moduleName, scheme
//						.getDefinedNames(), cacheState);
//
//				// TODO print current result with current newCount
//
//				// analyze results
//				return generateValues( result, scheme, cacheState ) ;
////					return new Verdict(new HashMap<Definition, BigInteger>(), null );
//
//			}
//			finally
//			{
//				if (tmp != null) tmp.delete();
//			}
		}
		
		return null;
	}
	
	private synchronized static String createTempModuleName()
	{
		Random rnd = new Random();
		int len = rnd.nextInt(5) + 3 ;
		StringBuffer sbuf = new StringBuffer();
		for( int i = 0; i < len; i++ )
		{
			sbuf.append( Character.toChars( rnd.nextInt('z' - 'a') + 'a' )[0] );
		}
		return sbuf.toString();
	}

	private static void checkVarsKnowness( Scheme scheme )
    	throws SchemeDefinitionError
    {
    	List<String> names = scheme.getDefinedNames();
    	for( Command cmd : scheme.getCommands() )
    	{
    		for( String name : cmd.getArgs() )
    		{
    			if ( ! names.contains( name ) )
    				throw new SchemeDefinitionError( "unknown variable: " + name );
    		}
    	}
    }

    private Verdict generateValues( 
    		  final CompoundTerm result
    		, final Scheme scheme
    		, final List<Cache> cacheLevels
    	)
    {
    	final Map<Definition, BigInteger> values = new HashMap<Definition, BigInteger>();
    	final List< Map<Long,List<Long>> > caches = new ArrayList<Map<Long,List<Long>>>();

    	int defNumber = 2;
    	
    	// cache
    	@SuppressWarnings("unchecked")
    	List<List<CompoundTerm>> cacheResults = (List<List<CompoundTerm>>)result.arg( defNumber++ );
//    	int cacheLevel = 0;
//    	for( List<CompoundTerm> setsForCacheLevel : cacheResults )
//    	{
//    		Map< Long, List<Long> > sets = new HashMap<Long, List<Long>>();
//    		int setSize = cacheLevels.get( cacheLevel ++ ).getSectionNumber();
//    		for( CompoundTerm set : setsForCacheLevel )
//    		{
//    			Object setNumber = set.arg(1);
//    			Long longSetNumber;
//				if ( setNumber instanceof Integer )
//					longSetNumber = (long)((Integer)setNumber).intValue();
//				else if ( setNumber instanceof Long )
//					longSetNumber = (Long)setNumber;
//				else
//					throw new Error( "unexpected eclipse behaviour: unknown type " + setNumber.getClass() );
//
//				@SuppressWarnings( "unchecked" )
//				List<CompoundTerm> hits = (List<CompoundTerm>)set.arg(3);
//    			List<Long> tags = new ArrayList<Long>();
//    			for( int i = 0; i < setSize; i++ )
//    			{
//    				Object tag = hits.get( i ).arg(1);
//    				if ( tag instanceof Integer )
//    					tags.add( (long)((Integer)tag).intValue() );
//    				else if ( tag instanceof Long )
//    					tags.add( (Long)tag );
//    				else
//    					throw new Error( "unexpected eclipse behaviour: unknown type " + tag.getClass() );
//    			}
//    			sets.put( longSetNumber, tags );
//    		}
//    		caches.add( sets );
//    	}
        
    	
    	// TLB
    	final Map<Integer, TLBRow> tlbrows = new HashMap<Integer, TLBRow>();
    	@SuppressWarnings("unchecked")
    	List<CompoundTerm> tlbResults = (List<CompoundTerm>)result.arg( defNumber++ );
    	for( CompoundTerm tlbrow : tlbResults )
    	{
    		final Integer index = (Integer)tlbrow.arg(1);
    		final CompoundTerm tlbtag = (CompoundTerm)tlbrow.arg(2);
    		final Integer Range = BitLen.intlist2bigint( (List<?>)tlbtag.arg(2) ).intValue();
    		final BigInteger VPNd2 = BitLen.intlist2bigint( (List<?>)tlbtag.arg(3) );
    		final Integer Mask = (Integer)tlbtag.arg(4);
    		final Integer G = (Integer)tlbtag.arg(5);
    		final Integer Asid = BitLen.intlist2bigint( (List<?>)tlbtag.arg(6) ).intValue();
    		final BigInteger pfn0 = BitLen.intlist2bigint( (List<?>)tlbrow.arg(3) );
    		final Integer Valid0 = (Integer)tlbrow.arg(4);
    		final Integer moDify0 = (Integer)tlbrow.arg(5);
    		final BigInteger pfn1 = BitLen.intlist2bigint( (List<?>)tlbrow.arg(6) );
    		final Integer Valid1 = (Integer)tlbrow.arg(7);
    		final Integer moDify1 = (Integer)tlbrow.arg(8);
    		
    		tlbrows.put(index, new TLBRow(){
				@Override
				public Integer getMask() {
					return Mask;
				}

				@Override
				public BigInteger getPFN0() {
					return pfn0;
				}

				@Override
				public BigInteger getPFN1() {
					return pfn1;
				}

				@Override
				public Integer getRange() {
					return Range;
				}

				@Override
				public BigInteger getVPNd2() {
					return VPNd2;
				}

				@Override
				public Integer getAsid() {
					return Asid;
				}

				@Override
				public Integer getGlobal() {
					return G;
				}

				@Override
				public int getValid0() {
					return Valid0;
				}

				@Override
				public int getValid1() {
					return Valid1;
				}

				@Override
				public int getmoDify0() {
					return moDify0;
				}

				@Override
				public int getmoDify1() {
					return moDify1;
				}});
    	}

    	
    	// memory cells
    	final Map<BigInteger, BigInteger> memory = new HashMap<BigInteger, BigInteger>();
    	@SuppressWarnings("unchecked")
    	List<CompoundTerm> memoryResults = (List<CompoundTerm>)result.arg( defNumber++ );
    	for( CompoundTerm cell : memoryResults )
    	{
    		BigInteger address = BitLen.intlist2bigint( (List<?>)cell.arg(1) );
    		BigInteger value = BitLen.intlist2bigint( (List<?>)cell.arg(2) );
    		memory.put( address, value ); 
    	}
    	
    	// tlbindexes
    	final Map<Command, Integer> tlbIndexes = new HashMap<Command, Integer>();
    	List<?> indexes = (List<?>)result.arg( defNumber++ );
    	for( int i = 0; i < indexes.size(); i++ )
    	{
    		Integer index = (Integer)indexes.get(i);
    		Command command = scheme.getCommands().get(i);
    		tlbIndexes.put(command, index);
    	}
    	
    	// registers and constants
		for( Definition def : scheme.getDefinitions() )
		{
			List<?> arg = (List<?>)result.arg( defNumber++ );
			values.put( def, BitLen.intlist2bigint( arg ) );
		}
		
		return new Verdict(){

			@Override
			public List<Map<Long, List<Long>>> getCacheInitialization() {
				return caches;
			}

			@Override
			public Map<Definition, BigInteger> getDefinitionValues() {
				return values;
			}

			@Override
			public Map<BigInteger, BigInteger> getMemory() {
				return memory;
			}

			@Override
			public Map<Integer, TLBRow> getTlbrows() {
				return tlbrows;
			}

			@Override
			public Map<Command, Integer> getTLBIndexes() {
				return tlbIndexes;
			}};
    }

	private void writeFile( File file, StringBuffer text )
    	throws IOException
    {
		java.io.FileWriter writer = new java.io.FileWriter( file );
		try
		{
			writer.write( text.toString() );
		}
		finally
		{
			writer.close();
		}
    }    
}