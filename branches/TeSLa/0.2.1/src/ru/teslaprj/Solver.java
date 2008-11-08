package ru.teslaprj;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import ru.teslaprj.scheme.Assert;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.Commandlike;
import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.SchemeDefinitionError;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;
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

public class Solver
{
	private static final String eoln = System.getProperty("line.separator");
	
	private File sourcePath;
	private File libPath;
	
	private Map<Cache, Map<Integer, Integer>> currentSetVersions;
	
	/**
	 * @param sourcePath	директория с описаниями тестовых ситуаций
	 * @param libPath		директория с clp-модулями (numbers.ecl, predicates.ecl)
	 */
	public Solver( File sourcePath, File libPath )
	{
		this.sourcePath = sourcePath;
		this.libPath = libPath;
	}
	
	public Verdict solve( Scheme scheme, List<Cache> cacheState )
		throws SchemeDefinitionError, IOException, EclipseException, RecognitionException
	{
		if ( cacheState == null || cacheState.isEmpty() )
		{
			throw new SchemeDefinitionError("cache is empty");
		}
		
		// 0. check correctness of the `scheme`
		checkVarsKnowness( scheme );
		
		for( Cache cache : cacheState )
		{			
			if ( cache.getTagBitLength() > BitLen.WORD_VALUE )
			{
				throw new SemanticException( null, "operations with addresses more than " + BitLen.WORD_VALUE + " bits are not implemented yet");
			}
		}
		
		// 1. translate `scheme` to .clp
		String moduleName = createTempModuleName();
		File tmp = File.createTempFile( moduleName, ".ecl", libPath );
		moduleName = tmp.getName();
		moduleName = moduleName.substring(0, moduleName.length() - 4 );

		try
		{
			writeFile( tmp, translate( scheme, moduleName, cacheState ) );
			
			// 2. run ECLiPSe and get results
//			CompoundTerm result = callECLiPSe( tmp, moduleName, scheme.getDefinedNames() );
			
			// 3. analyze results
//			return generateValues( result, scheme );
			return new Verdict(new HashMap<Definition, BigInteger>(), null );
		}
		finally
		{
//			tmp.delete();
		}
	}
	
	public class Verdict
	{
		public Verdict(
				  Map<Definition, BigInteger> definitionValues
				, List<Map<Long, List<Long>>> cacheInitialization
				)
		{
			this.cacheInitialization = cacheInitialization;
			this.definitionValues = definitionValues;
		}
		
		/**
		 * @return [ definition +> value ]
		 */
		public Map<Definition, BigInteger> getDefinitionValues() {
			return definitionValues;
		}
		/**
		 * @return [ set +> tag-list ]-list (by levels)
		 */
		public List< Map<Long, List<Long>> > getCacheInitialization() {
			return cacheInitialization;
		}
		
		private Map<Definition, BigInteger> definitionValues;
		private List< Map< Long, List<Long> > > cacheInitialization;
		
	}
	
	private String createTempModuleName()
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
	
    private CompoundTerm callECLiPSe( final File eclipseProgram, final String moduleName, final List<String> names ) 
    	throws EclipseException, IOException
	{
	    // Object representing the Eclipse process
	    EclipseEngine eclipse = null;
	
		try
		{
	        // Create some default Eclipse options
	        EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();
	
	        // Connect the Eclipse's standard streams to the JVM's
	        eclipseEngineOptions.setUseQueues(false);
	
	        // Initialise Eclipse
	        eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
		
	        // Compile the eclipse program.
	        eclipse.compile( eclipseProgram );
	
	        StringBuffer goal = new StringBuffer();
	        goal.append( moduleName ).append( ":go(_" );
	        for( String name : names )
	        {
        		goal.append( ",_" + name ) ;
	        }
	        goal.append(")");
	        
	        CompoundTerm result = eclipse.rpc( goal.toString() );
	        
	        return (CompoundTerm) result.arg(2);
		}
		finally
		{
	        // Destroy the Eclipse process
			try { ((EmbeddedEclipse) eclipse).destroy(); } catch( IOException e ) {}
		}
	}

    private void checkVarsKnowness( Scheme scheme )
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

    private Verdict generateValues( CompoundTerm result, Scheme scheme )
    {
    	Map<Definition, BigInteger> values = new HashMap<Definition, BigInteger>();

    	List<Definition> defs = scheme.getDefinitions();
		for( int i = 0; i < defs.size(); i++ )
		{
			List<?> arg = (List<?>)result.arg( i + 2 );
			values.put( defs.get(i), BitLen.intlist2bigint( arg ) );
		}
		
		return new Verdict( values, null );
    }

    private StringBuffer translate( 
    		  Scheme scheme
    		, String moduleName
    		, List<Cache> cacheLevels )
    	throws SchemeDefinitionError, IOException, RecognitionException
    {
		//TODO реализовать решение задачи на сеты
    	Map<Command, Map<Cache, Integer> > commandsSetNumbers = new HashMap<Command, Map<Cache,Integer>>();
    	for( Command command : scheme.getCommands() )
    	{
    		Map<Cache, Integer> sets = new HashMap<Cache, Integer>();
    		for( Cache cache : cacheLevels )
    		{
    			sets.put( cache, 0 );
    		}
    		commandsSetNumbers.put( command, sets );
    	}
    	Map<Cache, List<Integer>> allSets = calculateSets( commandsSetNumbers );



    	StringBuffer ecl = new StringBuffer();
		
		ecl.append(":- module( " ).append( moduleName ).append( " )." ).append(eoln);
		ecl.append(":- lib( ic ).").append(eoln);
		ecl.append(":- lib( ic_sets ).").append(eoln);
		ecl.append(":- use_module( lru ).").append(eoln);
		ecl.append(":- use_module( numbers ).").append(eoln);
		ecl.append(":- use_module( predicates ).").append(eoln);		
		ecl.append(eoln);

		StringBuffer commandPredicates = new StringBuffer();
    	
    	// сгенерировать предикат go: последовательность вызовов унифицированных "предикатов go"
		List<String> names = scheme.getDefinedNames();
		ecl.append( ":- export go/" ).append( names.size() + allSets.values().size() + 1 ).append( "." ).append(eoln).append(eoln);
    	ecl.append( "go( ")
//    	.append( "SetsFinal, " )
    	.append( "SetsInitial" );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getIniTagsOfSet( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}    	
		for( String name : names )
		{
			ecl.append( ", _" + name );
		}
		int Mmax = 1 * cacheLevels.get(0).getSectionNumber();  //sum by levels of: length( SetNumbers ) * length( tags for one set ) /associativity/;
    	ecl
    	.append(" ) :- ").append( eoln )
    	.append( "\tbinarySearchForOperators( SetsInitial, 0, ").append(Mmax).append( ", " ).append( Mmax * 2 ).append(", _, _, _, InitialProgramsLengths, SetsFinal, [ _");
		for( String name : names )
		{
			ecl.append( ", _" + name );
		}
		ecl
    	.append( " ] ),").append(eoln)
    	.append( eoln )
    	.append( "\tSetsInitial = [ _");
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getIniSetVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl.append( " ]," ).append( eoln );    	

    	ecl.append( "\tSetsFinal = [ _");
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getSetVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl.append( " ]," ).append( eoln );    	

    	ecl
    	.append( "\tInitialProgramsLengths = [ 0" );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getIniProgLenVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl
    	.append( " ]," ).append( eoln );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    	    	ecl.append( "\tlength( " )
    	    	.append( getIniTagsOfSet( cacheLevels.indexOf( cache ) + 1, set ) )
    	    	.append( ", " )
    	    	.append( getIniProgLenVar( cacheLevels.indexOf( cache ) + 1, set ) )
    	    	.append( ")," )
    	    	.append( eoln )
    	    	.append( "\tsubtract( " )
    	    	.append( getSetVar( cacheLevels.indexOf( cache ) + 1, set ) )
    	    	.append( ", " )
    	    	.append( getIniTagsOfSet( cacheLevels.indexOf( cache ) + 1, set ) )
    	    	.append( ", _ )," ).append( eoln );
    		}
    	}
 
    	ecl
    	.append( "true." ).append( eoln )
    	.append( eoln );

		
		
    	ecl
    	.append( "binarySearchForOperators(")
    		.append( "  SetsInitial" )
    		.append( ", MinM, CurrentM, _" )
    		.append( ", LastSuccessLi, LastSuccessSetsFinal, LastSuccessP" )
    		.append( ", SuccessLi, SuccessSetsFinal, SuccessP" )
    	.append( " ) :-" ).append( eoln )
    	.append( "\tInitialProgramsLengths = [ 0" );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getIniProgLenVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl
    	.append( " ]," ).append( eoln );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( "\t" ).append( getIniProgLenVar( cacheLevels.indexOf( cache ) + 1, set ) ).append( " #>= 0,").append( eoln );
    		}
    	}
    	ecl
    	.append( "\tCurrentM #= 0" );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( " + " ).append( getIniProgLenVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl.append( "," ).append( eoln )
    	.append( "\tlabeling( InitialProgramsLengths )," ).append( eoln );

    	ecl.append( "\tSetsInitial = [ _");
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getIniSetVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl.append( " ]," ).append( eoln );    	

    	ecl.append( "\tSetsFinal = [ _");
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getSetVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl.append( " ]," ).append( eoln );    	

    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl
    			.append( "\tlru:setConditions( " )
    			.append( getIniProgLenVar( cacheLevels.indexOf( cache ) + 1, set ) )
    			.append( ", " )
    			.append( getIniSetVar( cacheLevels.indexOf( cache ) + 1, set ) )
    			.append( ", ")
    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, set ) )
    			.append( " )," ).append( eoln );
    		}
    	}

    	ecl
    	.append( eoln )
    	.append( "\toperators( SetsFinal, P )," ).append( eoln )
    	.append( eoln );
    	for( Cache cache : cacheLevels )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( "\tlabeling( " )
    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, set ) )
    			.append( " )," ).append( eoln );
    		}
    	}
    	
    	ecl
    	.append( "\t!," ).append( eoln )
    	.append( "\t( MinM = CurrentM ->" ).append( eoln )
    	.append( "\t\tSuccessSetsFinal = SetsFinal," ).append( eoln )
    	.append( "\t\tSuccessLi = InitialProgramsLengths," ).append( eoln )
    	.append( "\t\tSuccessP = P," ).append( eoln )
    	.append( "\t\tLastSuccessLi = InitialProgramsLengths," ).append( eoln )
    	.append( "\t\tLastSuccessSetsFinal = SetsFinal," ).append( eoln )
    	.append( "\t\tLastSuccessP = P" ).append( eoln )
    	.append( "\t;" ).append( eoln )
    	.append( "\t\tNewCurrent is ( MinM + CurrentM ) div 2," ).append( eoln )
    	.append( "\t\tbinarySearchForOperators( SetsInitial, MinM, NewCurrent, CurrentM, InitialProgramsLengths, SetsFinal, P, SuccessLi, SuccessSetsFinal, SuccessP )").append( eoln )
    	.append( "\t) ." ).append( eoln )
    	.append( eoln )
    	
    	.append( "binarySearchForOperators(" )
    		.append( "  SetsInitial" )
    		.append( ", MinM, CurrentM, PreviousM" )
    		.append( ", LastSuccessLi, LastSuccessSetsFinal, LastSuccessP" )
    		.append( ", SuccessLi, SuccessSetsFinal, SuccessP" )
    	.append( " ) :-").append( eoln )
    	.append( "\tMinP1 is MinM + 1," ).append( eoln )
    	.append( "\t( PreviousM = MinP1 ->" ).append( eoln )
    	.append( "\t\tSuccessLi = LastSuccessLi," ).append( eoln )
    	.append( "\t\tSuccessSetsFinal = LastSuccessSetsFinal," ).append( eoln )
    	.append( "\t\tSuccessP = LastSuccessP" ).append( eoln )
    	.append( "\t;" ).append( eoln )
    	.append( "\t\tCurrentM < " ).append( Mmax ).append( "," ).append( eoln )
    	.append( "\t\tCurrentM < PreviousM," ).append( eoln )
    	.append( "\t\tNewCurrent is ( CurrentM + PreviousM ) div 2," ).append( eoln )
    	.append( "\t\tbinarySearchForOperators( SetsInitial, CurrentM, NewCurrent, PreviousM, LastSuccessLi, LastSuccessSetsFinal, LastSuccessP, SuccessLi, SuccessSetsFinal, SuccessP )").append( eoln )
    	.append( "\t) ." ).append( eoln )
    	.append( eoln );
    	
    	ecl.append( "operators( [ _" );
    	// initial states of sets
    	for( Cache cache : allSets.keySet() )
    	{
    		List<Integer> sets = allSets.get( cache );
    		for( int set : sets )
    		{
    			ecl.append( ", " ).append( getSetVar( cacheLevels.indexOf( cache ) + 1, set ) );
    		}
    	}
    	ecl.append( " ], [ _" );
		for( String name : names )
		{
			ecl.append( ", _0" + name );
		}
    	ecl.append( " ] ) :-" ).append( eoln )
    	.append( eoln );

    	for( Cache cache : allSets.keySet() )
    	{
    		List<Integer> sets = allSets.get( cache );
    		int tagsCount = cache.getSectionNumber();
    		for( int setNumber : sets )
    		{
    			ecl
    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, setNumber) )
    			.append( " = [  ").append( getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, 1 ) );
    			for( int k = 2; k <= tagsCount; k++ )
    			{
    				ecl.append( ", " ).append( getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, k ) );
    			}
    			ecl.append( "]," ).append( eoln );

    			for( int k = 1; k <= tagsCount; k++ )
    			{
    				String tag = getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, k );
					ecl.append( "intset( " ).append( tag ).append( "set, 0, " ).append( (int)Math.pow(2, cache.getTagBitLength() ) - 1 ).append( " ), " )
					.append( "#( " ).append( tag ).append( "set, 1 ), " )
					.append( tag ).append( " in " ).append( tag ).append( "set," ).append( eoln );
    			}
    			
    			ecl.append( eoln )
    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, setNumber) )
    			.append( "_0 = []");
    			
    			for( int k = 1; k <= tagsCount; k++ )
    			{
    				String tag = getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, k );
					ecl.append( " \\/ " ).append( tag ).append( "set" );
    			}
    			
    			ecl.append( "," ).append( eoln )
    			.append( eoln );
    		}
    	}
    	
    	Map<String, Integer> varVersions = new HashMap<String, Integer>();
    	for( String name : names )
    	{
    		varVersions.put( name, 0 );
    	}
    	
    	for( Assert asert : scheme.getAsserts() )
    		commandPredicates.append( commandlikeTranslate( asert, scheme, varVersions, ecl, null, cacheLevels, null, null ) );

    	VarsController tagNames = new VarsController();
    	
    	currentSetVersions = new HashMap<Cache, Map<Integer,Integer>>();
    	
    	Map<Cache, Map<Integer, LRUCollector>> collectors = new HashMap<Cache, Map<Integer,LRUCollector>>();
    	for( int i = 0; i < cacheLevels.size(); i++ )
    	{
    		Cache cache = cacheLevels.get(i);
    		currentSetVersions.put( cache, new HashMap<Integer, Integer>() );
    		List<Integer> setsOfThisCache = allSets.get( cache );
    		
    		Map<Integer, LRUCollector> cacheCollectors = new HashMap<Integer, LRUCollector>();
    		int tagsCount = cache.getSectionNumber();

    		// add initial content of set to the 'collector'
    		for( Integer setNumber : setsOfThisCache )
    		{
    			LRUCollector collector = new LRUCollector();
    			
    			for( int k = tagsCount; k > 0; k-- )
    			{
	    			collector.addHit( getIniTagVar( i + 1, setNumber, k ) );
    			}
    			collector.upgradeSet( getSetVar( i + 1, setNumber ) + "_0" );
    			
    			cacheCollectors.put( setNumber, collector );
    		}
    		collectors.put( cache, cacheCollectors );
    	}
   
    	
    	for( Command command : scheme.getCommands() )
    	{
    		commandPredicates.append( commandlikeTranslate( 
    				  command
    				, scheme
    				, varVersions
    				, ecl
    				, tagNames
    				, cacheLevels
    				, commandsSetNumbers.get( command )
    				, collectors
    			) );
    	}
    	
    	ecl.append( eoln );
    	ecl.append( "% LRU predicates" ).append( eoln ).append( eoln );
    	// LRU predicates
    	for( Cache cache : cacheLevels )
    	{
    		ecl.append( "% next cache level" ).append( eoln );
    		
    		Map<Integer, LRUCollector> collectorsForThisCache = collectors.get( cache );
    		for( LRUCollector collector : collectorsForThisCache.values() )
    		{
    			ecl.append( "% next set for this cache level" ).append( eoln );
	    		String tagCase = null; // CaseX5
		    	for( String tag : collector.getReversedVytTags() )
		    	{
		    		ecl.append( "% lru( " ).append( tag ).append( " )" ).append( eoln );
		    		if ( tagCase != null )
		    		{
		    			ecl.append( tagCase ).append( " #> " );
		    			tagCase = "Case" + tag;
		    			ecl.append( tagCase ).append( eoln )
		    			.append( eoln );
		    		}
		    		else
		    		{
		    			tagCase = "Case" + tag;
		    		}
		    		
		    		int index_before_vytesn = collector.getIndexBeforeVytesn( tag );
		    		int lastHitIndex = index_before_vytesn - cache.getSectionNumber();
		    		boolean hitsAreNotEmpty = lastHitIndex >= 0;
		    		if ( hitsAreNotEmpty )
		    		{
		    			ecl.append( "( false ").append( eoln );
		    		}
		    		for( String hitTag : collector.getReversedHitsFrom( lastHitIndex ) )
		    		{
		    			// ; CaseX5 = 5, X6 = C5, S2 \ TX6 sameset TX1 \/ TX3 \/ TX4 \/ TX5
		    			ecl.append( "; " ).append( tagCase ).append( " = " ).append( lastHitIndex )
		    			.append( ", " ).append( tag ).append( " = " ).append( hitTag )
		    			.append( ", " ).append( collector.getSetVarBefore( tag ) ).append( " \\ " ).append( tag ).append( "set sameset [] ");
		    			for( String immediateTag : collector.getTagsRange( lastHitIndex + 1, index_before_vytesn ) )
		    			{
		    				ecl.append( " \\/ " ).append( immediateTag ).append( "set" );
		    			}
		    			ecl.append( eoln );
		    			
		    			lastHitIndex -- ;
		    		}
		    		if ( hitsAreNotEmpty )
		    		{
		    			ecl.append( ")," ).append( eoln );
		    		}
	    			ecl.append( eoln );
		    	}
    		}
    	}
    	
		// предикаты выбора случайных значений для переменных
		for( String name : names )
		{
			ecl	.append( "numbers:random_result( _0" ).append( name ).append( " )," )
				.append( eoln );
		}
	
		ecl.append( "true.").append(eoln).append(eoln)
		.append( commandPredicates );

    	return ecl;
    }
    
    private static Map<Cache, List<Integer>> calculateSets(
			Map<Command, Map<Cache, Integer>> commandsSetNumbers)
	{
    	Map<Cache, List<Integer>> result = new HashMap<Cache, List<Integer>>();   	
    	
    	for( Command command : commandsSetNumbers.keySet() )
    	{
    		Map<Cache, Integer> setsForCommand = commandsSetNumbers.get( command );
    		for( Cache cache : setsForCommand.keySet() )
    		{
    			if ( result.containsKey( cache ) )
    			{
    				// add setsForCommand.get( cache ) to the 'cache tagset'
    				List<Integer> sets = result.get( cache );
    				if ( ! sets.contains( setsForCommand.get( cache ) ) )
    				{
	    				sets.add( setsForCommand.get( cache ) );
	    				result.put( cache, sets );
    				}
    			}
    			else
    			{
    				// create new 'cache tagset'
    				List<Integer> sets = new ArrayList<Integer>();
    				sets.add( setsForCommand.get( cache ) );
    				result.put( cache, sets );
    			}
    		}
    	}
    	
		return result;
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
    
    private static int nextInt = 0;
    private static synchronized int nextInt()
    {
    	return nextInt++;
    }
    
    /**
     * генерирует логический код для данной команды (предикаты)
     * @param command		команда
     * @param scheme		схема
     * @param varVersions	номера версий глобальных переменных
     * @param ecl			куда писать
     * @param tagsVersions	контроллер для получения уникальных имен переменных для тегов
     * @param cacheLevels	список переменных-кэш-уровней
     * @param setsNumbers	сеты, задействованные этой командой в каждом кэш-уровне
     * @param collectors	коллекторы, задействованные этой командой в каждом кэш-уровне и сете
     * @return
     * @throws IOException
     * @throws RecognitionException
     */
    private StringBuffer commandlikeTranslate( 
    		  final Commandlike command
    		, final Scheme scheme
    		, Map<String, Integer> varVersions
    		, StringBuffer ecl
    		, VarsController tagsVersions
    		, List<Cache> cacheLevels
    		, Map<Cache, Integer> setsNumbers
    		, Map<Cache, Map<Integer, LRUCollector>> collectors
    	)
    	throws IOException, RecognitionException
    {
		String pathToTSL = sourcePath + "\\" + command.getCop() + "\\" + command.getTestSituation() + ".tsl";
		CharStream inputStream = new ANTLRFileStream( pathToTSL );
		TeSLaLexer lexer = new TeSLaLexer( inputStream );
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		TeSLaParser parser = new TeSLaParser(tokens);
		String prefix = command.getCop() + "@" + command.getTestSituation() + "#" + nextInt() ;
		TeSLaParser.program_return prog = parser.program( command.getArgs(), command.getAdditionalArgs(), scheme, prefix, cacheLevels );

		ecl.append( "'" + prefix + "::main'( _" );
		for( String arg : command.getArgs() )
		{
			ecl.append( ", _" + varVersions.get( arg ) + arg );
		}
		Set<String> changedVars = new HashSet<String>();
		for( int i = 0; i < command.getArgs().size(); i++ )
		{
			String arg = command.getArgs().get(i);
			LogicalVariable logicalVar = prog.signature.get(i);
			Integer version = varVersions.get( arg );
			if ( logicalVar.getStatus() == Status.SIGNATURE_RESULT )
			{
				version++;
				changedVars.add( arg );
			}
			ecl.append( ", _" + version + arg );
		}
		List<String> tags = new ArrayList<String>();
		if ( prog.hasMemoryOperation && command instanceof Command )
		{
			Command cmd = (Command)command;
			tags = calculateTags( tagsVersions, cacheLevels.size(), cmd.getTestSituationParameters() );
			for( String tag : tags )
			{
				ecl.append( ", " + tag );
			}
		}
		ecl.append("),").append(eoln);
		
		if ( command instanceof Command )
		{
			Command	cmd = (Command)command;
			Map<String, Set<ProcedureTestSituation>> testSituation = cmd.getTestSituationParameters();
			for( String procedure : testSituation.keySet() )
			{
				Set<ProcedureTestSituation> params = testSituation.get( procedure );

				if ( procedure.equals( "LoadMemory" )
					|| procedure.equals( "StoreMemory" )
					)
				{
					for( ProcedureTestSituation ts : params )
					{
						if ( ts instanceof CacheHit )
						{
							CacheHit hit = (CacheHit)ts;
							Cache cache = cacheLevels.get( hit.getLevel() - 1 );
							String tag = tags.get( hit.getLevel() - 1);
							String tagset = tag + "set";
							int setNumber = setsNumbers.get( cache );
							LRUCollector collector = collectors.get( cache ).get( setNumber );
							
							// HIT for 'tag' in 'cache'
							int currentSetVersion;
							Map<Integer, Integer> setsForThisCacheLevel = currentSetVersions.get( cache );
							if ( setsForThisCacheLevel.get( setNumber ) == null )
							{
								currentSetVersion = 0;
							}
							else
							{
								currentSetVersion = setsForThisCacheLevel.get( setNumber );
							}
							String currentSetVar = 
								getSetVar( hit.getLevel(), setNumber )
							+ "_" + currentSetVersion;
							
							ecl.append( tag ).append( " in " ).append( currentSetVar ).append( "," ).append( eoln );
							ecl.append( "intset( " ).append( tagset ).append( ", 0, " ).append( (int)Math.pow(2, cache.getTagBitLength() ) - 1 ).append( " ), " )
							.append( "#( " ).append( tagset ).append( ", 1 ), " )
							.append( tag ).append( " in " ).append( tagset ).append( "," ).append( eoln )
							.append( eoln );
							
							collector.addHit( tag );
						}
						else if ( ts instanceof CacheMiss )
						{
							CacheMiss miss = (CacheMiss)ts;
							Cache cache = cacheLevels.get( miss.getLevel() - 1 );
							String tag = tags.get( miss.getLevel() - 1);
							String tagset = tag + "set";
							int setNumber = setsNumbers.get( cache );
							LRUCollector collector = collectors.get( cache ).get( setNumber );

							// MISS for 'tag' in 'cache
							String vytesnTag = miss.getVTagVar();
							if ( vytesnTag == null )
							{
								vytesnTag = tagsVersions.newVar().toString();
							}
							String vytesnTagSet = vytesnTag + "set";

							int currentSetVersion;
							Map<Integer, Integer> setsForThisCacheLevel = currentSetVersions.get( cache );
							if ( setsForThisCacheLevel.get( setNumber ) == null )
							{
								currentSetVersion = 0;
							}
							else
							{
								currentSetVersion = setsForThisCacheLevel.get( setNumber );
							}

							String currentSetVar = 
								getSetVar( miss.getLevel(), setNumber )
							+ "_" + currentSetVersion;

							String nextSetVar = 
								getSetVar( miss.getLevel(), setNumber )
							+ "_" + ( currentSetVersion + 1 );

							/*	X2 = vytesnTag, X3 = tag
							 *  X2 in S1,
								intset( TX2, 1, 5 ), #( TX2, 1 ), X2 in TX2,
								
								X3 #:: [ 1 .. 6 ], X3 notin S1,
								intset( TX3, 1, 6 ), #( TX3, 1 ), X3 in TX3,
								
								S2 = (( S1 \ TX2 ) \/ TX3),
							 */
							int maxTag = (int)Math.pow(2, cache.getTagBitLength() ) - 1;
							ecl
							.append( "% вытесняемый тег").append( eoln )
							.append( vytesnTag ).append( " in " ).append( currentSetVar ).append( "," ).append( eoln )
							.append( "intset( " ).append( vytesnTagSet ).append( ", 0, ").append( maxTag ).append( " ), " )
							.append( "#( " ).append( vytesnTagSet ).append( ", 1 ), ")
							.append( vytesnTag ).append( " in " ).append( vytesnTagSet ).append( "," ).append( eoln )
							.append( eoln )
							
							.append( "% тег - причина промаха").append( eoln )
							.append( tag ).append( " #:: [ 0 .. ").append( maxTag ).append(" ], " )
							.append( tag ).append( " notin " ).append( currentSetVar ).append( "," ).append( eoln )
							.append( "intset( " ).append( tagset ).append( ", 0, ").append( maxTag ).append( " ), " )
							.append( "#( " ).append( tagset ).append( ", 1 ), ")
							.append( tag ).append( " in " ).append( tagset ).append( "," ).append( eoln )
							.append( eoln );
							
							ecl.append( nextSetVar ).append( " = (( " ).append( currentSetVar ).append( " \\ " ).append( vytesnTagSet ).append(" ) \\/ ").append( tagset ).append( ")," ).append( eoln )
							.append( eoln );

							// increment version of set
							setsForThisCacheLevel.put( setNumber, currentSetVersion + 1 );
							
							collector.upgradeSet( nextSetVar, vytesnTag );
							collector.addHit( tag );
						}
						else
							continue;
					}
				}
			}
		}
		
		for( String var : changedVars )
		{
			Integer version = varVersions.get( var );
			varVersions.put( var, version + 1 );
		}
		return prog.eclipseProgram;
    }

    private synchronized static List<String> calculateTags( final VarsController tagVersions, final int cacheLevelsCount, final Map<String, Set<ProcedureTestSituation>> testSituationParameters )
    {
    	List<String> result = new ArrayList<String>( );
    	for( int i = 0; i < cacheLevelsCount; i++ )
    	{
    		result.add( null );
    	}
    	Set<ProcedureTestSituation> procTS;
    	if ( testSituationParameters.containsKey( "LoadMemory" ) )
    		procTS = testSituationParameters.get( "LoadMemory" );
    	else if ( testSituationParameters.containsKey( "StoreMemory" ) )
    		procTS = testSituationParameters.get( "StoreMemory" );
    	else
    		return null;
    	
    	for( ProcedureTestSituation ts : procTS )
    	{
    		int level;
    		String tagVar;
    		
    		if ( ts instanceof CacheHit )
    		{
    			CacheHit hit = (CacheHit)ts;
    			level = hit.getLevel();
    			tagVar = hit.getTagVar();
    		}
    		else if ( ts instanceof CacheMiss )
    		{
    			CacheMiss miss = (CacheMiss)ts;
    			level = miss.getLevel();
    			tagVar = miss.getTagVar();
    		}
    		else
    			continue;
    		
			if ( level < 0 || level > cacheLevelsCount )
				throw new SemanticException( null, "level mustn't be less than 0 and more then cache levels count( " + cacheLevelsCount + " )");
    		if ( result.get( level - 1 ) != null )
				throw new SemanticException( null, "level #" + level + " has been already defined" );
			
    		if ( tagVar == null )
			{
				tagVar = tagVersions.newVar().toString();
			}
    		else
    		{
    			//TODO проверить, что это имя не использовано уже в схеме
    			tagVar = "_" + tagVar;
    		}

    		result.set( level - 1, tagVar );
    	}
    	
    	for( int i = 0; i < cacheLevelsCount; i++ )
    	{
    		if ( result.get( i ) == null )
    		{
				String tagVar = tagVersions.newVar().toString();
				result.set( i, tagVar );
    		}
    	}
    	
    	return result;
    }
    
    private static String getSetVar( int cacheLevel, int setNumber )
    {
    	return "Level" + cacheLevel + "Set" + setNumber;
    }
    
    private static String getIniTagVar( int cacheLevel, int setNumber, int tagNumber )
    {
    	return getSetVar( cacheLevel, setNumber ) + "IniTag" + tagNumber;
    }
    
    private static String getIniProgLenVar( int cacheLevel, int setNumber )
    {
    	return getSetVar( cacheLevel, setNumber ) + "InitLen";
    }
    
    private static String getIniSetVar( int cacheLevel, int setNumber )
    {
    	return getSetVar( cacheLevel, setNumber ) + "Initial";
    }
    
    private static String getIniTagsOfSet( int cacheLevel, int setNumber )
    {
    	return getSetVar( cacheLevel, setNumber ) + "IniTags";
    }
}