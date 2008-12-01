package ru.teslaprj;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
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
			CompoundTerm result = callECLiPSe( tmp, moduleName, scheme.getDefinedNames(), cacheState );
			
			// 3. analyze results
			return generateValues( result, scheme, cacheState );
//			return new Verdict(new HashMap<Definition, BigInteger>(), null );
		}
		finally
		{
			tmp.delete();
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
	
    private CompoundTerm callECLiPSe( 
    		  final File eclipseProgram
    		, final String moduleName
    		, final List<String> names
    		, final List<Cache> cacheLevels
    	) 
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
	        goal.append( moduleName ).append( ":go(_, Caches" );
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

    private Verdict generateValues( 
    		  final CompoundTerm result
    		, final Scheme scheme
    		, final List<Cache> cacheLevels
    	)
    {
    	Map<Definition, BigInteger> values = new HashMap<Definition, BigInteger>();
    	List< Map<Long,List<Long>> > caches = new ArrayList<Map<Long,List<Long>>>();

    	int defNumber = 2;
    	@SuppressWarnings("unchecked")
    	List<List<CompoundTerm>> cacheResults = (List<List<CompoundTerm>>)result.arg( defNumber++ );
    	int cacheLevel = 0;
    	for( List<CompoundTerm> setsForCacheLevel : cacheResults )
    	{
    		Map< Long, List<Long> > sets = new HashMap<Long, List<Long>>();
    		int setSize = cacheLevels.get( cacheLevel ++ ).getSectionNumber();
    		for( CompoundTerm set : setsForCacheLevel )
    		{
    			Object setNumber = set.arg(1);
    			Long longSetNumber;
				if ( setNumber instanceof Integer )
					longSetNumber = (long)((Integer)setNumber).intValue();
				else if ( setNumber instanceof Long )
					longSetNumber = (Long)setNumber;
				else
					throw new Error( "unexpected eclipse behaviour: unknown type " + setNumber.getClass() );

				@SuppressWarnings( "unchecked" )
				List<CompoundTerm> hits = (List<CompoundTerm>)set.arg(3);
    			List<Long> tags = new ArrayList<Long>();
    			for( int i = 0; i < setSize; i++ )
    			{
    				Object tag = hits.get( i ).arg(1);
    				if ( tag instanceof Integer )
    					tags.add( (long)((Integer)tag).intValue() );
    				else if ( tag instanceof Long )
    					tags.add( (Long)tag );
    				else
    					throw new Error( "unexpected eclipse behaviour: unknown type " + tag.getClass() );
    			}
    			sets.put( longSetNumber, tags );
    		}
    		caches.add( sets );
    	}
        
		for( Definition def : scheme.getDefinitions() )
		{
			List<?> arg = (List<?>)result.arg( defNumber++ );
			values.put( def, BitLen.intlist2bigint( arg ) );
		}
		
		return new Verdict( values, caches );
    }

    List<String> vytesnTags;
    List<String> hitTags;
    List<String> missTags;
    
    private StringBuffer translate( 
    		  final Scheme scheme
    		, final String moduleName
    		, final List<Cache> cacheLevels
    	)
    	throws SchemeDefinitionError, IOException, RecognitionException
    {
    	/** command +> ( cacheLevelNumber +> setVar ) */
		Map<Command, Map<Integer, String> > commandsSetNumbers = new HashMap<Command, Map<Integer,String>>() ;



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
		ecl.append( ":- export go/" ).append( names.size() + 2 ).append( "." ).append(eoln).append(eoln);
    	ecl.append( "go( _, Caches" );
    	// initial states of sets
		for( String name : names )
		{
			ecl.append( ", _0" + name );
		}
    	ecl.append( " ) :-" ).append( eoln )
    	.append( eoln );

//    	for( Cache cache : allSets.keySet() )
//    	{
//    		int maxTagValue = (int)Math.pow(2, cache.getTagBitLength() ) - 1;
//    		List<Integer> sets = allSets.get( cache );
//    		int tagsCount = cache.getSectionNumber();
//    		for( int setNumber : sets )
//    		{
//    			ecl
//    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, setNumber) )
//    			.append( " = [ ").append( getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, 1 ) );
//    			for( int k = 2; k <= tagsCount; k++ )
//    			{
//    				ecl.append( ", " ).append( getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, k ) );
//    			}
//    			ecl.append( " ]," ).append( eoln )
//    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, setNumber) )
//    			.append( " #:: [ 0 .. ").append( maxTagValue ).append( " ],").append( eoln )
//    			.append( "ic_global:alldifferent( " ).append( getSetVar( cacheLevels.indexOf( cache ) + 1, setNumber) ).append( " )," ).append( eoln );
//
//    			for( int k = 1; k <= tagsCount; k++ )
//    			{
//    				String tag = getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, k );
//					ecl.append( "intset( " ).append( tag ).append( "set, 0, " ).append( (int)Math.pow(2, cache.getTagBitLength() ) - 1 ).append( " ), " )
//					.append( "#( " ).append( tag ).append( "set, 1 ), " )
//					.append( tag ).append( " in " ).append( tag ).append( "set," ).append( eoln );
//    			}
//    			
//    			ecl.append( eoln )
//    			.append( getSetVar( cacheLevels.indexOf( cache ) + 1, setNumber) )
//    			.append( "_0 = []");
//    			
//    			for( int k = 1; k <= tagsCount; k++ )
//    			{
//    				String tag = getIniTagVar( cacheLevels.indexOf( cache ) + 1, setNumber, k );
//					ecl.append( " \\/ " ).append( tag ).append( "set" );
//    			}
//    			
//    			ecl.append( "," ).append( eoln )
//    			.append( eoln );
//    		}
//    	}
    	
    	Map<String, Integer> varVersions = new HashMap<String, Integer>();
    	for( String name : names )
    	{
    		varVersions.put( name, 0 );
    	}
    	
    	VarsController tagNames = new VarsController();    	
    	
    	//////// TODO SETs PROBLEM
    	// 1. assign set variables to commands if command is memory operation
    	Set<String> allSetVars = new HashSet<String>();
    	for( Command cmd : scheme.getCommands() )
    	{
    		if ( commandIsMemoryOperation( cmd, scheme, cacheLevels ) )
    		{
    			Map<Integer, String> sets = new HashMap<Integer, String>();
    			for( int level = 1; level <= cacheLevels.size(); level++ )
    			{
    				String set = tagNames.newVar().toString();
    				sets.put( level, set );
    				allSetVars.add( set );
    				ecl.append( set ).append( " #:: [ 0 .. ")
    					.append( (int)Math.pow(2.0, cacheLevels.get(level - 1).getSetNumberBitLength() ) - 1 )
    					.append( " ]," ).append( eoln );
    			}
				commandsSetNumbers.put( cmd, sets );
    		}
    	}
    	// 2. derive set constrains (=,#)
    	// 3. generate prolog code for constraints ( =, #, labeling)
    	for( String set : allSetVars )
    	{
    		ecl.append( "indomain( " ).append( set ).append( " ),").append( eoln );
    	}
    	ecl.append( eoln );
    	
    	/////// инициализация
    	StringBuffer cacheslist = new StringBuffer( "Caches" );
    	for( int level = 1; level <= cacheLevels.size(); level++ )
    	{
    		ecl.append( "CurrentSetsOfLevel" ).append( level ).append( " = _ ,").append( eoln );
        	ecl.append( cacheslist )
        		.append( " = [ CurrentSetsOfLevel" ).append( level ).append( " | ");
        	cacheslist = tagNames.newVar();
        	ecl.append( cacheslist ).append( " ]," ).append( eoln );
    	}
    	ecl.append( cacheslist ).append( " = []," ).append( eoln );
    	
    	List<StringBuffer> initialTagLists = new ArrayList<StringBuffer>();
		for( Command cmd : scheme.getCommands() )
		{
			Map<Integer, String> sets = commandsSetNumbers.get( cmd );
			if ( sets != null ) 
			{
				// this is a 'memory instruction'
				for( Integer level : sets.keySet() )
				{
					String setNumberVar = sets.get( level );
					int Max = (int)Math.pow(2, cacheLevels.get( level - 1 ).getTagBitLength() ) - 1;

					List<StringBuffer> tags = new ArrayList<StringBuffer>();
					StringBuffer list = tagNames.newVar();
					StringBuffer setlist = list;
					StringBuffer list2 = tagNames.newVar();
					StringBuffer tagsetlist = list2;
					for( int setPosition = 0; setPosition < cacheLevels.get( level - 1 ).getSectionNumber(); setPosition++ )
					{
						StringBuffer tag = tagNames.newVar();
						tags.add( tag );
						ecl
						.append( tag ).append( " #:: [ 0 .. " ).append( Max ).append( " ], " )
						.append( "intset( " )
							.append( tag ).append( "set, " )
							.append( "0, " )
							.append( Max )
						.append( " ), " )
						.append( "#( " ).append( tag ).append( "set, 1 ), " )
						.append( tag ).append( " in " ).append( tag ).append( "set," ).append( eoln )
						.append( list ).append( " = [ " ).append( tag ).append( " | " );
						list = tagNames.newVar();
						ecl.append( list ).append( " ]," ).append( eoln )
						.append( list2 ).append( " = [ " ).append( tag ).append( "set | " );
						list2 = tagNames.newVar();
						ecl.append( list2 ).append( " ]," ).append( eoln );
					}
					ecl.append( list ).append( " = []," ).append( eoln );
					ecl.append( list2 ).append( " = []," ).append( eoln );
					ecl.append( "ic_global:alldifferent( ").append( setlist ).append( " )," ).append( eoln );
					initialTagLists.add( setlist );
					
					// 'setVar' is var for intset with initial values of set
					StringBuffer setVar = tagNames.newVar();
					ecl
					.append( "intset( " )
						.append( setVar )
						.append( ", 0, " )
						.append( Max )
					.append( " ), " )
					
					.append( "#( " )
						.append( setVar ).append( ", " )
						.append( tags.size() )
					.append( " )," ).append( eoln );
					
					for( StringBuffer tag : tags )
					{
						ecl.append( tag ).append(" in " ).append( setVar ).append( ", " ).append( eoln );
					}
					
					ecl
					.append( "lru:initialize( " ) // set initial state of 'set' if it is not set yet
						.append( setNumberVar ).append( ", " ) // in runtime 'setVar' is int constant!
						.append( setVar ).append( ", " )
						.append( "CurrentSetsOfLevel" ).append( level ).append( ", " )
						.append( setlist ).append( ", " )
						.append( tagsetlist )
					.append( " )," ).append( eoln );
				}
			}
		}
    	ecl.append( eoln );
    	
    	////////// assert's
    	for( Assert asert : scheme.getAsserts() )
    		commandPredicates.append( 
    				commandlikeTranslate( 
    						  asert
    						, scheme
    						, varVersions
    						, ecl
    						, null
    						, cacheLevels
    						, null
    					)
    			);

    	currentSetVersions = new HashMap<Cache, Map<Integer,Integer>>();
    	
    	for( int i = 0; i < cacheLevels.size(); i++ )
    	{
    		Cache cache = cacheLevels.get(i);
    		currentSetVersions.put( cache, new HashMap<Integer, Integer>() );
    	}
   
        vytesnTags = new ArrayList<String>();
        hitTags = new ArrayList<String>();
        missTags = new ArrayList<String>();

        ///////////// команды
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
    			) );
    	}
    	
    	ecl.append( eoln );
    	
    	//////////// LRU
    	ecl.append( "% LRU predicates" ).append( eoln );
    	int level = 1;
    	for( Cache cache : cacheLevels )
    	{
    		ecl.append( "lru:vytesnTagsLRU( " )
    			.append( "CurrentSetsOfLevel" ).append( level++ ).append( ", " )
    			.append( cache.getSectionNumber() )
    		.append( ")," ).append( eoln );
    	}
    	ecl.append( eoln );

		/////////////// labeling
    	// 1. выбор вытесняемых тегов
    	ecl.append( "%вытесненные" ).append( eoln );
    	for( String vt : vytesnTags )
    	{
		    ecl.append( "indomain( " ).append( vt ).append( " )," ).append( eoln );
    	}
    	vytesnTags = null;
    	
    	// 2. выбор hit-тегов
    	ecl.append( "%hit" ).append( eoln );
    	for( String ht : hitTags )
    	{
		    ecl.append( "indomain( " ).append( ht ).append( " )," ).append( eoln );
    	}
    	hitTags = null;
    	
    	// 3. выбор miss тегов
    	ecl.append( "%miss" ).append( eoln );
    	for( String mt : missTags )
    	{
		    ecl.append( "indomain( " ).append( mt ).append( " )," ).append( eoln );
    	}
    	missTags = null;
    	
    	// 4. labeling( Seti_0 ) initial sets
    	for( StringBuffer ist : initialTagLists )
    	{
    		ecl.append( "labeling( " ).append( ist ).append( " )," ).append( eoln );
    	}
    	initialTagLists = null;
    	
    	// 5. random_result( var )
		for( String name : names )
		{
			ecl	.append( "numbers:random_result( _0" ).append( name ).append( " )," )
				.append( eoln );
		}
	
		ecl.append( "true." ).append( eoln )
		.append( eoln )
		.append( commandPredicates );

    	return ecl;
    }
    
//    private static Map<Cache, List<Integer>> calculateSets(
//			Map<Command, Map<Cache, Integer>> commandsSetNumbers)
//	{
//    	Map<Cache, List<Integer>> result = new HashMap<Cache, List<Integer>>();   	
//    	
//    	for( Command command : commandsSetNumbers.keySet() )
//    	{
//    		Map<Cache, Integer> setsForCommand = commandsSetNumbers.get( command );
//    		for( Cache cache : setsForCommand.keySet() )
//    		{
//    			if ( result.containsKey( cache ) )
//    			{
//    				// add setsForCommand.get( cache ) to the 'cache tagset'
//    				List<Integer> sets = result.get( cache );
//    				if ( ! sets.contains( setsForCommand.get( cache ) ) )
//    				{
//	    				sets.add( setsForCommand.get( cache ) );
//	    				result.put( cache, sets );
//    				}
//    			}
//    			else
//    			{
//    				// create new 'cache tagset'
//    				List<Integer> sets = new ArrayList<Integer>();
//    				sets.add( setsForCommand.get( cache ) );
//    				result.put( cache, sets );
//    			}
//    		}
//    	}
//    	
//		return result;
//	}

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

    private boolean commandIsMemoryOperation (
    		  final Commandlike command
    	    , final Scheme scheme
    	    , List<Cache> cacheLevels
    	 )
    throws IOException, RecognitionException
    {
    	String pathToTSL = sourcePath + "\\" + command.getCop() + "\\" + command.getTestSituation() + ".tsl";
    	CharStream inputStream = new ANTLRFileStream( pathToTSL );
    	TeSLaLexer lexer = new TeSLaLexer( inputStream );
    	CommonTokenStream tokens = new CommonTokenStream(lexer);
    	TeSLaParser parser = new TeSLaParser(tokens);
    	TeSLaParser.program_return prog = parser.program( command.getArgs(), command.getAdditionalArgs(), scheme, "", cacheLevels );
    	return prog.hasMemoryOperation;
    }
    
    /**
     * возвращает дополнительные предикаты, необходимые команде
     * обладает побочным эффектом: меняет ecl
     * 
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
    		, Map<Integer,String> setNumVars
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
		ecl.append( ")," ).append( eoln );
		
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
							
							// HIT for 'tag' in 'cache'
							
							StringBuffer setVar = tagsVersions.newVar();
							String setVarsStructure = "CurrentSetsOfLevel" + hit.getLevel();
//							String hitsStructure = "HitsOfLevel" + hit.getLevel();
							StringBuffer hitSetsStructure = tagsVersions.newVar();
//							String setVersionsStructure = "SetVersionsOfLevel" + hit.getLevel();
							String setNumber = setNumVars.get( hit.getLevel() );
							
							ecl.append( "lru:latestSetVar( " )
								.append( setVar ).append( ", " )
								.append( hitSetsStructure ).append( ", " )
								.append( "_, " )
								.append( setVarsStructure ).append( ", " )
								.append( setNumber )
							.append( " )," ).append( eoln )
							.append( tag ).append( " in " ).append( setVar ).append( "," ).append( eoln )
							.append( "intset( " ).append( tagset ).append( ", 0, " ).append( (int)Math.pow(2, cache.getTagBitLength() ) - 1 ).append( " ), " )
							.append( "#( " ).append( tagset ).append( ", 1 ), " )
							.append( tag ).append( " in " ).append( tagset ).append( "," ).append( eoln )
							
							.append( "lru:addHit( ")
								.append( tag ).append( ", " )
								.append( tag ).append( "set, " )
								.append( setVar ).append( ", " )
								.append( hitSetsStructure )
							.append( " )," ).append( eoln )
							
//							.append( "lru:addSet( ").append( setVar ).append( ", " ).append( setVersionsStructure ).append( ", " ).append( setNumber ).append( " )," ).append( eoln )
							.append( eoln );

						    hitTags.add( tag );
						}
						else if ( ts instanceof CacheMiss )
						{
							CacheMiss miss = (CacheMiss)ts;
							Cache cache = cacheLevels.get( miss.getLevel() - 1 );
							String tag = tags.get( miss.getLevel() - 1);
							String tagset = tag + "set";

							// MISS for 'tag' in 'cache
							String vytesnTag = miss.getVTagVar();
							if ( vytesnTag == null )
							{
								vytesnTag = tagsVersions.newVar().toString();
							}
							String vytesnTagSet = vytesnTag + "set";

							/*	X2 = vytesnTag, X3 = tag
							 *  X2 in S1,
								intset( TX2, 1, 5 ), #( TX2, 1 ), X2 in TX2,
								
								X3 #:: [ 1 .. 6 ], X3 notin S1,
								intset( TX3, 1, 6 ), #( TX3, 1 ), X3 in TX3,
								
								S2 = (( S1 \ TX2 ) \/ TX3),
							 */
							
							StringBuffer latestSetVar = tagsVersions.newVar();
							String setNumber = setNumVars.get( miss.getLevel() );
							String setVarsStructure = "CurrentSetsOfLevel" + miss.getLevel();
							StringBuffer hitSetsStructure = tagsVersions.newVar();
							StringBuffer vytesnStructure = tagsVersions.newVar();
							
							int maxTag = (int)Math.pow(2, cache.getTagBitLength() ) - 1;
							ecl
							.append( "lru:latestSetVar( " )
								.append( latestSetVar ).append( ", " )
								.append( hitSetsStructure ).append( ", " )
								.append( vytesnStructure ).append( ", " )
								.append( setVarsStructure ).append ( ", " )
								.append( setNumber )
							.append( " )," ).append( eoln )
							
							.append( "% вытесняемый тег").append( eoln )
							.append( vytesnTag ).append( " in " ).append( latestSetVar ).append( "," ).append( eoln );
							
							StringBuffer latestNHits = tagsVersions.newVar();
							
							ecl.append( "lru:latestNHits( ")
								.append( latestNHits ).append( ", " )
								.append( cache.getSectionNumber() - 1 ).append( ", " )
								.append( hitSetsStructure )
							.append( ")," ).append( eoln )
							.append( "( foreach( NH, " ).append( latestNHits ).append( " ),").append( eoln )
								.append( "  param( " ).append( vytesnTag ).append( " )").append( eoln )
								.append( "do" ).append( eoln )
								.append( vytesnTag ).append( " #\\= NH" ).append( eoln )
							.append( ")," ).append( eoln );
							
//							ecl
//							.append( "intset( " )
//								.append( vytesnTagSet )
//								.append( ", 0, ")
//								.append( maxTag )
//								.append( " ), " )
//							.append( "#( " ).append( vytesnTagSet ).append( ", 1 ), ")
//							.append( vytesnTag ).append( " in " ).append( vytesnTagSet ).append( "," ).append( eoln );
							
//							String vytesnTagsStructure = "VytesnTagsOfLevel" + miss.getLevel();
//							String vytesnTagSetsStructure = "VytesnTagSetsOfLevel" + miss.getLevel();
//							String vytesnTagIdxsStructure = "VytesnTagIdxsOfLevel" + miss.getLevel();

							ecl
							.append( "lru:addVytesnTag( ")
								.append( vytesnTag ).append( ", " )
								.append( latestSetVar ).append( ", " )
								.append( hitSetsStructure ).append( ", " )
								.append( vytesnStructure )
							.append( ")," ).append( eoln )
							
							.append( "intset( " ).append( vytesnTag ).append( "set, 0, " ).append( maxTag ).append( " )," )
							.append( "#( " ).append( vytesnTag ).append( "set, 1 )," )
							.append( vytesnTag ).append( " in " ).append( vytesnTag ).append( "set," ).append( eoln )
							
//							.append( "lru:addVytesnTagSet( ")
//								.append( vytesnTagSet ).append( ", " )
//								.append( vytesnTagSetsStructure ).append( ", " )
//								.append( setNumber )
//							.append( ")," ).append( eoln );
//							
//							StringBuffer hitsCount = tagsVersions.newVar();
//							ecl.append( "lru:hitsCount( " )
//								.append( hitsCount ).append( ", " )
//								.append( hitsStructure ).append( ", " )
//								.append( setNumber )
//							.append( ")," ).append( eoln )
//
//							.append( "lru:addVytesnTagIdx( ")
//								.append( hitsCount ).append( ", " )
//								.append( vytesnTagIdxsStructure ).append( ", " )
//								.append( setNumber )
//							.append( ")," ).append( eoln )

							.append( eoln )
							
							.append( "% тег - причина промаха").append( eoln )
							.append( tag ).append( " #:: [ 0 .. ").append( maxTag ).append(" ], " )
							.append( tag ).append( " notin " ).append( latestSetVar ).append( "," ).append( eoln )
							.append( "intset( " ).append( tagset ).append( ", 0, ").append( maxTag ).append( " ), " )
							.append( "#( " ).append( tagset ).append( ", 1 ), ")
							.append( tag ).append( " in " ).append( tagset ).append( "," ).append( eoln )
							.append( eoln );
	
							StringBuffer nextSetVar = tagsVersions.newVar();
							ecl.append( nextSetVar ).append( " = (( " )
								.append( latestSetVar ).append( " \\ " ).append( vytesnTagSet )
								.append(" ) \\/ ").append( tagset )
							.append( ")," ).append( eoln )
							.append( "lru:addSetVar( " )
								.append( nextSetVar ).append( ", " )
								.append( setVarsStructure ).append( ", " )
								.append( setNumber )
							.append( ")," ).append( eoln )
							.append( "lru:addHit( " )
								.append( tag ).append( ", " )
								.append( tag ).append( "set, " )
								.append( nextSetVar ).append( ", " )
								.append( hitSetsStructure )
							.append( ")," ).append( eoln )
							.append( eoln );

							vytesnTags.add( vytesnTag );
						    missTags.add( tag );
						}
						else
							continue;
					}
				}
			}
		}
		
		ecl.append( eoln );
		
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
    
//    private static String getSetVar( int cacheLevel, int setNumber )
//    {
//    	return "Level" + cacheLevel + "Set" + setNumber;
//    }
//    
//    private static String getIniTagVar( int cacheLevel, int setNumber, int tagNumber )
//    {
//    	return getSetVar( cacheLevel, setNumber ) + "IniTag" + tagNumber;
//    }
//    
//    private static String getIniProgLenVar( int cacheLevel, int setNumber )
//    {
//    	return getSetVar( cacheLevel, setNumber ) + "InitLen";
//    }
//    
//    private static String getIniSetVar( int cacheLevel, int setNumber )
//    {
//    	return getSetVar( cacheLevel, setNumber ) + "Initial";
//    }
//    
//    private static String getIniTagsOfSet( int cacheLevel, int setNumber )
//    {
//    	return getSetVar( cacheLevel, setNumber ) + "IniTags";
//    }
}