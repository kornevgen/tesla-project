package ru.teslaprj;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import ru.teslaprj.constraints.Argument;
import ru.teslaprj.constraints.ArgumentsManager;
import ru.teslaprj.constraints.Constraint;
import ru.teslaprj.constraints.ConstraintManager;
import ru.teslaprj.constraints.Constraint.Relation;
import ru.teslaprj.constraints.args.PhysicalAddressAfterTranslation;
import ru.teslaprj.constraints.args.Tag;
import ru.teslaprj.constraints.args.VirtualAddress;
import ru.teslaprj.scheme.Assert;
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
import ru.teslaprj.syntax.TeSLaParser.MemoryCommand;

import com.parctechnologies.eclipse.CompoundTerm;
import com.parctechnologies.eclipse.EclipseEngine;
import com.parctechnologies.eclipse.EclipseEngineOptions;
import com.parctechnologies.eclipse.EclipseException;
import com.parctechnologies.eclipse.EmbeddedEclipse;

public class Solver
{
	public static final String eoln = System.getProperty("line.separator");
	
	private File sourcePath;
	private File libPath;
	
	/**
	 * @param sourcePath	директория с описаниями тестовых ситуаций
	 * @param libPath		директория с clp-модулями (numbers.ecl, predicates.ecl)
	 */
	public Solver( File sourcePath, File libPath )
	{
		this.sourcePath = sourcePath;
		this.libPath = libPath;
		
		if ( ! System.getProperties().containsKey( "eclipse.directory" ) )
		{
			throw new Error("variable 'eclipse.directory' is not set");
		}
		
		if ( ! new File(System.getProperty("eclipse.directory") ).exists() )
		{
			throw new Error( "folder 'eclipse.directory' doesn't exist" );
		}
	}
	
	public Verdict solve( Scheme scheme, List<Cache> cacheState, TLB tlb )
		throws SchemeDefinitionError, IOException, EclipseException, RecognitionException
	{
		// 0. check correctness of the `scheme`
		checkVarsKnowness( scheme );
		
		if ( cacheState != null )
		{
			for( Cache cache : cacheState )
			{			
				if ( cache.getTagBitLength() > BitLen.WORD_VALUE )
				{
					throw new SemanticException( null, "operations with addresses more than " + BitLen.WORD_VALUE + " bits are not implemented yet");
				}
			}
		}
		
		// 1. translate `scheme` to .clp
		String moduleName = createTempModuleName();
		File tmp = File.createTempFile( moduleName, ".ecl", libPath );
		moduleName = tmp.getName();
		moduleName = moduleName.substring(0, moduleName.length() - 4 );
		
		if ( cacheState == null )
		{
			cacheState = new ArrayList<Cache>();
		}
		
		if ( tlb != null )
		{
			for( Cache cache : cacheState )
			{
				if ( cache.getAddressBitLength() != tlb.getPhysicalAddressBitLen() )
				{
					throw new SemanticException( null, "Size of physical address in tlb must be equal with one in caches" );
				}
			}
		}

		try
		{
	    	writeFile( tmp, translate( scheme, moduleName, cacheState, tlb ) );
//	    	translate( scheme, moduleName, cacheState, tlb );
			
			// 2. run ECLiPSe and get results
			CompoundTerm result = callECLiPSe( tmp, moduleName, scheme.getDefinedNames(), cacheState );
			
			// 3. analyze results
			return generateValues( result, scheme, cacheState ) ;
//			return new Verdict(new HashMap<Definition, BigInteger>(), null );
		}
		finally
		{
			tmp.delete();
		}
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
	        goal.append( moduleName ).append( ":go(_, Caches, TLB, Memory, TLBIndexes" );
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

    private StringBuffer translate( 
    		  final Scheme scheme
    		, final String moduleName
    		, final List<Cache> cacheLevels
    		, final TLB tlb
    	)
    	throws SchemeDefinitionError, IOException, RecognitionException
    {
    	StringBuffer ecl = new StringBuffer();
		
		ecl.append(":- module( " ).append( moduleName ).append( " )." ).append(eoln);
		ecl.append(":- lib( ic ).").append(eoln);
		ecl.append(":- lib( ic_sets ).").append(eoln);
		ecl.append(":- use_module( lru ).").append(eoln);
		ecl.append(":- use_module( numbers ).").append(eoln);
		ecl.append(":- use_module( predicates ).").append(eoln);
		ecl.append(":- use_module( tlb ).").append( eoln );
		ecl.append(eoln);

		StringBuffer commandPredicates = new StringBuffer();
    	
    	// сгенерировать предикат go: последовательность вызовов унифицированных "предикатов go"
		List<String> names = scheme.getDefinedNames();
		ecl.append( ":- export go/" ).append( names.size() + 5 ).append( "." ).append(eoln).append(eoln);
    	ecl.append( "go( _, Caches, TLB, Memory, TLBIndexes" );
    	// initial states of sets
		for( String name : names )
		{
			ecl.append( ", _0" + name );
		}
    	ecl.append( " ) :-" ).append( eoln )
    	.append( eoln );
    	
    	/////// 0. Virtual Addresses constrains
    	// 1. virtual addresses constraints
    	// translate test situations: AddressTranslation()->new virtual address var, LoadMemory()->null?
    	Map<String, Integer> varVersions = new LinkedHashMap<String, Integer>();
    	for( String name : names )
    	{
    		varVersions.put( name, 0 );
    	}
    	Map<Command, String> falseVirtualAddresses = new HashMap<Command, String>();
    	VarsController tagNames = new VarsController();

    	Set<Command> storeCommands = new HashSet<Command>();
    	Set<Command> dataTlbCommands = new HashSet<Command>();

    	for( Command command : scheme.getCommands() )
    	{
    		commandPredicates.append( falseCommandlikeTranslate( 
    				  command
    				, scheme
    				, varVersions
    				, ecl
    				, tagNames
    				, cacheLevels
    				, tlb
    				, storeCommands
    				, dataTlbCommands
    				, falseVirtualAddresses
    			) );
    	}
    	
    	// 2. false virtual addresses differences vars + labeling
    	Map<String, List<Command>> diffFalseVars = new HashMap<String, List<Command>>();
    	Collection<Command> viewed_cmds = new HashSet<Command>();
    	if ( tlb != null )
    	{
	    	for( Command cmd_i : falseVirtualAddresses.keySet() )
	    	{
	    		viewed_cmds.add(cmd_i);
	    		String vAddr_i = falseVirtualAddresses.get( cmd_i );
	    		for( Command cmd_j : falseVirtualAddresses.keySet() )
	    		{
	    			if ( viewed_cmds.contains(cmd_i) )
	    				continue;
	    			
	    			String vAddr_j = falseVirtualAddresses.get( cmd_j );
	
	    			// generate var for vAddr_i == vAddr_j
	    			String diffVar = tagNames.newVar().toString();
	    			ecl.append( diffVar + " #::[0..1]," ).append( eoln );
	    			ecl.append( diffVar + " #= ( " 
	    					+ vAddr_i + " #= " + vAddr_j + " )," + eoln );
	    			diffFalseVars.put( diffVar, Arrays.asList(cmd_i, cmd_j) );
	    		}
	    	}
	    	
	    	for( String diffVar : diffFalseVars.keySet() )
	    	{
	    		ecl.append( "indomain( " + diffVar + " )," + eoln );
	    	}
	    	ecl.append( eoln );
    	}
    	
    	// collect commands successfully worked with tlb
    	Set<Command> tlbCommands = new HashSet<Command>();
    	for( Command cmd : falseVirtualAddresses.keySet() )
    	{
    		if ( cmd.getTLBSituation() instanceof TLBExists )
    		{
    			tlbCommands.add( cmd );
    		}
    	}
    	
    	
    	// 3. to TLB buffer lines
    	Map<Command, String> tlbBufferIndexes = new HashMap<Command, String>();
    	Map<Command, String> tlbBufferVytesnIndexes = new HashMap<Command, String>();
    	if ( tlb != null )
    	{
	    	ecl.append( "[ 1" );
	    	for( Command cmd : tlbCommands )
	    	{
				String index = tagNames.newVar().toString();
				tlbBufferIndexes.put( cmd, index );
				ecl.append( ", " ).append( index );
				// fill tlbBufferVytesnIndexes
				if ( cmd.getTLBSituation() instanceof TLBMiss )
				{
					String vindex = tagNames.newVar().toString();
					tlbBufferVytesnIndexes.put( cmd, vindex );
	    			ecl.append( ", " ).append( vindex );
				}
	    	}
	    	ecl.append( "] #:: [ 1 .. " ).append( tlb.getSize() ).append(" ],").append( eoln );

	    	// i1.g != i2.g => i1 != i2
	    	viewed_cmds.clear();
	    	for( Command cmd1 : tlbCommands )
	    	{
	    		viewed_cmds.add( cmd1 );
	    		if ( ((TLBExists)cmd1.getTLBSituation()).getG() == null )
	    			continue;
	    		for( Command cmd2 : tlbCommands )
	    		{
	    			if ( viewed_cmds.contains( cmd2 ) )
	    				continue;
	    			if ( ((TLBExists)cmd2.getTLBSituation()).getG() == null )
	    				continue;
	    			if ( ((TLBExists)cmd1.getTLBSituation()).getG().intValue() 
	    					!= ((TLBExists)cmd2.getTLBSituation()).getG().intValue() )
	    			{
	    				ecl.append( tlbBufferIndexes.get(cmd1) ).append( " #\\= " )
	    				.append( tlbBufferIndexes.get(cmd2) ).append( "," ).append( eoln );
	    			}
	    		}
	    	}
	    	for( String diffVar : diffFalseVars.keySet() )
	    	{
	    		ecl.append( "( " + diffVar + " = 1 -> " );
	    		List<Command> diffCmds = diffFalseVars.get( diffVar );
	    		String tlb_i = tlbBufferIndexes.get( diffCmds.get(0));
	    		String tlb_j = tlbBufferIndexes.get( diffCmds.get(1));
	    		ecl.append( tlb_i + " #= " + tlb_j + " ; true )," + eoln );
	    	}
	    	ecl.append( eoln );
    	}
    	
    	// 4. TLB test situations (tlbHit, tlbMiss)
        List<String> vytesnTags = new ArrayList<String>();
        List<String> hitTags = new ArrayList<String>();
        List<String> missTags = new ArrayList<String>();
        
        String tlb_latestSetVar_DATA = tagNames.newVar().toString();
        if ( tlb != null )
        {
	        ecl.append( tlb_latestSetVar_DATA ).append( " = [] ");
	        for( int i = 1; i <= tlb.getBufferSize(); i++ )
	        {
	        	ecl.append( " \\/ [" + i + "] " );
	        }
	        ecl.append( "," ).append( eoln );
        }
        
        String tlb_latestSetVar_INSTRUCTION = tagNames.newVar().toString();
        if ( tlb != null )
        {
	        ecl.append( tlb_latestSetVar_INSTRUCTION ).append( " = [] ");
	        for( int i = 1; i <= tlb.getBufferSize(); i++ )
	        {
	        	ecl.append( " \\/ [" + i + "] " );
	        }
	        ecl.append( "," ).append( eoln );
        }
        
    	// add false 'DATA' first-level cache
    	if ( ! cacheLevels.isEmpty() )
    	{
    		final Cache cache1 = cacheLevels.get(0);
    		Cache new_cache = new Cache(){
				@Override
				public int getAddressBitLength() {
					return cache1.getAddressBitLength();
				}

				@Override
				public int getSectionNumber() {
					return cache1.getSectionNumber();
				}

				@Override
				public int getSetNumberBitLength() {
					return cache1.getSetNumberBitLength();
				}

				@Override
				public int getTagBitLength() {
					return cache1.getTagBitLength();
				}};
			cacheLevels.add(0, new_cache);
    	}
    	
    	// asserts on indexes from template
        // if virtual addresses are equal then tlb indexes are equal too !
    	ConstraintManager constraintManager = new ConstraintManager();
    	ArgumentsManager argManager = new ArgumentsManager();
    	//TODO предполагает, что cache0 уже добавлен!
    	readConstraintsFromTemplate(
    			scheme,
    			constraintManager,
    			argManager,
    			tagNames,
    			cacheLevels );
    	constraintManager.closeConstraints();

    	if ( tlb != null )
    	{
        	Map<List<Command>, Relation> virtualAddressesStaticConstraints =
        		constraintManager.getVirtualAddressesStaticConstraints();
	    	for( List<Command> c : virtualAddressesStaticConstraints.keySet() )
	    	{
	    		if ( virtualAddressesStaticConstraints.get(c) == Relation.EQ )
	    		{
	    			String tlb1 = tlbBufferIndexes.get(c.get(0));
	    			String tlb2 = tlbBufferIndexes.get(c.get(1));
	    			ecl.append( tlb1 + " #= " + tlb2 + eoln );
	    		}
	    	}

	        String hitsStructure_DATA = tagNames.newVar().toString();
	        ecl.append( hitsStructure_DATA ).append( " = _," ).append( eoln );
	        String vytesnStructure_DATA = tagNames.newVar().toString();
	        ecl.append( vytesnStructure_DATA ).append( " = _," ).append( eoln );

	        String hitsStructure_INSTRUCTION = tagNames.newVar().toString();
	        ecl.append( hitsStructure_INSTRUCTION ).append( " = _," ).append( eoln );
	        String vytesnStructure_INSTRUCTION = tagNames.newVar().toString();
	        ecl.append( vytesnStructure_INSTRUCTION ).append( " = _," ).append( eoln );
	    	
	        for( Command cmd : tlbCommands )
	    	{
	        	boolean isDATAinAddressTranslation = dataTlbCommands.contains(cmd);

	        	String s = tlbOperationTranslate(
	    				  cmd
	    				, tlb
	    				, tagNames
	    				, ecl
	    				, tlbBufferIndexes.get(cmd)
	    				, tlbBufferVytesnIndexes.get(cmd)
	    				, (isDATAinAddressTranslation? hitsStructure_DATA : hitsStructure_INSTRUCTION )
	    				, (isDATAinAddressTranslation? vytesnStructure_DATA : vytesnStructure_INSTRUCTION )
	    				, hitTags
	    				, missTags
	    				, vytesnTags
	    				, (isDATAinAddressTranslation? tlb_latestSetVar_DATA : tlb_latestSetVar_INSTRUCTION )
	    			);
	        	if ( isDATAinAddressTranslation )
	        	{
	        		tlb_latestSetVar_DATA = s;
	        	}
	        	else
	        	{
	        		tlb_latestSetVar_INSTRUCTION = s;	        		
	        	}
	    	}
	    	//////////// 4a. tlbLRU
	    	ecl.append( "% LRU predicates" ).append( eoln );

	    	StringBuffer tmpset_DATA = tagNames.newVar();
	    	ecl.append( "lru:makeSet( ")
	    		.append( tmpset_DATA ).append( ", " )
	    		.append( hitsStructure_DATA ).append( ", " )
	    		.append( vytesnStructure_DATA ).append( " )," ).append( eoln );
			ecl.append( "lru:vytesnTagsLRU( [ " )
				.append( tmpset_DATA ).append( " ], ")
				.append( tlb.getBufferSize() )
			.append( ")," ).append( eoln );
	    	ecl.append( eoln );
	
	    	StringBuffer tmpset_INSTRUCTION = tagNames.newVar();
	    	ecl.append( "lru:makeSet( ")
	    		.append( tmpset_INSTRUCTION ).append( ", " )
	    		.append( hitsStructure_INSTRUCTION ).append( ", " )
	    		.append( vytesnStructure_INSTRUCTION ).append( " )," ).append( eoln );
			ecl.append( "lru:vytesnTagsLRU( [ " )
				.append( tmpset_INSTRUCTION ).append( " ], ")
				.append( tlb.getBufferSize() )
			.append( ")," ).append( eoln );
	    	ecl.append( eoln );
	
			/////////////// 4b. tlb-labeling
	    	// выбор вытесняемых тегов
	    	ecl.append( "%вытесненные" ).append( eoln );
	    	for( String vt : vytesnTags )
	    	{
			    ecl.append( "indomain( " ).append( vt ).append( " )," ).append( eoln );
	    	}
	    	vytesnTags = null;
	    	
	    	// выбор hit-тегов
	    	ecl.append( "%hit" ).append( eoln );
	    	for( String ht : hitTags )
	    	{
			    ecl.append( "indomain( " ).append( ht ).append( " )," ).append( eoln );
	    	}
	    	hitTags = null;
	    	
	    	// выбор miss тегов
	    	ecl.append( "%miss" ).append( eoln );
	    	for( String mt : missTags )
	    	{
			    ecl.append( "indomain( " ).append( mt ).append( " )," ).append( eoln );
	    	}
	    	missTags = null;    	
    	} 
    	
    	// 5. create vars for virtual addresses
    	Map<Command, String> virtualAddresses = new HashMap<Command, String>();
    	Map<Command, String> physicalAddressesAfterTrans = new HashMap<Command, String>();
    	Map<Command, String> physicalAddressesForMemOperation = new HashMap<Command, String>();
    	Map<Command, String> values = new HashMap<Command, String>();
    	for( Command cmd : falseVirtualAddresses.keySet() )
    	{
    		/* NB
    		 * виртуальные адреса бывают двух типов: те, для которых есть строка TLB,
    		 * и те, для которых такой строки нет.
    		 */
    		String virtualAddress = tagNames.newVar().toString();
    		String physicalAddress = tagNames.newVar().toString();
    		virtualAddresses.put( cmd, virtualAddress );
    		physicalAddressesAfterTrans.put( cmd, physicalAddress );
    		physicalAddressesForMemOperation.put(cmd, tagNames.newVar().toString() );
    		values.put( cmd, tagNames.newVar().toString() );
    		ecl.append( "numbers:sizeof( " + 
    				virtualAddress + ",  " + 
    				tlb.getVirtualAddressBitLen() + " )," + eoln );
    	}
    	
    	// 5a. simple constraints on virtual addresses from test template
    	Map<List<Command>, Relation> virtualAddressesStaticConstraints =
    			constraintManager.getVirtualAddressesStaticConstraints();
    	for( List<Command> c : virtualAddressesStaticConstraints.keySet() )
    	{
    		String va1 = virtualAddresses.get(c.get(0));
    		String va2 = virtualAddresses.get(c.get(1));
    		switch( virtualAddressesStaticConstraints.get(c) )
    		{
    		case EQ: ecl.append( "numbers:equal( " + va1 + ", " + va2 + ", " +
    				tlb.getVirtualAddressBitLen() +	" )," + eoln ); break;
    		case NEQ: ecl.append( "numbers:notequal( " + va1 + ", " + va2 + ", " +
    				tlb.getVirtualAddressBitLen() +	" )," + eoln ); break;
    		}
    	}

    	// "dynamic" constraints on virtual addresses (not from constraintManager!)
    	for( String diffVar : diffFalseVars.keySet() )
    	{
    		List<Command> diffCmds = diffFalseVars.get( diffVar );
    		if ( ! tlbCommands.contains( diffCmds.get(0) ) 
    				|| ! tlbCommands.contains( diffCmds.get(1) )
    			)
    			continue;
    		String tlb_i = tlbBufferIndexes.get( diffCmds.get(0));
    		String tlb_j = tlbBufferIndexes.get( diffCmds.get(1));
    		String vAddr_i = virtualAddresses.get( diffCmds.get(0));
    		String vAddr_j = virtualAddresses.get( diffCmds.get(1));
    		ecl.append( "( " + tlb_i + " #\\= " + tlb_j + " -> " +
    			"numbers:notequal( " +
    				vAddr_i + ", " + 
    				vAddr_j + ", " +
    				tlb.getVirtualAddressBitLen() + 
    			" ); true )," + eoln );
    	}
    	ecl.append( eoln );
    	
    	
    	// build global flags
    	Map<Command, String> globalFlagVars = new HashMap<Command, String>();
    	gConstraints(ecl, tagNames, viewed_cmds, tlbBufferIndexes,
				tlbCommands, globalFlagVars);
    	
    	Map<Command, String> entryHi = new HashMap<Command, String>();
    	Set<Command> loadCommands = new HashSet<Command>();
		Map<Command, Set<String> > loadsMap = new HashMap<Command, Set<String>>();
    	//где задавать размер EntryHi?
    	for( Command command : scheme.getCommands() )
    	{
    		entryHi.put(command, "0"); // убрать это, когда будет сделан EntryHi!
    		// сохранить историю значений EntryHi (список переменных-версий) для каждой команды
    		// i1 = i2 /\ i1.g = 0 -> EntryHi1 = EntryHi2
    		commandPredicates.append( commandlikeTranslate( 
    				  command
    				, scheme
    				, varVersions
    				, ecl
    				, tagNames
    				, cacheLevels
    				, virtualAddresses.get( command )
    				, physicalAddressesAfterTrans.get( command )
    				, physicalAddressesForMemOperation.get( command )
    				, values.get( command )
    				, tlb
    				, loadCommands
    				, loadsMap
    			) );
    	}
    	
    	ecl.append( eoln );
    	
    	// build 'ASID' fields
    	Map<Command, String> asidVars = new HashMap<Command, String>();
    	asidConstraints(tlb, ecl, tagNames, tlbCommands, globalFlagVars,
				entryHi, asidVars);

    	// 6+7. create vars for TLB lines fields
    	//      constraints for addresses with the same TLB buffer line
    	StringBuffer rows = tagNames.newVar();
    	ecl.append( rows ).append( " = _," ).append( eoln );
    	for( Command cmd : tlbCommands )
    	{
    		String virtualAddress = virtualAddresses.get( cmd );
    		StringBuffer tlbtag = tagNames.newVar();
    		ecl.append( "tlb:key( " )
    			.append( virtualAddress ).append( ", " )
    			.append( tlb.getVirtualAddressBitLen() ).append( ", " )
    			.append( tlbBufferIndexes.get( cmd ) ).append( ", " )
    			.append( globalFlagVars.get(cmd) ).append( ", " )
    			.append( asidVars.get(cmd ) ).append( ", " )
    			.append( tlbtag ).append( ", " )
    			.append( tlb.getRangeEndBit() ).append( ", " )
    			.append( tlb.getRangeStartBit() ).append( ", " )
    			.append( tlb.getMaximumOfMask() ).append( ", " )
    			.append( tlb.getVPNd2EndBit() ).append( ", " )
    			.append( tlb.getVPNd2StartBit() )
    		.append( ")," ).append( eoln )
    		.append( "tlb:addRow( " )
    			.append( rows ).append( ", " )
    			.append( tlbtag )
    		.append( ")," ).append( eoln );
    	}
    	ecl
    	.append( "tlb:closeRows( " ).append( rows ).append( " )," ).append( eoln )
    	// 8. only one TLB row suitabling
    	.append( "tlb:onlyOne( " ).append( rows ).append( " )," ).append( eoln );
    	
    	// refill constraints
    	for( Command cmd : virtualAddresses.keySet() )
    	{
    		if ( cmd.getTLBSituation() instanceof TLBRefill )
    		{
    			ecl.append( "tlb:refill( " )
    				.append( rows ).append( ", " )
    				.append( virtualAddresses.get(cmd) ).append( ", " )
    				.append( tlb.getVirtualAddressBitLen() ).append( ", " )
    				.append( tlb.getRangeEndBit() ).append( ", " )
    				.append( tlb.getRangeStartBit() ).append( ", " )
    				.append( tlb.getVPNd2EndBit() ).append( ", " )
    				.append( tlb.getVPNd2StartBit() )
    			.append( " )," ).append( eoln );
    		}
    	}

    	// create names for sets and tags
    	Set<Command> normalCommands = new HashSet<Command>();
    	for( Command cmd : tlbCommands )
    	{
    		if ( ((TLBExists)cmd.getTLBSituation()).getValid() == 1 
    				&&
    				( ! storeCommands.contains(cmd) 
    				||
    				((TLBExists)cmd.getTLBSituation()).getmoDify() == 1 )
    			)
    		{
    			normalCommands.add(cmd);
    		}
    	}
    	
    	Map<Command, Map<Cache, String>> setVars = new HashMap<Command, Map<Cache,String>>();
    	Map<Command, Map<Cache, String>> tagVars = new HashMap<Command, Map<Cache,String>>();
    	Map<Command, String> indexVars = new HashMap<Command, String>();
    	Map<Command, Map<Cache, String>> fakeSetVars = new HashMap<Command, Map<Cache,String>>();
    	Map<Command, Map<Cache, String>> vytesntagVars = new HashMap<Command, Map<Cache,String>>();
    	
    	createSetsAndTagsNames(
    			cacheLevels
    			, tlb
    			, ecl
    			, tagNames
    			, normalCommands
    			, setVars, tagVars, indexVars, fakeSetVars,	vytesntagVars);    	
    	
    	// introduction of "difference between sets vars" and put it to the constraintManager
    	ecl.append( "[0" );
    	Map<Cache, Map<List<Command>, String>> setDiffs = new HashMap<Cache, Map<List<Command>,String>>();
    	for( Cache cache : cacheLevels )
    	{
    		Map<List<Command>, String> diffs = new HashMap<List<Command>, String>();
    		Collection<Command> cmd1viewed = new HashSet<Command>();
    		for( Command cmd1 : normalCommands )
    		{
        		Collection<Command> cmd2viewed = new HashSet<Command>();
    			for( Command cmd2 : normalCommands )
    			{
    				if ( cmd2 != cmd1  &&
    						! ( cmd1viewed.contains(cmd2) && cmd2viewed.contains(cmd1) )
    					)// already viewed
    				{
	    				String diff = tagNames.newVar().toString();
	    				diffs.put( Arrays.asList(cmd1, cmd2), diff );
	    				constraintManager.addDifferenceVar(
	    						  argManager.getSet( cmd1, cache )
	    						, argManager.getSet( cmd2, cache )
	    						, diff );
	    				ecl.append( ", " + diff );
    				}
    				cmd2viewed.add( cmd2 );
    			}
    			cmd1viewed.add( cmd1 );
    		}
    		setDiffs.put( cache, diffs );
    	}
    	ecl.append( " ] #:: [0..1]," + eoln );
    	
    	// static constraints on sets
    	Map<String, Integer> staticConstraints = constraintManager.getStaticConstraints();
    	for( String diff : staticConstraints.keySet() )
    	{
    		if ( setDiffs.containsValue( diff ) )
    			ecl.append( diff + " = " + staticConstraints.get(diff) + "," + eoln );
    	}
    	
    	// TODO dynamic difference constraints on set differences
    	// for example, ~d12 /\ ~d13 -> ~d23,  d12 <-> d21
    	ecl.append( constraintManager.getDynamicConstraints( setdiffs + virtualaddressesdiffs - but vadiffs has not values :(  ) );
    	
    	// 12. build set distribution from Constraints
    	ecl.append( "labeling( [0" );
    	for( Map<List<Command>, String> d : setDiffs.values() )
    	{
    		for( String diffVar : d.values() )
    		{
    			ecl.append( ", " + diffVar );
    		}
    	}
    	ecl.append( " ] )," ).append( eoln );

    	// build diff on fakes
    	for( Cache cache : cacheLevels )
    	{
    		Map<List<Command>, String> differs = setDiffs.get( cache );
    		for( List<Command> vars : differs.keySet() )
    		{
    			String fakeSet1 = fakeSetVars.get(vars.get(0)).get(cache);
    			String fakeSet2 = fakeSetVars.get(vars.get(1)).get(cache);
    			ecl.append( "( " + differs.get(vars) + " = 0 " +
    					"-> " + fakeSet1 + " #\\= " + fakeSet2 +
    					"; " + fakeSet1 + " #= " + fakeSet2 + " )," + eoln );
    		}
    	}
    	
    	// labeling of fakes
    	ecl.append( "labeling( [ 0" );
    	for( Map<Cache, String> sets : fakeSetVars.values() )
    	{
    		for( String fakeSet : sets.values() )
    		{
    			ecl.append( ", " ).append( fakeSet );
    		}
    	}
    	ecl.append( "] )," ).append( eoln );
    	    	
    	// 14. cache test situations
    	
    	varVersions.clear();
    	for( String name : names )
    	{
    		varVersions.put( name, 0 );
    	}
    	
    	Map< List<Tag>, Relation > tagStaticConstraints =
    		constraintManager.getTagStaticConstraints();
    	for( List<Tag> tagPair : tagStaticConstraints.keySet() )
    	{
    		Tag tag1 = tagPair.get(0);
    		Tag tag2 = tagPair.get(1);
    		String tagVar1 = tagVars.get(tag1.getCommand())
				.get( cacheLevels.get( tag1.getLevel() ) );
    		String tagVar2 = tagVars.get(tag2.getCommand())
				.get( cacheLevels.get( tag2.getLevel() ) );
    		switch( tagStaticConstraints.get(tagPair) )
    		{
    		case EQ: ecl.append( tagVar1 + " #= " + tagVar2 + eoln ); break;
    		case NEQ: ecl.append( tagVar1 + " #\\= " + tagVar2 + eoln ); break;
    		}
    	}
    	
    	/////// 20. инициализация
    	StringBuffer cacheslist = new StringBuffer( "Caches" );
    	if ( ! cacheLevels.isEmpty() )
	    	for( int level = 0; level <= cacheLevels.size(); level++ )
	    	{
	    		ecl.append( "CurrentSetsOfLevel" ).append( level ).append( " = _ ,").append( eoln );
	        	ecl.append( cacheslist )
	        		.append( " = [ CurrentSetsOfLevel" ).append( level ).append( " | ");
	        	cacheslist = tagNames.newVar();
	        	ecl.append( cacheslist ).append( " ]," ).append( eoln );
	    	}
    	ecl.append( cacheslist ).append( " = []," ).append( eoln );
		
    	List<StringBuffer> initialTagLists = new ArrayList<StringBuffer>();
		for( Map<Cache, String> sets : fakeSetVars.values() )
		{
			// this is a 'memory instruction'
			for( Cache cache : sets.keySet() )
			{
				int Max = (int)Math.pow(2, cache.getTagBitLength() ) - 1;

				List<StringBuffer> tags = new ArrayList<StringBuffer>();
				StringBuffer list = tagNames.newVar();
				StringBuffer setlist = list;
				StringBuffer list2 = tagNames.newVar();
				StringBuffer tagsetlist = list2;
				for( int setPosition = 0; setPosition < cache.getSectionNumber(); setPosition++ )
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
					.append( sets.get(cache) ).append( ", " ) // in runtime 'setVar' is int constant!
					.append( setVar ).append( ", " )
					.append( "CurrentSetsOfLevel" ).append( cacheLevels.indexOf(cache) ).append( ", " )
					.append( setlist ).append( ", " )
					.append( tagsetlist )
				.append( " )," ).append( eoln );
			}
		}
    	ecl.append( eoln );
    	
    	////////// 30. assert's -- deprecation because an assert is similar to a command
//    	for( Assert asert : scheme.getAsserts() )
//    		commandPredicates.append( 
//    				commandlikeTranslate( 
//    						  asert
//    						, scheme
//    						, varVersions
//    						, ecl
//    						, null
//    						, cacheLevels
//    						, null
//    						, null
//    						, null
//    						, null
//    						, tlb
//    					)
//    			);

        ///////////// 40. cache operation translation
//    	Map<Cache, Map<Integer, Integer>> currentSetVersions = new HashMap<Cache, Map<Integer,Integer>>();
    	
//    	for( Cache cache : cacheLevels )
//    	{
//    		currentSetVersions.put( cache, new HashMap<Integer, Integer>() );
//    	}
   
        vytesnTags = new ArrayList<String>();
        hitTags = new ArrayList<String>();
        missTags = new ArrayList<String>();

        for( Command cmd : normalCommands )
        {
            cacheOperationTranslate(
          		  cmd
          		, cacheLevels
          		, tagNames
          		, ecl
          		, fakeSetVars.get(cmd)
          		, tagVars.get( cmd )
          		, vytesntagVars.get( cmd )
          		, hitTags
          		, missTags
          		, vytesnTags
          	);
        }
    	
    	//////////// 50. LRU
    	ecl.append( "% LRU predicates" ).append( eoln );
    	int level = 0;
    	for( Cache cache : cacheLevels )
    	{
    		ecl.append( "lru:vytesnTagsLRU( " )
    			.append( "CurrentSetsOfLevel" ).append( level++ ).append( ", " )
    			.append( cache.getSectionNumber() )
    		.append( ")," ).append( eoln );
    	}
    	ecl.append( eoln );
    	
		/////////////// 60. labeling
    	// 61. выбор вытесняемых тегов
    	ecl.append( "%вытесненные" ).append( eoln );
    	for( String vt : vytesnTags )
    	{
		    ecl.append( "indomain( " ).append( vt ).append( " )," ).append( eoln );
    	}
    	vytesnTags = null;
    	
    	// 62. выбор hit-тегов
    	ecl.append( "%hit" ).append( eoln );
    	for( String ht : hitTags )
    	{
		    ecl.append( "indomain( " ).append( ht ).append( " )," ).append( eoln );
    	}
    	hitTags = null;
    	
    	// 63. выбор miss тегов
    	ecl.append( "%miss" ).append( eoln );
    	for( String mt : missTags )
    	{
		    ecl.append( "indomain( " ).append( mt ).append( " )," ).append( eoln );
    	}
    	missTags = null;
    	
    	// 64. labeling( Seti_0 ) initial sets  - to below ??
    	for( StringBuffer ist : initialTagLists )
    	{
    		ecl.append( "labeling( " ).append( ist ).append( " )," ).append( eoln );
    	}
    	initialTagLists = null;
    	
    	// 65. random_result( var )  - to below ??
		for( String name : names )
		{
			ecl	.append( "numbers:random_result( _0" ).append( name ).append( " )," )
				.append( eoln );
		}
		
		// 70. common labeling ( Constraints[[s1 = s2]] + s1 = v1[..] + labeling(s1, v1) )
    	for( Cache cache : cacheLevels )
    	{
    		Map<List<Command>, String> differs = setDiffs.get( cache );
    		for( List<Command> vars : differs.keySet() )
    		{
    			String set1 = setVars.get(vars.get(0)).get(cache);
    			String set2 = setVars.get(vars.get(1)).get(cache);
    			ecl.append( "( " + differs.get(vars) + " = 0 " +
    					"-> " + set1 + " #\\= " + set2 +
    					"; " + set1 + " #= " + set2 + " )," + eoln );
    		}
    	}
    	
    	// labeling of sets
    	ecl.append( "labeling( [ 0" );
    	for( Map<Cache, String> sets : setVars.values() )
    	{
    		for( String set : sets.values() )
    		{
    			ecl.append( ", " ).append( set );
    		}
    	}
    	ecl.append( "] )," ).append( eoln );

    	settagvaIntersections(cacheLevels, tlb, ecl, tagNames,
				normalCommands, setVars, tagVars);
    	
    	oddbitConstraints(tlb, ecl, tagNames, tlbBufferIndexes,
				virtualAddresses, physicalAddressesAfterTrans, rows);
    	
		// 80. LOAD-STORE constraints => diff on indexes
		load_store( 
				  ecl
				, scheme
				, constraintManager
				, argManager
				, normalCommands
				, tagNames
				, tagVars
				, setVars
				, indexVars
				, values
				, ( tlb != null ? tlb.getPhysicalAddressBitLen() : 
					( !cacheLevels.isEmpty() ? cacheLevels.get(0).getAddressBitLength() : 0 ) )
			);
		
		// phys в LoadMemory = tag||set||idx
		if ( ! cacheLevels.isEmpty() )
			for( Command command : normalCommands )
			{
				String physicalAddress = physicalAddressesForMemOperation.get( command );
				String tag = "[ " + tagVars.get(command).get(cacheLevels.get(0)) + " ]";
				String set = "[ " + setVars.get(command).get(cacheLevels.get(0)) + " ]";
				String index = indexVars.get(command);
				String tmp = tagNames.newVar().toString();
				int tslen = cacheLevels.get(0).getTagBitLength() + cacheLevels.get(0).getSetNumberBitLength();
				ecl.append( "numbers:concat( " )
					.append( tmp ).append( ", " )
					.append( tag ).append( ", " )
					.append( cacheLevels.get(0).getTagBitLength() ).append( ", " )
					.append( set ).append( ", " )
					.append( cacheLevels.get(0).getSetNumberBitLength() )
				.append( " )," ).append( eoln )
				.append( "numbers:concat( " )
					.append( physicalAddress ).append( ", " )
					.append( tmp ).append( ", " )
					.append( tslen ).append( ", " )
					.append( index ).append( ", " )
					.append( tlb.getPhysicalAddressBitLen() - tslen )
				.append( " )," ).append( eoln );
			}
		
    	// labeling of virtual addresses
    	for( String index : indexVars.values() )
    	{
    		ecl.append( "numbers:random_result( " )
    		.append(index).append( " )," ).append( eoln );
    	}
    	for( String virtualAddress : virtualAddresses.values() )
    	{
    		ecl.append( "numbers:random_result( " )
    		.append(virtualAddress).append( " )," ).append( eoln );
    	}
    	for( String physicalAddress : physicalAddressesAfterTrans.values() )
    	{
    		ecl.append( "numbers:random_result( " )
    			.append( physicalAddress ).append( " )," ).append( eoln );
    	}
    	for( String physicalAddress : physicalAddressesForMemOperation.values() )
    	{
    		ecl.append( "numbers:random_result( " )
    			.append( physicalAddress ).append( " )," ).append( eoln );
    	}
    	for( String value : values.values() )
    	{
    		ecl.append( "numbers:random_result( " )
    			.append( value ).append( " )," ).append( eoln );
    	}
    	ecl.append( eoln );
		
    	// 90. labeling of the initial state
    	ecl.append( "tlb:labelRows( " ).append( rows ).append( " )," ).append( eoln );
    	
    	// build 'TLB' from rows and PFNs'
    	ecl.append( "TLB = _," ).append( eoln );
    	for( Command cmd : tlbCommands )
    	{
    		ecl.append( "tlb:addPfn( TLB, " )
    			.append( rows ).append( ", " )
    			.append( tlbBufferIndexes.get(cmd) ).append( ", " )
    			.append( virtualAddresses.get(cmd) ).append( ", " )
    			.append( tlb.getVirtualAddressBitLen() ).append( ", " )
    			.append( tlb.getVPNd2StartBit() ).append( ", " )
    			.append( physicalAddressesAfterTrans.get(cmd) ).append( ", " )
    			.append( tlb.getPhysicalAddressBitLen() ).append( ", " )
    			.append( tlb.getPhysicalAddressBitLen() - 1 ).append( ", " )
    			.append( tlb.getPhysicalAddressBitLen() - tlb.getPFNBitLen() ).append( ", " )
    			.append( ((TLBExists)cmd.getTLBSituation()).getValid() ).append( ", " )
    			.append( ((TLBExists)cmd.getTLBSituation()).getmoDify() )
    		.append( " )," ).append( eoln );
    	}
    	ecl.append( "lru:closeList( TLB )," ).append( eoln );
    	if ( tlb != null )
    	{
	    	ecl.append( "tlb:labelTLB( TLB, " ) 
	    		.append( tlb.getPFNBitLen() )   	
	    	.append( " )," ).append( eoln );
    	}
    	
    	// build 'Memory' variable: [ address +> value ]
    	// не во всех случаях нужно инициализировать память, даже если делается LW:
    	// а именно в тех случаях, когда результат загрузки ни на что не влияет
    	ecl.append( "Memory = _," ).append( eoln );
    	for( Command cmd : loadCommands )
    	{
    		ecl.append( "lru:addCell( Memory, " )
    			.append( physicalAddressesForMemOperation.get(cmd) ).append( ", " )
    			.append( values.get(cmd) ).append( " )," ).append( eoln );
    	}
    	ecl.append( "lru:closeList( Memory )," ).append( eoln );
    	
    	// build 'TLBIndexes'
    	ecl.append( "TLBIndexes = ");
    	for( Command cmd : scheme.getCommands() )
    	{
    		if ( tlbBufferIndexes.containsKey(cmd) )
    			ecl.append( "[ " ).append( tlbBufferIndexes.get(cmd) ).append( "| " );
    		else
    			ecl.append( "[ " ).append( 0 ).append( "| " );
    	}
    	ecl.append( "[]" );
    	for( int i = 0; i < scheme.getCommands().size(); i++ )
    	{
    		ecl.append( "]");
    	}
    	ecl.append( "," ).append( eoln );
    	
		ecl.append( "true." ).append( eoln )
		.append( eoln )
		.append( commandPredicates );

    	return ecl;
    }

	private void asidConstraints(
			final TLB tlb, 
			StringBuffer ecl,
			VarsController tagNames, 
			Set<Command> tlbCommands,
			Map<Command, String> globalFlagVars, 
			Map<Command, String> entryHi,
			Map<Command, String> asidVars)
	{
    	for( Command cmd : tlbCommands )
    	{
    		String var = tagNames.newVar().toString();
    		asidVars.put(cmd, var);
    		ecl.append( "numbers:sizeof( " )
    			.append( var ).append( ", " )
    			.append( tlb.getASIDBitLen() )
    		.append( " )," ).append( eoln );    		
    	}
    	// i1 = i2 -> asid1 = asid2
//    	viewed_cmds.clear();
//    	for( Command cmd1 : tlbCommands )
//    	{
//    		viewed_cmds.add( cmd1 );
//    		for( Command cmd2 : tlbCommands )
//    		{
//    			if ( viewed_cmds.contains(cmd2) )
//    				continue;
//    			ecl.append( "( " )
//    				.append( tlbBufferIndexes.get(cmd1) ).append( " = " ).append( tlbBufferIndexes.get(cmd2) )
//    				.append( " -> " )
//    				.append( asidVars.get(cmd1) ).append( " #= " ).append( asidVars.get(cmd2) )
//    			.append( " ; true )," ).append( eoln );
//    		}
//    	}
    	// g = 0 -> asid = version of EntryHi
    	for( Command cmd : tlbCommands )
    	{
    		ecl.append( "( " ).append( globalFlagVars.get(cmd) )
    			.append( " = 0 -> ")
    			.append( asidVars.get(cmd) ).append( " #= " ).append( entryHi.get(cmd) )
    		.append( " ; true )," ).append( eoln );
    	}
	}

	private void gConstraints(StringBuffer ecl, VarsController tagNames,
			Collection<Command> viewed_cmds,
			Map<Command, String> tlbBufferIndexes,
			Set<Command> tlbCommands,
			Map<Command, String> globalFlagVars) {
		ecl.append( "[ 0" );
    	for( Command cmd : tlbCommands )
    	{
    		String g = tagNames.newVar().toString();
    		ecl.append( ", " ).append( g );
    		globalFlagVars.put(cmd, g);
    	}
    	ecl.append( " ] #:: [0..1]," ).append( eoln );
    	// g = const
    	for( Command cmd : tlbCommands )
    	{
    		if ( ((TLBExists)cmd.getTLBSituation()).getG() != null )
    		{
    			ecl.append( globalFlagVars.get(cmd) ).append( " = " )
    				.append( ((TLBExists)cmd.getTLBSituation()).getG().intValue() ).append( "," ).append( eoln );
    		}
    	}
    	// i1 = i2 => g1 = g2
    	viewed_cmds.clear();
    	for( Command cmd1 : tlbCommands )
    	{
    		viewed_cmds.add( cmd1 );
    		for( Command cmd2 : tlbCommands )
    		{
    			if ( viewed_cmds.contains(cmd2))
    				continue;
    			ecl.append( "(" ).append( tlbBufferIndexes.get(cmd1) )
    				.append( " = " ).append( tlbBufferIndexes.get(cmd2) )
    				.append( " -> " )
    					.append( globalFlagVars.get(cmd1) ).append( " #= ")
    					.append( globalFlagVars.get(cmd2) ).append( "; true ),").append( eoln );    				
    		}
    	}
    	
    	// label g
    	for( String g : globalFlagVars.values() )
    	{
    		ecl.append( "indomain( " ).append( g ).append( ", max ),").append( eoln );
    	}
	}

	private void oddbitConstraints(final TLB tlb, StringBuffer ecl,
			VarsController tagNames, Map<Command, String> tlbBufferIndexes,
			Map<Command, String> virtualAddresses,
			Map<Command, String> physicalAddressesAfterTrans, StringBuffer rows)
	{
		// if odd bits are the same then physAddrTr1[pfn] = physAddrTr2[pfn]
    	Collection<Command> viewed_cmd = new HashSet<Command>();
    	for( Command cmd1 : tlbBufferIndexes.keySet() )
    	{
    		viewed_cmd.add(cmd1);
    		String index1 = tlbBufferIndexes.get(cmd1);
    		for( Command cmd2 : tlbBufferIndexes.keySet() )
    		{
    			if ( viewed_cmd.contains(cmd2) )
    				continue;
    			
    			String index2 = tlbBufferIndexes.get(cmd2);
    			
//    			ecl.append
//    			>> ( index1 #\= index2 -> true ;
//    				(	tlb:oddbit(ob1,virtual1,mask1),
//    					tlb:oddbit(ob2,virtual2,mask2),
//    					numbers:getbits(pfn1,phys1),
//    					numbers:getbits(pfn2,phys2),
//    			****		( ob1 #= ob2, numbers:equal(pfn1, pfn2)
//							; ob1 #\= ob2, numbers:notequal(pfn1, pfn2) )
//    			**** (v1 != v2)	 ob1 #\= ob2, numbers:notequal(pfn1, pfn2) )
//    			>> ) )
    			
    			StringBuffer oddbit1 = tagNames.newVar();
    			StringBuffer oddbit2 = tagNames.newVar();
    			StringBuffer pfn1 = tagNames.newVar();
    			StringBuffer pfn2 = tagNames.newVar();
    			String physical1 = physicalAddressesAfterTrans.get(cmd1);
    			String physical2 = physicalAddressesAfterTrans.get(cmd2);
    			String virtualAddress1 = virtualAddresses.get(cmd1);
    			String virtualAddress2 = virtualAddresses.get(cmd2);

    			ecl.append( "( " ).append( index1 ).append( " #\\= " ).append( index2 )
    				.append( " -> true ; " ).append( eoln )
    				.append( "( " )
    				.append( "tlb:oddbit( " )
						.append( oddbit1 ).append( ", " )
						.append( tlbBufferIndexes.get(cmd1) ).append( ", " )
						.append( virtualAddress1 ).append( ", " )
						.append( tlb.getVirtualAddressBitLen() ).append( ", " )
						.append( tlb.getVPNd2StartBit() ).append( ", " )
						.append( tlb.getMaximumOfMask() ).append( ", " )
						.append( rows )
					.append( " ), " ).append( eoln )
    				.append( "tlb:oddbit( " )
						.append( oddbit2 ).append( ", " )
						.append( tlbBufferIndexes.get(cmd2) ).append( ", " )
						.append( virtualAddress2 ).append( ", " )
						.append( tlb.getVirtualAddressBitLen() ).append( ", " )
						.append( tlb.getVPNd2StartBit() ).append( ", " )
						.append( tlb.getMaximumOfMask() )
						.append( rows )
					.append( " ), " ).append( eoln )
					.append( "numbers:getbits( " )
						.append( pfn1 ).append( ", " )
						.append( physical1 ).append( ", " )
						.append( tlb.getPhysicalAddressBitLen() ).append( ", " )
						.append( tlb.getPhysicalAddressBitLen() - 1 ).append( ", " )
						.append( tlb.getPhysicalAddressBitLen() - tlb.getPFNBitLen() )
					.append( " ), " ).append( eoln )
					.append( "numbers:getbits( " )
						.append( pfn2 ).append( ", " )
						.append( physical2 ).append( ", " )
						.append( tlb.getPhysicalAddressBitLen() ).append( ", " )
						.append( tlb.getPhysicalAddressBitLen() - 1 ).append( ", " )
						.append( tlb.getPhysicalAddressBitLen() - tlb.getPFNBitLen() )
					.append( " ), " ).append( eoln );
					
				TLBExists situation1 = (TLBExists)cmd1.getTLBSituation();
				TLBExists situation2 = (TLBExists)cmd2.getTLBSituation();
					if ( situation1.getValid() != situation2.getValid() 
						||
						situation1.getmoDify() != situation2.getmoDify() )
					{
						ecl .append( oddbit1 ).append( " #\\= " ).append( oddbit2 )
						.append( ", " ).append( "numbers:notequal( " )
									.append( pfn1 ).append( ", " )
									.append( pfn2 ).append( ", " )
									.append( tlb.getPFNBitLen() ).append( " ) ) )," ).append( eoln );
					}
					else
					{
						ecl .append( "( " ).append( oddbit1 ).append( " #= " ).append( oddbit2 )
						.append( ", " ).append( "numbers:equal( " )
									.append( pfn1 ).append( ", " )
									.append( pfn2 ).append( ", " )
									.append( tlb.getPFNBitLen() ).append( " ); " ).append( eoln )
						.append( oddbit1 ).append( " #\\= " ).append( oddbit2 )
						.append( ", " ).append( "numbers:notequal( " )
									.append( pfn1 ).append( ", " )
									.append( pfn2 ).append( ", " )
									.append( tlb.getPFNBitLen() ).append( " ) ) ) )," ).append( eoln );    			
					}
    		}
    	}
	}

	private void createSetsAndTagsNames(
			final List<Cache> cacheLevels,
			final TLB tlb,
			StringBuffer ecl, 
			VarsController tagNames,
			Set<Command> normalCommands,
			Map<Command, Map<Cache, String>> setVars,
			Map<Command, Map<Cache, String>> tagVars,
			Map<Command, String> indexVars,
			Map<Command, Map<Cache, String>> fakeSetVars,
			Map<Command, Map<Cache, String>> vytesntagVars)
	{
		for( Command cmd : normalCommands )
    	{
    		Map<Cache, String> sets = new HashMap<Cache, String>();
    		Map<Cache, String> tags = new HashMap<Cache, String>();
    		Map<Cache, String> vytesntags = new HashMap<Cache, String>();
    		Map<Cache, String> fakeSets = new HashMap<Cache, String>();
    		
    		// TODO а если не все уровни кэш-памяти нужны команде (не участвуют в тестовой ситуации?)
    		for( Cache cache : cacheLevels )
    		{
    			String set = tagNames.newVar().toString();
    			String tag = tagNames.newVar().toString();
    			String fakeSet = tagNames.newVar().toString();
    			
    			ecl.append( set ).append( " #:: [ 0 .. " )
    			.append((int)Math.pow(2, cache.getSetNumberBitLength()) - 1 ).append( " ]," ).append( eoln );
    			
    			ecl.append( tag ).append( " #:: [ 0 .. " )
    			.append((int)Math.pow(2, cache.getTagBitLength() ) - 1 ).append( " ]," ).append( eoln );
    			
    			sets.put(cache, set);
    			tags.put(cache, tag);
    			Set<ProcedureTestSituation> ts = null;
    			if (  cmd.isLOAD() )
    				ts = cmd.getTestSituationParameters().get( "LoadMemory" );
    			else if ( cmd.isSTORE() )
    				ts = cmd.getTestSituationParameters().get( "StoreMemory" );
    			if ( ts != null )
    			{
    				for( ProcedureTestSituation pts : ts )
    				{
    					if ( pts instanceof CacheMiss 
    							&& cacheLevels.get( ((CacheMiss)pts).getLevel() ).equals( cache ) )
    					{
    						String vyt = tagNames.newVar().toString();
    	    				vytesntags.put(cache, vyt);
    						break;
    					}
    				}
    			}
    			fakeSets.put(cache, fakeSet);
    			ecl.append( fakeSet + " #:: [ 1 .. " + 
    					Math.min( (int)Math.pow(2, cache.getSetNumberBitLength() ), normalCommands.size() ) 
    				+ " ]," + eoln );
    		}
    		setVars.put(cmd, sets);
    		tagVars.put(cmd, tags);
    		vytesntagVars.put(cmd, vytesntags);
    		fakeSetVars.put(cmd, fakeSets);
    		
    		String index = tagNames.newVar().toString();
    		indexVars.put(cmd, index);
    		ecl.append( "numbers:sizeof( " + index + ", " + 
    				(tlb.getPhysicalAddressBitLen() 
    						- cacheLevels.get(0).getTagBitLength()
    						- cacheLevels.get(0).getSetNumberBitLength()
    				) + " )," + eoln );
    	}
	}

	private void settagvaIntersections(
			final List<Cache> cacheLevels,
			final TLB tlb,
			StringBuffer ecl,
			VarsController tagNames,
			Set<Command> normalCommands,
			Map<Command, Map<Cache, String>> setVars,
			Map<Command, Map<Cache, String>> tagVars)
	{
		for( Command cmd : normalCommands )
		{
			Collection<Cache> viewed = new HashSet<Cache>();
			for( Cache cache1 : cacheLevels )
			{
				viewed.add(cache1);
				for( Cache cache2 : cacheLevels )
				{
					if ( viewed.contains(cache2) )
						continue;
					
					String tag1 = tagVars.get(cmd).get(cache1);
					String set1 = setVars.get(cmd).get(cache1);
					String tag2 = tagVars.get(cmd).get(cache2);
					String set2 = setVars.get(cmd).get(cache2);
					
					int tag1BitLength = cache1.getTagBitLength();
					int set1BitLength = cache1.getSetNumberBitLength();
					int tag2BitLength = cache2.getTagBitLength();
					int set2BitLength = cache2.getSetNumberBitLength();

					if ( tag1BitLength == tag2BitLength
							&&
						set1BitLength == set2BitLength )
					{
						ecl.append( tag1 + " #= " + tag2 + eoln );
						ecl.append( set1 + " #= " + set2 + eoln );
					}
					
					if ( tag1BitLength < tag2BitLength )
					{
						tagSetInterConstraints(ecl, tagNames, tag1, set1, tag2,
								set2, tag1BitLength, set1BitLength,
								tag2BitLength, set2BitLength);
					}
					else if ( tag1BitLength > tag2BitLength )
					{
						tagSetInterConstraints(ecl, tagNames, tag2, set2, tag1,
								set1, tag2BitLength, set2BitLength,
								tag1BitLength, set1BitLength);						
					}
				}
			}
		}
		
//		for( Command cmd : virtualAddresses.keySet() )
//    	{
//    		// пересечь границы виртуального адреса и границы сета
//    		// если что останется - сгенерировать numbers:getbits...
//			String virtualAddress = virtualAddresses.get(cmd);
//    		for( Cache cache : cacheLevels )
//    		{
//    			if ( cache.getTagBitLength() + cache.getSetNumberBitLength() 
//    					> tlb.getPFNBitLen() )
//    			{
//    				String set = "[ " + setVars.get(cmd).get(cache) + " ]";
//    				String n1 = tagNames.newVar().toString();
//    				String n2 = tagNames.newVar().toString();
//    				if ( cache.getTagBitLength() > tlb.getPFNBitLen() )
//    				{
//	    				// set влезает целиком
//	    				ecl.append( "numbers:getbits( " )
//	    				.append( n1 ).append( ", " )
//	    				.append( virtualAddress ).append( ", " )
//	    				.append( tlb.getVirtualAddressBitLen() ).append( ", " )
//	    				.append( (tlb.getPhysicalAddressBitLen() 
//	    						- cache.getTagBitLength() - 1) ).append( ", " )
//	    	    		.append( (tlb.getPhysicalAddressBitLen() 
//	    	    				- cache.getTagBitLength() 
//	    	    				- cache.getSetNumberBitLength()) ).append( " )," )
//	    	    		.append( eoln )
//	    	    		.append( "numbers:equal( " )
//	    	    		.append( n1 ).append( ", " )
//	    	    		.append( set ).append( ", " )
//	    	    		.append( cache.getSetNumberBitLength() )
//	    	    		.append( " )," )
//	    	    		.append( eoln ).append( eoln );	    				
//    				}
//    				else
//    				{
//    					// только кусок set'a
//	    				ecl.append( "numbers:getbits( " )
//	    				.append( n1 ).append( ", " )
//	    				.append( virtualAddress ).append( ", " )
//	    				.append( tlb.getVirtualAddressBitLen() ).append( ", " )
//	    				.append( (tlb.getPhysicalAddressBitLen() 
//	    						- tlb.getPFNBitLen() - 1) ).append( ", " )
//	    	    		.append( (tlb.getPhysicalAddressBitLen() 
//	    	    				- cache.getTagBitLength() 
//	    	    				- cache.getSetNumberBitLength()) ).append( " )," )
//	    	    		.append( eoln )
//	    	    		.append( "numbers:getbits( " )
//	    	    		.append( n2 ).append( ", " )
//	    	    		.append( set ).append( ", " )
//	    	    		.append( cache.getSetNumberBitLength() ).append( ", " )
//	    	    		.append( (cache.getTagBitLength() 
//	    	    				+ cache.getSetNumberBitLength()
//	    	    				- tlb.getPFNBitLen() - 1
//	    	    				) ).append( ", " )
//	    	    		.append( 0 ).append( " ),")
//	    	    		.append( eoln )
//	    	    		.append( "numbers:equal( " )
//	    	    		.append( n1 ).append( ", " )
//	    	    		.append( n2 ).append( ", " )
//	    	    		.append( (cache.getTagBitLength() 
//	    	    				+ cache.getSetNumberBitLength()
//	    	    				- tlb.getPFNBitLen() ) )
//	    	    		.append( " )," )
//	    	    		.append( eoln ).append( eoln );    					
//    				}
//    			}
//    			
//    			if ( cache.getTagBitLength() > tlb.getPFNBitLen() )
//    			{
//    				String tag = "[ " + tagVars.get(cmd).get(cache) + " ]";
//    				String n1 = tagNames.newVar().toString();
//    				String n2 = tagNames.newVar().toString();
//    				
//    				ecl.append( "numbers:getbits( " )
//    				.append( n1 ).append( ", " )
//    				.append( virtualAddress ).append( ", " )
//    				.append( tlb.getVirtualAddressBitLen() ).append( ", " )
//    				.append( (tlb.getPhysicalAddressBitLen() 
//    						- tlb.getPFNBitLen() - 1) ).append( ", " )
//    	    		.append( (tlb.getPhysicalAddressBitLen() 
//    	    				- cache.getTagBitLength() ) ).append( " )," )
//    	    		.append( eoln )
//    	    		.append( "numbers:getbits( " )
//    	    		.append( n2 ).append( ", " )
//    	    		.append( tag ).append( ", " )
//    	    		.append( cache.getTagBitLength() ).append( ", " )
//    	    		.append( (cache.getTagBitLength() 
//    	    				- tlb.getPFNBitLen() - 1
//    	    				) ).append( ", " )
//    	    		.append( 0 ).append( " ),")
//    	    		.append( eoln )
//    	    		.append( "numbers:equal( " )
//    	    		.append( n1 ).append( ", " )
//    	    		.append( n2 ).append( ", " )
//    	    		.append( (cache.getTagBitLength() 
//    	    				- tlb.getPFNBitLen() ) )
//    	    		.append( " )," )
//    	    		.append( eoln ).append( eoln );    					
//    			}
//    		}
//    	}
	}

	private void tagSetInterConstraints(StringBuffer ecl,
			VarsController tagNames, String tag1, String set1, String tag2,
			String set2, int tag1BitLength, int set1BitLength,
			int tag2BitLength, int set2BitLength)
	{
		// t1 = t2[..]
		ecl.append( "numbers:getbits( " )
			.append( "[ " + tag1 + " ], " )
			.append( "[ " + tag2 + " ], " )
			.append( tag2BitLength ).append( ", " )
			.append( tag2BitLength - 1 ).append( ", " )
			.append( tag2BitLength - tag1BitLength )
		.append( " ),").append( eoln );

		// s2 = s1[..]
		ecl.append( "numbers:getbits( " )
			.append( "[ " + set2 + " ], " )
			.append( "[ " + set1 + " ], " )
			.append( set1BitLength ).append( ", " )
			.append( set2BitLength - 1 ).append( ", " )
			.append( 0 )
		.append( " ),").append( eoln );
		
		// s1[..] = t2[..]
		StringBuffer tmp = tagNames.newVar();
		ecl.append( "numbers:getbits( ")
			.append( tmp ).append( " , " )
			.append( "[ " + set1 + " ], " )
			.append( set1BitLength ).append( ", " )
			.append( set1BitLength - 1 ).append( ", " )
			.append( set1BitLength - tag2BitLength + tag1BitLength )
			.append( " )," ).append( eoln );
		ecl.append( "numbers:getbits( ")
			.append( tmp ).append( " , " )
			.append( "[ " + tag2 + " ], " )
			.append( tag2BitLength ).append( ", " )
			.append( tag2BitLength - tag1BitLength - 1 ).append( ", " )
			.append( 0 )
		.append( " )," ).append( eoln );
	}
    
    private String notnull( String name, VarsController tagNames )
    {
    	if ( name != null )
    		return name;
    	else
    		return tagNames.newVar().toString();
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
    
    /**
     * возвращает дополнительные предикаты, необходимые команде
     * обладает побочным эффектом: меняет ecl (дописывает вызов предиката)
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
    		  Command command
    		, Scheme scheme
    		, Map<String, Integer> varVersions
    		, StringBuffer ecl
    		, VarsController tagsVersions
    		, List<Cache> cacheLevels
    		, String virtualAddress
    		, String physicalAddressAfterTranslation
    		, String physicalAddressForMemOperation
    		, String value
    		, TLB tlb
    		, Set<Command> LoadCommands
    		//, Set<Command> StoreCommands
    		, Map<Command, Set<String> > loadsMap
    	)
    	throws IOException, RecognitionException
    {
		String pathToTSL = sourcePath + "\\" + command.getCop() + "\\" + command.getTestSituation() + ".tsl";
		CharStream inputStream = new ANTLRFileStream( pathToTSL );
		TeSLaLexer lexer = new TeSLaLexer( inputStream );
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		TeSLaParser parser = new TeSLaParser(tokens);
		String prefix = command.getCop() + "@" + command.getTestSituation() + "#" + tagsVersions.newVar().toString();
		TeSLaParser.program_return prog = parser.program( 
				  command.getArgs()
				, scheme
				, command
				, prefix
				, cacheLevels
				, tlb
				, command.getTLBSituation()
				, ( tlb != null ? tlb.getPhysicalAddressBitLen() : ( !cacheLevels.isEmpty() ? cacheLevels.get(0).getAddressBitLength() : 0 ))
			);

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
		
		// process prog.readlist
		for( String var : prog.readlist )
		{
			// if [ L +> { ... var ... } ] << loadsMap then { add L to loadCommands; remove [L +> {var}] from loadsMap; }
			for( Command cmd : loadsMap.keySet() )
			{
				Set<String> vars = loadsMap.get( cmd );
				if ( vars.contains( var ) )
				{
					LoadCommands.add( cmd );
					loadsMap.remove( cmd );
					break;
				}
			}
		}
		
		// foreach RESULT var: if last changed operation was LoadMemory then { add it to the 'LoadCommands'; var.read = false; save this changing command if it with LoadMemory; }
		for( int i = 0; i < command.getArgs().size(); i++ )
		{
			String arg = command.getArgs().get(i);
			if ( prog.signature.get(i).getStatus() == Status.SIGNATURE_RESULT )
			{
				Map<Command, Set<String>> newLoadsMap = new HashMap<Command, Set<String>>();
				for( Command cmd : loadsMap.keySet() )
				{
					if ( loadsMap.get(cmd).contains( arg ))
					{
						if ( loadsMap.get(cmd).size() > 1 )
						{						
							loadsMap.get(cmd).remove( arg );
							newLoadsMap.put(cmd, loadsMap.get(cmd));
						}
					}
					else
					{
						newLoadsMap.put(cmd, loadsMap.get(cmd));
					}
				}
				loadsMap = newLoadsMap;
			}
		}
		
		// foreach var from prog.loadlist
		if ( ! prog.loadlist.isEmpty() )
		{
			loadsMap.put( command, prog.loadlist );
		}
		
		if ( prog.hasAddressTranslation )
		{
			ecl.append( ", " + virtualAddress + ", " + physicalAddressAfterTranslation );
		}

		if ( prog.memoryOperation != null )
		{
			ecl.append( ", " ).append( physicalAddressForMemOperation )
			.append( ", " ).append( value );
			
			if ( prog.memoryOperation == MemoryCommand.LOAD )
				LoadCommands.add(command);

			if ( prog.isDataCacheused )
				Level0Searching:
				for( String procedure : command.getTestSituationParameters().keySet() )
				{
					if ( procedure != "LoadMemory" && procedure != "StoreMemory" )
						continue;
					
					Set<ProcedureTestSituation> parameters = command.getTestSituationParameters().get(procedure);
					for( ProcedureTestSituation proc : parameters )
					{
						if ( proc instanceof CacheTestSituation )
						{
							if ( ((CacheTestSituation) proc).getLevel() == 1 )
							{
								// change level to 0
								CacheTestSituation proc0;
								if ( proc instanceof CacheHit )
								{
									final CacheHit procTS = (CacheHit)proc;
									proc0 = new CacheHit(){
										@Override
										public int getLevel() {
											return 0;
										}

										@Override
										public String getSetVar() {
											return procTS.getSetVar();
										}

										@Override
										public String getTagVar() {
											return procTS.getTagVar();
										}};
								}
								else if ( proc instanceof CacheMiss )
								{
									final CacheMiss procTS = (CacheMiss)proc;
									proc0 = new CacheMiss(){
										@Override
										public String getVTagVar() {
											return procTS.getVTagVar();
										}

										@Override
										public int getLevel() {
											return 0;
										}

										@Override
										public String getSetVar() {
											return procTS.getSetVar();
										}

										@Override
										public String getTagVar() {
											return procTS.getTagVar();
										}};
								}
								else
									throw new Error("unexpecting type of cache test situation");
								
								parameters.remove( (CacheTestSituation)proc );
								parameters.add( proc0 );
								break Level0Searching;
							}
						}
					}
				}
		}
		
		ecl.append( ")," ).append( eoln );
		
		
		ecl.append( eoln );
		
		command.setMemValueSize( prog.memValueSize );
		
		for( String var : changedVars )
		{
			Integer version = varVersions.get( var );
			varVersions.put( var, version + 1 );
		}
		return prog.eclipseProgram;
    }
    
    private void cacheOperationTranslate(
    		  Command command
    		, List<Cache> cacheLevels
    		, VarsController tagsVersions
    		, StringBuffer ecl
    		, Map<Cache, String> setVars
    		, Map<Cache, String> tagVars
    		, Map<Cache, String> vytesntagVars
    		, final List<String> hitTags
    	    , final List<String> vytesnTags
    		, final List<String> missTags
    	)
    {
		Map<String, Set<ProcedureTestSituation>> testSituation = command.getTestSituationParameters();
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
						if ( cacheLevels.isEmpty() )
						{
							throw new Error("Define caches for using cache test situations");
						}
						
						CacheHit hit = (CacheHit)ts;
						Cache cache = cacheLevels.get( hit.getLevel() );
						String tag = tagVars.get( cache );
						String tagset = tag + "set";
						
						// HIT for 'tag' in 'cache'
						
						StringBuffer setVar = tagsVersions.newVar();
						String setVarsStructure = "CurrentSetsOfLevel" + hit.getLevel();
//							String hitsStructure = "HitsOfLevel" + hit.getLevel();
						StringBuffer hitSetsStructure = tagsVersions.newVar();
//							String setVersionsStructure = "SetVersionsOfLevel" + hit.getLevel();
						String setNumber = setVars.get( cache );
						
						ecl.append( "lru:latestSetVar( " )
						.append( hitSetsStructure ).append( ", " )
							.append( setVar ).append( ", " )
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
						if ( cacheLevels.isEmpty() )
						{
							throw new Error("Define caches for using cache test situations");
						}
						
						CacheMiss miss = (CacheMiss)ts;
						Cache cache = cacheLevels.get( miss.getLevel() );
						String tag = tagVars.get( cache );
						String tagset = tag + "set";

						// MISS for 'tag' in 'cache
						String vytesnTag = miss.getVTagVar();
						if ( vytesnTag == null )
						{
							vytesnTag = vytesntagVars.get( cache );
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
						String setNumber = setVars.get( cache );
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

//    private String tlb_latestSetVar;
    private String tlbOperationTranslate(
	  		  Command command
	  		, TLB tlb
	  		, VarsController tagsVersions
	  		, StringBuffer ecl
	  		, String index
	  		, String vytesnIndex
	  		, String hitSetsStructure
	  		, String vytesnStructure
	  		, final List<String> hitTags
	  	    , final List<String> vytesnTags
	  	    , final List<String> missTags
	  	    , final String tlb_latestSetVar
		  	)
	{
		Map<String, Set<ProcedureTestSituation>> testSituation = command.getTestSituationParameters();
		if ( ! testSituation.containsKey( "AddressTranslation" ) )
			return "";

		TLBSituation tlbSituation = command.getTLBSituation();
		if ( tlbSituation instanceof TLBHit )
		{
			String tagset = index + "set";
					
			ecl
			.append( index ).append( " in " ).append( tlb_latestSetVar ).append( "," ).append( eoln )
			.append( "intset( " ).append( tagset ).append( ", 1, " ).append( tlb.getSize() ).append( " ), " )
			.append( "#( " ).append( tagset ).append( ", 1 ), " )
			.append( index ).append( " in " ).append( tagset ).append( "," ).append( eoln )
			
			.append( "lru:addHit( ")
				.append( index ).append( ", " )
				.append( tagset ).append( ", " )
				.append( 0 ).append( ", " )
				.append( hitSetsStructure )
			.append( " )," ).append( eoln )
			.append( eoln );

		    hitTags.add( index );
		    
		    return tlb_latestSetVar;
		}
		else if ( tlbSituation instanceof TLBMiss )
		{
			String tagset = index + "set";

			// MISS for 'tag' in 'cache
			String vytesnTagSet = vytesnIndex + "set";

			/*	X2 = vytesnTag, X3 = tag
			 *  X2 in S1,
				intset( TX2, 1, 5 ), #( TX2, 1 ), X2 in TX2,
				
				X3 #:: [ 1 .. 6 ], X3 notin S1,
				intset( TX3, 1, 6 ), #( TX3, 1 ), X3 in TX3,
				
				S2 = (( S1 \ TX2 ) \/ TX3),
			 */
			
			ecl				
			.append( "% вытесняемый тег").append( eoln )
			.append( vytesnIndex ).append( " in " ).append( tlb_latestSetVar ).append( "," ).append( eoln );
			
			StringBuffer latestNHits = tagsVersions.newVar();
			
			ecl.append( "lru:latestNHits( ")
				.append( latestNHits ).append( ", " )
				.append( tlb.getBufferSize() - 1 ).append( ", " )
				.append( hitSetsStructure )
			.append( ")," ).append( eoln )
			.append( "( foreach( NH, " ).append( latestNHits ).append( " ),").append( eoln )
				.append( "  param( " ).append( vytesnIndex ).append( " )").append( eoln )
				.append( "do" ).append( eoln )
				.append( vytesnIndex ).append( " #\\= NH" ).append( eoln )
			.append( ")," ).append( eoln );
			
			ecl
			.append( "lru:addVytesnTag( ")
				.append( vytesnIndex ).append( ", " )
				.append( tlb_latestSetVar ).append( ", " )
				.append( hitSetsStructure ).append( ", " )
				.append( vytesnStructure )
			.append( ")," ).append( eoln )
			
			.append( "intset( " ).append( vytesnTagSet ).append( ", 1, " ).append( tlb.getSize() ).append( " )," )
			.append( "#( " ).append( vytesnTagSet ).append( ", 1 )," )
			.append( vytesnIndex ).append( " in " ).append( vytesnTagSet ).append( "," ).append( eoln )
			
			.append( eoln )
			
			.append( "% тег - причина промаха").append( eoln )
			.append( index ).append( " #:: [ 1 .. ").append( tlb.getSize() ).append(" ], " )
			.append( index ).append( " notin " ).append( tlb_latestSetVar ).append( "," ).append( eoln )
			.append( "intset( " ).append( tagset ).append( ", 1, ").append( tlb.getSize() ).append( " ), " )
			.append( "#( " ).append( tagset ).append( ", 1 ), ")
			.append( index ).append( " in " ).append( tagset ).append( "," ).append( eoln )
			.append( eoln );

			StringBuffer nextSetVar = tagsVersions.newVar();
			ecl.append( nextSetVar ).append( " = (( " )
				.append( tlb_latestSetVar ).append( " \\ " ).append( vytesnTagSet )
				.append(" ) \\/ ").append( tagset )
			.append( ")," ).append( eoln )
			.append( "lru:addHit( " )
				.append( index ).append( ", " )
				.append( tagset ).append( ", " )
				.append( nextSetVar ).append( ", " )
				.append( hitSetsStructure )
			.append( ")," ).append( eoln )
			.append( eoln );
			

			vytesnTags.add( vytesnIndex );
		    missTags.add( index );
			return nextSetVar.toString();
		}
		else
			return tlb_latestSetVar;
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
     * @param falseVirtualAddresses 
     * @param storeCommands 
     * @param setsNumbers	сеты, задействованные этой командой в каждом кэш-уровне
     * @param collectors	коллекторы, задействованные этой командой в каждом кэш-уровне и сете
     * @return
     * @throws IOException
     * @throws RecognitionException
     */
    private StringBuffer falseCommandlikeTranslate( 
    		  final Command command
    		, final Scheme scheme
    		, Map<String, Integer> varVersions
    		, StringBuffer ecl
    		, VarsController tagsVersions
    		, List<Cache> cacheLevels
    		, TLB tlb
    		, Set<Command> storeCommands
    		, Set<Command> dataCommands
    		, Map<Command, String> falseVirtualAddresses
    	)
    	throws IOException, RecognitionException
    {
		String pathToTSL = sourcePath + "\\" + command.getCop() + "\\" + command.getTestSituation() + ".tsl";
		CharStream inputStream = new ANTLRFileStream( pathToTSL );
		TeSLaLexer lexer = new TeSLaLexer( inputStream );
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		TeSLaParser parser = new TeSLaParser(tokens);
		String prefix = command.getCop() + "@" + command.getTestSituation() + "#" + tagsVersions.newVar().toString();
		TeSLaParser.program_return prog = parser.program( 
				  command.getArgs()
				, scheme
				, command
				, prefix
				, cacheLevels
				, tlb
				, command.getTLBSituation()
				, ( tlb != null ? tlb.getPhysicalAddressBitLen() : ( ! cacheLevels.isEmpty() ? cacheLevels.get(0).getAddressBitLength() : 0 ))
			);

		// call test situation predicate
		ecl.append( "'" + prefix + "::main'( _" );
		for( String arg : command.getArgs() )
		{
			ecl.append( ", _" + varVersions.get( arg ) + arg );
		}
		Set<String> changedArgs = new HashSet<String>();
		for( int i = 0; i < command.getArgs().size(); i++ )
		{
			String arg = command.getArgs().get(i);
			LogicalVariable logicalVar = prog.signature.get(i);
			Integer version = varVersions.get( arg );
			if ( logicalVar.getStatus() == Status.SIGNATURE_RESULT )
			{
				version++;
				changedArgs.add( arg );
			}
			ecl.append( ", _" + version + arg );
		}
		if ( prog.hasAddressTranslation )
		{
			if ( ! command.getTestSituationParameters().containsKey("AddressTranslation") )
			{
				throw new Error("Test situation for 'AddressTranslation' is not find in " + command.getCop());
			}
			
			if ( tlb == null )
			{
				throw new Error( "Define TLB for using AddressTranslation" );
			}
			
			String vAddr = tagsVersions.newVar().toString();		
			ecl.append( ", " + vAddr + ", _");
			falseVirtualAddresses.put( command, vAddr );
		}
		if ( prog.memoryOperation != null )
		{
			switch( prog.memoryOperation )
			{
			case LOAD:
				if ( ! command.getTestSituationParameters().containsKey("LoadMemory") )
				{
					throw new Error("Test situation for LoadMemory is not find in " + command.getCop());
				}
				break;
			case STORE:
				if ( ! command.getTestSituationParameters().containsKey("StoreMemory") )
				{
					throw new Error("Test situation for memory operation is not find in " + command.getCop());
				}
				break;
			}
			ecl.append( ", _, _" );
		}
		ecl.append( ")," ).append( eoln );
		
		ecl.append( eoln );
		
		for( String var : changedArgs )
		{
			Integer version = varVersions.get( var );
			varVersions.put( var, version + 1 );
		}
		
		if ( prog.hasAddressTranslation )
		{
			if ( prog.isStoreAddressTranslation )
			{
				storeCommands.add(command);
			}
			
			if ( prog.isDataTLBused )
			{
				dataCommands.add(command);
			}
		}
		
		return prog.eclipseProgram;
    }
    
    private void load_store( 
    		  StringBuffer ecl
    		, Scheme scheme
    		, ConstraintManager constraintManager
    		, ArgumentsManager argManager
    		, Set<Command> normalCommands
    		, VarsController versions
    		, Map<Command, Map<Cache, String>> tagVars
    		, Map<Command, Map<Cache, String>> setVars
    		, Map<Command, String> indexVars
    		, Map<Command, String> values
    		, int physicalAddressBitLen
    	)
    {
    	Map<List<Command>, String> physDiffVars = new HashMap<List<Command>, String>();
    	List<Command> reverseMemoryCommands = buildReverse(scheme, normalCommands);
    	
    	// 1. define difference flag for each pair of physical addresses
    	ecl.append( "[ 0" );
    	Set<Command> viewed = new HashSet<Command>();
    	for( Command cmd1 : reverseMemoryCommands )
    	{
    		viewed.add(cmd1);
    		for( Command cmd2 : reverseMemoryCommands )
    		{
    			if ( viewed.contains(cmd2) )
    				continue;
    			//create diff
    			String diff = versions.newVar().toString();
    			ecl.append( ", " ).append( diff );
    			physDiffVars.put( Arrays.asList(cmd2, cmd1), diff );
    			constraintManager.addDifferenceVar(
    					  argManager.getArgument(cmd2.getPhysicalAddress())
    					, argManager.getArgument(cmd1.getPhysicalAddress())
    					, diff
    				);
    		}
    		ecl.append( eoln );
    	}
    	ecl.append( "] #:: [ 0 .. 1 ]," ).append( eoln );
    	
    	// 2. build and write static constraints on these flags
    	Map<String, Integer> staticConstraints = 
    			constraintManager.getStaticConstraints();
    	
    	for( String name : staticConstraints.keySet() )
    	{
    		if ( physDiffVars.containsValue(name) )
    			ecl.append( name + " = " + staticConstraints.get(name) + "," + eoln );
    	}
    	
    	// 3. labeling of flags
    	ecl.append( "labeling( [ 0" );
    	for( String name : physDiffVars.values() )
    	{
    		ecl.append( ", " + name );
    	}
    	ecl.append( "] )," ).append( eoln );
    	
    	// 4. write dynamic constraints on physForMemoryDiff
    	//TODO записать в это место ВСЕ динамические ограничения ??
    	// или как-то выделить динам.ограничения на эти diff?
    	// а если это завязано на другие переменные?..
    	ecl.append( constraintManager.getDynamicConstraints() );
    	
    	// 5. LOAD-STORE model on flags
    	viewed.clear();
    	// phys +> all load-store diffs with it
    	for( Command cmd1 : reverseMemoryCommands )
    	{
    		viewed.add(cmd1);
    		if ( ! cmd1.isLOAD() )
    			continue;
    		int bracesCount = 0;
        	// 5a: LOAD-STORE
    		for( Command cmd2 : reverseMemoryCommands )
    		{
    			if ( viewed.contains(cmd2) )
    				continue;
    			if ( ! cmd2.isSTORE() )
    				continue;
    			
    			//TODO STORE-инструкция меняет не просто часть, а весь регистр целиком?
    			
    			physAddrAndValuesIntersection(ecl, versions, values,
						physicalAddressBitLen, cmd1, cmd2);
    			bracesCount++;
    		}
        	// 5b: LOAD-LOAD
    		for( Command cmd2 : reverseMemoryCommands )
    		{
    			if ( viewed.contains(cmd2) )
    				continue;
    			if ( ! cmd2.isLOAD() )
    				continue;
    			
    			physAddrAndValuesIntersection(ecl, versions, values,
						physicalAddressBitLen, cmd1, cmd2);
    			
    			bracesCount++;
    		}
    		ecl.append( "true" );
    		for( int i = 0; i < bracesCount; i++ )
    		{
    			ecl.append( ")");
    		}
    		ecl.append(",").append( eoln );
    	}
    	
    	// 6. translate flags to tags, sets, indexes difference
    	for( List<Command> diffCmds : physDiffVars.keySet() )
    	{
    		String diffVar = physDiffVars.get( diffCmds );
    		Command cmd1 = diffCmds.get(0);
    		Command cmd2 = diffCmds.get(1);
    		ecl.append( "( " ).append( diffVar ).append( " = 1 -> " );
    		int tagPset = 0;
    		for( Cache cache : tagVars.get(cmd1).keySet() )
    		{
	    		// определить теги
	    		String tag1 = tagVars.get(cmd1).get(cache);
	    		String tag2 = tagVars.get(cmd2).get(cache);
	    		// определить сеты
	    		String set1 = setVars.get(cmd1).get(cache);
	    		String set2 = setVars.get(cmd2).get(cache);
	    		ecl
	    		.append( tag1 + " #= " + tag2 + ", " )
	    		.append( set1 + " #= " + set2 + ", " );
	    		tagPset = cache.getTagBitLength() + cache.getSetNumberBitLength();
    		}
    		// определить индексы
    		ecl.append( "numbers:equal( " + 
    				indexVars.get(cmd1) + ", " + 
    				indexVars.get(cmd2) + ", " +
    				( physicalAddressBitLen - tagPset )
    				+ ") ;"
    			);

    		for( Cache cache : tagVars.get(cmd1).keySet() )
    		{
	    		// определить теги
	    		String tag1 = tagVars.get(cmd1).get(cache);
	    		String tag2 = tagVars.get(cmd2).get(cache);
	    		// определить сеты
	    		String set1 = setVars.get(cmd1).get(cache);
	    		String set2 = setVars.get(cmd2).get(cache);
	    		ecl
	    		.append( tag1 + " #\\= " + tag2 + "; " )
	    		.append( set1 + " #\\= " + set2 + "; " );
    		}
    		// определить индексы
    		ecl.append( "numbers:notequal( " +
    				indexVars.get(cmd1) + ", " + 
    				indexVars.get(cmd2) + ", " +
    				( physicalAddressBitLen - tagPset ) +
    				" ) )," )
    		.append( eoln );
    	}
    }

	private void physAddrAndValuesIntersection(StringBuffer ecl,
			VarsController versions, Map<Command, String> values,
			int physicalAddressBitLen, Command cmd1, Command cmd2) {
		String value1 = values.get(cmd1);
		String value2 = values.get(cmd2);
		int s = Math.max(cmd1.getMemoryValueSize(), cmd2.getMemoryValueSize());
		int min = Math.min( cmd1.getMemoryValueSize(), cmd2.getMemoryValueSize() );
		// phys1[physBitLen .. s] = phys2[physBitLen .. s] -> val1 = val2
		ecl.append( "( numbers:boundedEqual( " )
			.append( cmd1.getPhysicalAddress() ).append( ", " )
			.append( cmd2.getPhysicalAddress() ).append( ", " )
			.append( physicalAddressBitLen ).append( ", " )
			.append( physicalAddressBitLen - 1 ).append( ", " )
			.append( s )
		.append( " ) -> " );
		// равенство пересечений
		if ( s == min )
		{
			ecl.append( "numbers:equal( " )
				.append( value1 ).append( ", " )
				.append( value2 ).append( ", " )
				.append( s )
			.append( " );").append( eoln );
		}
		else
		{
			if ( cmd1.getMemoryValueSize() > cmd2.getMemoryValueSize() )
				valuesIntersection(
						ecl, versions, physicalAddressBitLen,
						value1, value2, s, min);
			else
				valuesIntersection(
						ecl, versions, physicalAddressBitLen,
						value2, value1, s, min);    					
		}
	}

	private void valuesIntersection(StringBuffer ecl, VarsController versions,
			int physicalAddressBitLen, String value1, String value2, int s,
			int min) {
		// пусть они отличаются на N бит 
		// => value[valuelen-1..0] делится на 2^N интервалов, 
		// один интервал соответствует месту для val2 внутри val1
		// а соответствие определяется значащими в phys2 битами
		// phys2[s-1..cmd2.valuelen] = 1 -> val1[..] = val2
		ecl.append( "( " );

		String phys2Part = versions.newVar().toString();
		ecl.append( "numbers:getbits( " )
			.append( "[ " ).append( phys2Part ).append( "], " )
			.append( min ).append( ", " )
			.append( physicalAddressBitLen ).append( ", " )
			.append( s-1 ).append( ", " )
			.append( min )
		.append( " )," ).append( eoln );
		
		int partsCount = (int)Math.pow( 2, s - min );
		int value1len = (int)Math.pow( 2, s + 3 );
		int partLen = value1len / partsCount;
		int brCount = 0;
		for( int partN = partsCount-1; partN >= 0; brCount++,partN-- )
		{
			// TODO как-то учесть BigEndian...
			int value1EndBit = (partN + 1) * partLen - 1;
			int value1StartBit = partN * partLen;
			// ( phys2Part #= partN -> getbits( value2, value1, endbit, startbit ) ; 
			ecl.append( "( " ).append( phys2Part ).append( " #= " ).append( partN ).append( " -> " ).append( eoln );
			ecl.append( "numbers:getbits( ")
				.append( value2 ).append( ", " )
				.append( value1 ).append( ", " )
				.append( value1len ).append( ", " )
				.append( value1EndBit ).append( ", " )
				.append( value1StartBit )
			.append( " ) ; ").append( eoln );    					
		}
		ecl.append( "true");
		for( ; brCount > 0; brCount-- )
		{
			ecl.append( " )" );
		}
		ecl.append( "),").append( eoln );
	}
    
    private List<Command> buildReverse( Scheme scheme, Set<Command> memory )
    {
    	List<Command> initial = scheme.getCommands();
    	List<Command> result = new ArrayList<Command>();
    	for( int i = initial.size() - 1; i >= 0; i-- )
    	{
    		Command cmd = initial.get(i);
    		if ( memory.contains(cmd) )
    			result.add(cmd);
    	}
    	return result;
    }
    
    private void readConstraintsFromTemplate(
    			Scheme scheme,
    			ConstraintManager constraintManager,
    			ArgumentsManager argManager,
    			final VarsController tagNames,
    			final List<Cache> cacheLevels
    		)
    {
    	for( final Command c : scheme.getCommands() )
    	{
    		Map<String, Set<ProcedureTestSituation>> tsParams = c.getTestSituationParameters();
    		Set<ProcedureTestSituation> memts = new HashSet<ProcedureTestSituation>();
    		if ( c.isLOAD() )
    		{
    			memts.addAll( tsParams.get( "LoadMemory" ) );
    		}
    		if ( c.isSTORE() )
    		{
    			memts.addAll( tsParams.get( "StoreMemory" ) );
    		}
    		if ( tsParams.containsKey( "AddressTranslation" ) )
    		{
    			memts.addAll( tsParams.get( "AddressTranslation" ) );
    		}
    			
			for( final ProcedureTestSituation ts : memts )
			{
				if ( ts instanceof CacheHit )
				{
					final CacheHit hit = (CacheHit)ts;
					argManager.addArgument( 
							new ru.teslaprj.constraints.args.Set(){

								@Override
								public Cache getCache() {
									return cacheLevels.get( hit.getLevel() );
								}

								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public String getName() {
									return notnull( hit.getSetVar(), tagNames );
								}} 
						);
					argManager.addArgument( 
							new ru.teslaprj.constraints.args.Tag(){
								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public int getLevel() {
									return hit.getLevel();
								}

								@Override
								public String getName() {
									return notnull( hit.getTagVar(), tagNames );
								}}
						);
				}
				else if ( ts instanceof CacheMiss )
				{
					final CacheMiss miss = (CacheMiss)ts;
					argManager.addArgument(
							new ru.teslaprj.constraints.args.Set(){

								@Override
								public Cache getCache() {
									return cacheLevels.get( miss.getLevel() );
								}

								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public String getName() {
									return notnull( miss.getSetVar(), tagNames );
								}}
						);
					argManager.addArgument(
							new ru.teslaprj.constraints.args.Tag(){
								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public int getLevel() {
									return miss.getLevel();
								}

								@Override
								public String getName() {
									return notnull( miss.getTagVar(), tagNames );
								}}
    					);
					// TODO вытесненный тег!!! constraint: vytesn = p
				}
				else if ( ts instanceof TLBHit )
				{
					argManager.addArgument(
							new VirtualAddress(){
								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public String getName() {
									return notnull( ((TLBHit)ts).getVirtualAddressVar(), tagNames );
								}}
						);
					argManager.addArgument(
							new PhysicalAddressAfterTranslation(){
								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public String getName() {
									return notnull( ((TLBHit)ts).getPhysicalAddressVar(), tagNames );
								}}
						);
				}
				else if ( ts instanceof TLBMiss )
				{
					argManager.addArgument(
							new VirtualAddress(){
								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public String getName() {
									return notnull( ((TLBMiss)ts).getVirtualAddressVar(), tagNames );
								}}
						);
					argManager.addArgument(
							new PhysicalAddressAfterTranslation(){
								@Override
								public Command getCommand() {
									return c;
								}

								@Override
								public String getName() {
									return notnull( ((TLBMiss)ts).getPhysicalAddressVar(), tagNames );
								}}
							);
				}
			}
    	}
    	
    	for( Assert a : scheme.getAsserts() )
    	{
    		if ( (a.getTestSituation().equals("=") || a.getTestSituation().equals("#"))
    				&& a.getArgs().size() == 2 )
    		{
    			Argument first = argManager.getArgument( a.getArgs().get(0) );
    			Argument second = argManager.getArgument( a.getArgs().get(1) );
    			// TODO проверить, что эти аргументы одного динамического типа
    			// что у них одинаковый битовый размер
    			// и если это теги, сеты, то что еще они относятся к одному уровню кэша
    			// Если что-нибудь из этого не выполнено, выдавать ошибку!
    			// проверить, возможен ли тут уровень0 для тегов-сетов!
    			Constraint c = new Constraint (
    					( a.getTestSituation().equals( "=" ) ? Relation.EQ : Relation.NEQ ),
    					first, 
    					second );
    			constraintManager.add( c );
    		}
    		//TODO else выдать ошибку, что в таком виде спец.ограничения не описывают!
    	}
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