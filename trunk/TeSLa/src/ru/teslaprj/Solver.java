package ru.teslaprj;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
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
import ru.teslaprj.syntax.BitLen;
import ru.teslaprj.syntax.LogicalVariable;
import ru.teslaprj.syntax.TeSLaLexer;
import ru.teslaprj.syntax.TeSLaParser;
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
		// 0. check correctness of `scheme`
		checkVarsKnowness( scheme );
		
		// 1. translate `scheme` to .clp
		String moduleName = createTempModuleName();
		File tmp = File.createTempFile( moduleName, ".ecl", libPath );
		moduleName = tmp.getName();
		moduleName = moduleName.substring(0, moduleName.length() - 4 );

		try
		{
			writeFile( tmp, translate( scheme, moduleName ) );
			
			// 2. run ECLiPSe and get results
			CompoundTerm result = callECLiPSe( tmp, moduleName, scheme.getDefinedNames() );
			
			// 3. analyze results
			return generateValues( result, scheme );
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

    private StringBuffer translate( Scheme scheme, String moduleName )
    	throws SchemeDefinitionError, IOException, RecognitionException
    {
    	StringBuffer ecl = new StringBuffer();
		
		ecl.append(":- module( " ).append( moduleName ).append( " )." ).append(eoln);
		ecl.append(":- lib( ic ).").append(eoln);
		ecl.append(":- use_module( numbers ).").append(eoln);
		ecl.append(":- use_module( predicates ).").append(eoln);		
		ecl.append(eoln);

		StringBuffer commandPredicates = new StringBuffer();
    	
    	// сгенерировать предикат go: последовательность вызовов унифицированных "предикатов go"
		List<String> names = scheme.getDefinedNames();
		ecl.append( ":- export go/" ).append( names.size() + 1 ).append( "." ).append(eoln).append(eoln);
    	ecl.append( "go( _"); 
		for( String name : names )
		{
			ecl.append( ", _0" + name );
		}
    	ecl.append(") :- ").append( eoln );
    	
    	Map<String, Integer> varVersions = new HashMap<String, Integer>();
    	for( String name : names )
    	{
    		varVersions.put( name, 0 );
    	}
    	
    	for( Assert asert : scheme.getAsserts() )
    		commandPredicates.append( commandlikeTranslate( asert, scheme, varVersions, ecl ) );
    	
    	for( Command command : scheme.getCommands() )
    		commandPredicates.append( commandlikeTranslate( command, scheme, varVersions, ecl ) );
    	
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
    
    private StringBuffer commandlikeTranslate( 
    		  final Commandlike command
    		, final Scheme scheme
    		, Map<String, Integer> varVersions
    		, StringBuffer ecl
    	)
    	throws IOException, RecognitionException
    {
		String pathToTSL = sourcePath + "\\" + command.getCop() + "\\" + command.getTestSituation() + ".tsl";
		CharStream inputStream = new ANTLRFileStream( pathToTSL );
		TeSLaLexer lexer = new TeSLaLexer( inputStream );
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		TeSLaParser parser = new TeSLaParser(tokens);
		String prefix = command.getCop() + "@" + command.getTestSituation() + "#" + nextInt() ;
		TeSLaParser.program_return prog = parser.program( command.getArgs(), command.getAdditionalArgs(), scheme, prefix );

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
		ecl.append("),").append(eoln);
		for( String var : changedVars )
		{
			Integer version = varVersions.get( var );
			varVersions.put( var, version + 1 );
		}
		return prog.eclipseProgram;
    }

}