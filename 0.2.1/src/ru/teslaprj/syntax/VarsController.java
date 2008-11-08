package ru.teslaprj.syntax;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.Token;


public class VarsController
{
	private static final String eoln = System.getProperty( "line.separator" );
	
	private Map<String, LogicalVariable> vars = new LinkedHashMap<String, LogicalVariable>();
	private List<LogicalVariable> signature = new ArrayList<LogicalVariable>();
			
	public StringBuffer types()
	{
		StringBuffer result = new StringBuffer();
		
		for( LogicalVariable var : vars.values() )
		{
			result.append( getSizePredicate( var ) );
		} 
		
		return result;
	}
	
	public StringBuffer getSizePredicate( LogicalVariable var )
	{
		return new StringBuffer( "numbers:sizeof( " )
		.append( var.current() ).append( ", " ).append( var.size )
		.append( ")," ).append( eoln );
	}

	/** return "(x, y, z)" when x, y, z - all known variables */
	public StringBuffer getAllVarsAsParameters()
	{
		return getAllVarsAsParametersWithStatus( new LogicalVariable.Status[]{} );
	}
	
	public StringBuffer getAllVarsAsParametersWithStatus( LogicalVariable.Status... status )
	{
		StringBuffer result = new StringBuffer();
		boolean isFirstArgument = true;
		for( LogicalVariable var : vars.values() )
		{
			if ( status.length > 0 && ! Arrays.asList( status ).contains( var.getStatus() ) )
				continue;
			
			if ( isFirstArgument )
				isFirstArgument = false;
			else
				result.append( ", " );
				
			result.append( var.current() );
		}
		return result;
	}
	
	public void addVar( final String name, final int size, final LogicalVariable.Status status, final boolean toSignature )
	{
		LogicalVariable var = new LogicalVariable( name, size, status );
		vars.put( name, var );
		if ( toSignature )
			signature.add( var );
	}
	
	private int newVarNumber = 0;
	public synchronized StringBuffer newVar()
	{
		return new StringBuffer( "_" ).append( newVarNumber++ );
	}
	
	public String getCurrent( final Token ID, final String name )
	{
		return getVar( ID, name ).current();
	}
	
	public String nextVersion( final Token ID, final String name )
	{
		LogicalVariable var = getVar( ID, name );
		var.nextVersion();
		vars.put( name, var );
		
		return getCurrent( ID, name );
	}
	
	public List<LogicalVariable> getVarsCopy()
	{
		List<LogicalVariable> result = new ArrayList<LogicalVariable>();
		for( LogicalVariable var : vars.values() )
		{
			result.add( var );
		}
		
		return result;
	}
	
	public LogicalVariable getVar( final Token ID, final String name )
	{
		if ( ! vars.containsKey( name ) )
			throw new UndefinedVariable( ID, name );
		
		return vars.get( name );
	}
	
	public boolean isKnown( final String varName )
	{
		return vars.containsKey( varName );
	}
	
	public List<LogicalVariable> getSignature()
	{
		return signature;
	}
}
