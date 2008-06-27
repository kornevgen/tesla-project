package ru.kornevgen;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.Token;

public class VarsController
{
	private static final String eoln = System.getProperty( "line.separator" );
	
	private Map<String, LogicalVariable> vars = new LinkedHashMap<String, LogicalVariable>();
	
	public StringBuffer createSignature()
	{
		StringBuffer sb = new StringBuffer();
		
		sb.append(":- export go/" + (vars.size() + 1) + "." + eoln);
		sb.append("go( _");
		for( LogicalVariable var : vars.values() )
		{
			sb.append( ", _" ).append( var.getCanonicalName() );
		}
		sb.append( " ) :-" ).append( eoln );
		
		// conformance of string representation and number representation
		for( LogicalVariable var : vars.values() )
		{
			sb.append( "numbers:nstring2nlist( _" ).append( var.getCanonicalName() )
			.append( ", " ).append( var.current() )
			.append( ", " ).append( var.size )
			.append( "),").append( eoln );
		}
		
//		sb.append("go1( _" );
//		for( LogicalVariable var : vars.values() )
//		{
//			sb.append(", " ).append( var.current() );
//		}
//		sb.append( ")." ).append( eoln ).append( eoln );
//		
//		sb.append("go1( _");
//		for( LogicalVariable var : vars.values() )
//		{
//			sb.append( ", " ).append( var.current() );
//		}
//		sb.append( " ) :-" ).append( eoln );

		return sb;
	}
		
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

	public StringBuffer printMedians( List<LogicalVariable> parameters )
	{
		StringBuffer result = new StringBuffer();
		
		for( LogicalVariable var : parameters )
		{
			result	.append( "numbers:random_result( " ).append( var.getBaseName() ).append( " )," )
					.append( eoln );
		}
		
		for( LogicalVariable var : parameters )
		{
			result	
			.append( "numbers:nlist2nstring( " )
				.append( var.getBaseName() ).append( ", _" )
				.append( var.getCanonicalName()).append(", ")
				.append( var.size )
			.append( " )," ).append( eoln );
		}
		
		return result;
	}

	/** return "(x, y, z)" when x, y, z - all known variables */
	public StringBuffer getAllVarsAsParameters()
	{
		StringBuffer result = new StringBuffer( "( " );
		boolean isFirstArgument = true;
		for( LogicalVariable var : vars.values() )
		{
			if ( isFirstArgument )
				isFirstArgument = false;
			else
				result.append( ", " );
				
			result.append( var.current() );
		}
		return result.append( " ) " );
	}
	
	public void addVar( final String name, final int size )
	{
		vars.put( name, new LogicalVariable( name, size ) );
	}
	
	private int newVarNumber = 0;
	public StringBuffer newVar()
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
}
