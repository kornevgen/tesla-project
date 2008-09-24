package ru.teslaprj.arithm;

import java.util.HashMap;
import java.util.Map;

public class PredicatesController 
{
	private int predicateNumber = 0;
	
	private static final String eoln = System.getProperty( "line.separator" );

	public class BoolExprPredicate
	{
		private StringBuffer predicateName;
		private StringBuffer predicateBody;
	
		public BoolExprPredicate( final StringBuffer body, final StringBuffer parameters )
		{
			predicateName = new StringBuffer()
				.append("'")
				.append( predicateNumber++ )
				.append( "'" );
			
			predicateBody = new StringBuffer()
				.append( predicateName )
				.append( parameters )
				.append( " :-" )
				.append( eoln )
				.append( body )
				.append( "." );
		}
		
		public String getName()
		{
			return predicateName.toString();
		}
	}
	
	private Map<StringBuffer, BoolExprPredicate> predicates = new HashMap<StringBuffer, BoolExprPredicate>();
	
	public StringBuffer newPredicate( final StringBuffer body, final StringBuffer parameters )
	{
		BoolExprPredicate predicate = new BoolExprPredicate( body, parameters );
		predicates.put( predicate.predicateName, predicate );
		return predicate.predicateName;
	}
	
	public StringBuffer printPredicates()
	{
		StringBuffer result = new StringBuffer();
		for( BoolExprPredicate predicate : predicates.values() )
		{
			result	.append( eoln )
					.append( predicate.predicateBody )
					.append( eoln );
		}
		
		return result;
	}
}
