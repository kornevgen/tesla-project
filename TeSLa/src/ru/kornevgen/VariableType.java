package ru.kornevgen;

public enum VariableType
{
	INT
	{
		public StringBuffer getIsPredicate( final String varName )
		{
			return new StringBuffer( "numbers:is_int( " ).append( varName ).append( " )");
		}
		
		public Integer size()
		{
			return 32;
		}
	}
	,
	LONG
	{
		public StringBuffer getIsPredicate( final String varName )
		{
			return new StringBuffer( "numbers:is_long( " ).append( varName ).append( " )");
		}
		
		public Integer size()
		{
			return 64;
		}
	}
	,
	NUMBER
	{
		public StringBuffer getIsPredicate( final String varName )
		{
			return new StringBuffer( "true" );
		}
		
		public Integer size()
		{
			return null;
		}
	}
	;
	
	public abstract StringBuffer getIsPredicate( final String varName );
	public abstract Integer size();
}
