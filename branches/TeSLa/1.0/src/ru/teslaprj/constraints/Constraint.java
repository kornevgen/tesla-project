package ru.teslaprj.constraints;

public class Constraint
{
	public static enum Relation
	{
		EQ, NEQ
	}
	
	private Relation relation;
	private java.util.Set<Argument> arguments;
	
	public Constraint( Relation r, Argument a, Argument b )
	{
		relation = r;
		arguments = new java.util.HashSet<Argument>();
		arguments.add( a );
		arguments.add( b );
	}
	
	public Relation getRelation()
	{
		return relation;
	}
	
	public java.util.Set<Argument> getArguments()
	{
		return arguments;
	}
}