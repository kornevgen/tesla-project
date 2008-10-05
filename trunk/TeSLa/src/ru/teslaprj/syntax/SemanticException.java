package ru.teslaprj.syntax;

import org.antlr.runtime.Token;

public class SemanticException extends RuntimeException
{
	private static final long serialVersionUID = -4479856257644853038L;

	public SemanticException( Token token, String message )
	{
		super( (token != null ? "line " + token.getLine() + ": " : "") + message );
	}
}

class UndefinedVariable extends SemanticException
{
	private static final long serialVersionUID = -72579923714691043L;

	public UndefinedVariable( Token token, String name )
	{
		super( token, "Undefined variable '" + name + "'" );
		
	}
}

class ConstantIsTooLarge extends SemanticException
{
	private static final long serialVersionUID = -3217879269543877574L;

	public ConstantIsTooLarge( Token token, String constant )
	{
		super( token, "constant " + constant + " is too large" );
	}
}