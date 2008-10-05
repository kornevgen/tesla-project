grammar TeSLa;

options
{
	language=Java;
}

@header {
package ru.teslaprj.syntax;
}
@lexer::header {
package ru.teslaprj.syntax;
}

@members
{
	final private String eoln = System.getProperty("line.separator");
	
	private class ExpressionResult
	{
		public StringBuffer resultVarName = new StringBuffer();	
		public StringBuffer expressionBody = new StringBuffer();
		public int size;
	}
	
	private class Constant extends ExpressionResult
	{
		public Constant( String value )
		{
			super();
			resultVarName = new StringBuffer( value );
			size = -1;			
		}		
	}
	
	StringBuffer ecl;
	
	VarsController varsc;
	PredicatesController predsc;
	
	String commandPrefix;
	
	@Override
	public void reportError( RecognitionException e )
	{
		throw new ParserException( e.getMessage() );
	}
	
	public class ParserException extends RuntimeException
	{
		public ParserException( String s )
		{
			super( s );
		}
	}
}

program[
      List<String> args
	, List<String> additionalArgs
	, ru.teslaprj.scheme.Scheme scheme
	, String prefix
] returns [StringBuffer eclipseProgram, List<LogicalVariable> signature]
@init {
List<LogicalVariable> parameters = new ArrayList<LogicalVariable>();
StringBuffer preArgs;
ecl = new StringBuffer();
varsc = new VarsController();
predsc = new PredicatesController();
preArgs = null;
}
	: signature
			{
				parameters = varsc.getVarsCopy();
				preArgs = 
					varsc.getAllVarsAsParametersWithStatus( LogicalVariable.Status.SIGNATURE_RESULT, LogicalVariable.Status.SIGNATURE_READONLY );

				if ( args.size() > parameters.size() )
				{
					throw new SemanticException( null, "using more arguments (" + args.size() + ") than test sutiation has (" + parameters.size() + ")");
				}

				if ( args.size() + additionalArgs.size() > parameters.size() )
				{
					throw new SemanticException( null, "using more arguments (" + (args.size() + additionalArgs.size()) + ") than test sutiation has (" + parameters.size() + ")");
				}

				// check args bitlens
				for( int i = 0; i < args.size(); i++ )
				{
					int argLen = scheme.getBitlen( args.get(i) );
					int varLen = parameters.get(i).getBitlen();
					if ( argLen != varLen )
					{
						throw new SemanticException( null, "bitlens of variable and argument are not equal: " + argLen + " and " + varLen );
					}
				}
				// TODO check actual names of results var (must different)
				
				commandPrefix = prefix;
			}
		(operator)*
			{
//				ecl	.append( varsc.printMedians( parameters ) ).append( "true." + eoln)
//					.append( predsc.printPredicates() );
				
				StringBuffer postArgs = varsc.getAllVarsAsParametersWithStatus( LogicalVariable.Status.SIGNATURE_RESULT, LogicalVariable.Status.SIGNATURE_READONLY );
				StringBuffer optional = varsc.getAllVarsAsParametersWithStatus( LogicalVariable.Status.OPTIONAL );
				if ( optional.length() > 0 )
					postArgs.append( ", " );
					
				retval.eclipseProgram = new StringBuffer(
					"'" + prefix + "::main'( _, " )
					.append( preArgs ).append(", ")
					.append( postArgs ).append( optional )
					.append( ") :- ").append( eoln )
					.append( ecl )
					.append( "true." ).append( eoln )
					.append( predsc.printPredicates() ).append(eoln);
				retval.signature = varsc.getSignature();
			}
	;

signature
	: ( var_rule )*
	;

var_rule
@init{ LogicalVariable.Status status = null;}
	: 'VAR'
		( 'RESULT' { status = LogicalVariable.Status.SIGNATURE_RESULT; }
		| 'READONLY' { status = LogicalVariable.Status.SIGNATURE_READONLY; }
		| 'OPTIONAL' { status = LogicalVariable.Status.OPTIONAL; } )
		ID ':' INTEGER 
		{
			varsc.addVar( $ID.text, Integer.parseInt( $INTEGER.text ), status, true );
			ecl.append( varsc.getSizePredicate( varsc.getVar( $ID, $ID.text ) ) );
		}
		';'
	;
	
operator
	: ( assertOperator
	| assignOperator
	) ';'
	;
	
assertOperator
	: 'ASSERT' predicate = boolexpr 
		{ ecl.append( predicate .append( "(" ).append( varsc.getAllVarsAsParameters() ).append("),").append( eoln ));}
	;

assignOperator
	: ID '<-' name = expr 
		{
			String id;
			// each variable can't change its size!!!
			if ( varsc.isKnown( $ID.text ) )
			{
				if ( name.size != varsc.getVar( $ID, $ID.text ).size )
				{
					throw new SemanticException( $ID, "Incompatible sizes: size of " + $ID.text + " is " +  varsc.getVar( $ID, $ID.text ).size + " bit(s) and size of expression is " + name.size + "; these sizes must be the same" );
				}
				
				if ( varsc.getVar( $ID, $ID.text ).getStatus() == LogicalVariable.Status.SIGNATURE_READONLY )
				{
					throw new SemanticException( $ID, "Cannot change readonly variable: " + $ID.text );
				}
				
				id = varsc.nextVersion( $ID, $ID.text );
			}
			else
			{
				varsc.addVar( $ID.text, name.size, LogicalVariable.Status.LOCAL, false );
				id = varsc.getCurrent( $ID, $ID.text );			
			}

			ecl.append( varsc.getSizePredicate( varsc.getVar( $ID, $ID.text ) ) )
			.append( name.expressionBody )
			.append( id ).append( " = " ).append( name.resultVarName ).append( "," ).append( eoln);
		}
	;

	
///////////// EXPRESSIONS ///////////////////////////
boolexpr returns [StringBuffer predicateName]
	: name1 = orexpr { predicateName = name1; }
	;
	
orexpr returns [StringBuffer predicateName]
	: name1 = andexpr 
	( { predicateName = name1; }
	| 'OR' name2 = orexpr 
		{
			predicateName = predsc.newPredicate(
						commandPrefix, 
						name1.append("(").append( varsc.getAllVarsAsParameters() )
						.append( "); " )
						.append( name2 ).append( "(" )
						.append( varsc.getAllVarsAsParameters() ).append(")"),
						varsc.getAllVarsAsParameters().insert(0, "(").append(")")
					); 
		}
	)
	;

andexpr returns [StringBuffer predicateName]
	: name1 = boolexpr_brackets
	( { predicateName = name1; }
	| 'AND' name2 = andexpr 
		{
			predicateName = predsc.newPredicate(
						commandPrefix, 
						name1.append("(").append( varsc.getAllVarsAsParameters() )
						.append( "), " )
						.append( name2 ).append("(")
						.append( varsc.getAllVarsAsParameters() ).append(")"),
						varsc.getAllVarsAsParameters().insert(0, "(").append(")") 
					); 
		}
	)
	;
	
boolexpr_brackets returns [StringBuffer predicateName]
options { backtrack=true; }
@init{ Token first = input.LT(1); }
	: ID '(' pars = parameters ')'
		{
			predicateName = predsc.newPredicate(
				commandPrefix, 
				new StringBuffer( "predicates:'" ).append( $ID.getText() )
				.append( "'( " ).append( pars ).append( " )" ),
				varsc.getAllVarsAsParameters().insert(0, "(").append(")")
			);
		}
	| '(' be = boolexpr ')' { predicateName = be; }
	| name1 = expr pred = comparesign name2 = expr
	{
			if ( name1.size < 0 )
			{
				if ( name2.size > 0 )
				{
					if ( BitLen.bitlen( name1.resultVarName ) > name2.size )
						throw new SemanticException( first, "size of constant " + name1.resultVarName + " must be not greater than " + name2.size ); 
						
					name1.size = name2.size;
					name1.resultVarName = BitLen.number2nlist( name1.resultVarName, name1.size );
				}
				else //both arguments are constants
				{
					// do calculations and return
					return predsc.newPredicate(
						commandPrefix, 
						pred.bitlenMethod.c( name1.resultVarName, name2.resultVarName ),
						varsc.getAllVarsAsParameters().insert(0,"(").append(")") );
				}
			}
			else // name1.size >= 0
			{
				if ( name2.size < 0 )
				{
					if ( BitLen.bitlen( name2.resultVarName ) > name1.size )
						throw new SemanticException( first, "size of constant " + name2.resultVarName + " must be not greater than " + name1.size ); 
						
					name2.size = name1.size;
					name2.resultVarName = BitLen.number2nlist( name2.resultVarName, name2.size );
				}				 
			}
		
		assert name1.size > 0 && name2.size > 0;
		
		if ( name1.size != name2.size )
			throw new SemanticException( first, "operand sizes are different (" + name1.size + " and " + name2.size + "); must be the same" );
			
		StringBuffer body = name1.expressionBody.append( name2.expressionBody )
		.append( pred.predicateName ).append( "( " )
			.append( name1.resultVarName ).append( ", " )
			.append( name2.resultVarName ).append( ", " )
			.append( name1.size )
		.append( " )");

		predicateName = predsc.newPredicate( commandPrefix, body, varsc.getAllVarsAsParameters().insert(0,"(").append(")") );
	}
	;

// return variable with evaluation result
expr returns [ExpressionResult name]
	: name1 = addexpr { name = name1; }
	;
	
addexpr returns [ExpressionResult name]
@init{ Token first = input.LT(1); }
	: name1 = mulexpr 
	 	( { name = name1; }
		| pred = addsign name2 = addexpr
		{
			if ( name1.size < 0 )
			{
				if ( name2.size > 0 )
				{
					if ( BitLen.bitlen( name1.resultVarName ) > name2.size )
						throw new SemanticException( first, "size of constant " + name1.resultVarName + " must be not greater than " + name2.size ); 
						
					name1.size = name2.size;
					name1.resultVarName = BitLen.number2nlist( name1.resultVarName, name1.size );
				}
				else //both arguments are constants
				{
					// do calculations and return
					name = new Constant( pred.bitlenMethod.c( name1.resultVarName, name2.resultVarName ).toString() );
					return name;
				}
			}
			else
			{
				if ( name2.size < 0 )
				{
					name2.size = name1.size;
					name2.resultVarName = BitLen.number2nlist( name2.resultVarName, name2.size );	
				}
			}
			
			if ( name1.size != name2.size )
				throw new SemanticException( first, "operands have different sizes: " + name1.size + " and " + name2.size + "; must have the same sizes" );
								
			name = new ExpressionResult();
			name.resultVarName = varsc.newVar();
			name.size = name1.size;
			name.expressionBody = 
				name1.expressionBody
				.append( name2.expressionBody )
				
				.append( pred.predicateName ).append( "( " )
					.append( name.resultVarName ).append( ", " )
					.append( name1.resultVarName ).append( ", " )
					.append( name2.resultVarName ).append( ", " )
					.append( name1.size )
				.append( " )," )
				.append( eoln );
		}
		) 
	;

mulexpr returns [ExpressionResult name]
@init{ Token first = input.LT(1); }
	: name1 = arithmterm 
		(  { name = name1; }
		| pred = mulsign name2 = mulexpr
		{
			if ( name1.size < 0 )
			{
				if ( name2.size > 0 )
				{
					if ( BitLen.bitlen( name1.resultVarName ) > name2.size )
						throw new SemanticException( first, "size of constant " + name1.resultVarName + " must be not greater than " + name2.size ); 
						
					name1.size = name2.size;
					name1.resultVarName = BitLen.number2nlist( name1.resultVarName, name1.size );	
				}
				else //both arguments are constants
				{
					// do calculations and return
					name = new Constant( BitLen.mul( name1.resultVarName, name2.resultVarName ).toString() );
					return name;
				}
			}
			else
			{
				if ( name2.size < 0 )
				{
					name2.size = name1.size;
					name2.resultVarName = BitLen.number2nlist( name2.resultVarName, name2.size );
				}
			}
			
			assert name1.size > 0 && name2.size > 0;
			
			if ( name1.size != name2.size )
				throw new SemanticException( first, "operands have different sizes: " + name1.size + " and " + name2.size + "; must have the same sizes" );
				
			name = new ExpressionResult();
			name.resultVarName = varsc.newVar();
			name.size = name1.size;
			name.expressionBody = 
				name1.expressionBody
				.append( name2.expressionBody )
							
				.append( pred ).append( "( " )
					.append( name.resultVarName ).append( ", " )
					.append( name.size ).append( ", " )					
					.append( name1.resultVarName ).append( ", " )
					.append( name2.resultVarName ).append( ", " )
					.append( name1.size )
				.append( " )," )
				.append( eoln );
		}
		) 
	;

arithmterm returns [ExpressionResult name]
	: name1 = bit_expr { name = name1; } 
	;

bit_expr returns [ExpressionResult name]
@init{ Token CT = null; int new_size = 0; }
	: name1 = bit_concat_term { name = name1; }
	| '(' newsize = INTEGER ')' { CT = input.LT(1); } name1 = bit_concat_term
			{
				try
				{
					new_size = Integer.decode( newsize.getText() );
				}
				catch( Exception e )
				{
					throw new SemanticException( newsize, "constants is too large: " + newsize.getText() );
				}

				if ( name1.size < 0 )
				{
					// do calculations
					name = new Constant( BitLen.sign_extend( CT, name1.resultVarName, new_size ).toString() );
					return name;
				}
				
				name = new ExpressionResult();
				name.size = new_size;
				name.resultVarName = varsc.newVar();
				name.expressionBody
					.append( name1.expressionBody )
					.append( "numbers:signExtend( " )
						.append( name.resultVarName ).append( ", " )
						.append( name1.resultVarName ).append( ", " )
						.append( name1.size ).append( ", " )
						.append( newsize.getText() )
					.append( " )," ).append( eoln );
			}
	;
	
bit_concat_term returns [ExpressionResult name]
@init{ Token N1 = input.LT(1); Token N2 = null; }
	: name1 = bit_bound_term  
		(  { name = name1; }
		| '||' { N2 = input.LT(1); } name2 = bit_concat_term
			{
				if ( name1.size < 0 )
				{
					if ( name2.size < 0 )
					{
						// do calculations
						name = new Constant( BitLen.bitconcat( N1, N2, name1.resultVarName, name2.resultVarName ).toString() );
						return name;
					}
					else
					{
						name1.size = BitLen.bitlen( name1.resultVarName );
						name1.resultVarName = BitLen.number2nlist( name1.resultVarName, name1.size );
					}
				}
				else
				{
					if ( name2.size < 0 )
					{
						name2.size = BitLen.bitlen( name2.resultVarName );
						name2.resultVarName = BitLen.number2nlist( name2.resultVarName, name2.size );
					}
				}
				
				name = new ExpressionResult();
				name.resultVarName = varsc.newVar();
				name.size = name1.size + name2.size;
				
				name.expressionBody =
					name1.expressionBody
					.append( name2.expressionBody )
												
					.append( "numbers:concat( " )
						.append( name.resultVarName ).append( ", " )
						.append( name1.resultVarName ).append( ", " )
						.append( name1.size ).append( ", " )
						.append( name2.resultVarName ).append( ", " )
						.append( name2.size )
					.append( " )," )
					.append( eoln );
			}
		)
	;

bit_bound_term returns [ExpressionResult name]
@init{ Token first = input.LT(1); Token powToken = null; Token endToken = null; Token startToken = null; int endValue = 0; int startValue = 0; int powValue = 0; }
	: name1 = bit_term   
		(  { name = name1; }
		| '[' { endToken = input.LT(1); } end = INTEGER 
			{
				try
				{
					endValue = Integer.decode( end.getText() );
				}
				catch( Exception e )
				{
					throw new SemanticException( endToken, "constants is too large: " + end.getText() );
				}
			}
			( ']'
				{
					if ( name1.size < 0 )
					{
						// do calculations
						name = new Constant( BitLen.abit( first, name1.resultVarName, endValue ).toString() );
						return name;
					}
					
					name = new ExpressionResult();
					name.resultVarName = varsc.newVar();
					name.size = 1;
					name.expressionBody
						.append( name1.expressionBody )
						.append( "numbers:getbit( " )
							.append( name.resultVarName ).append( ", " )
							.append( name1.resultVarName ).append( ", " )
							.append( name1.size ).append( ", " )
							.append( end.getText() )
						.append( " )," )
						.append( eoln );
				}
			| '..' { startToken = input.LT(1); } start = INTEGER ']' // bit range
				{
					try
					{
						startValue = Integer.decode( start.getText() );
						if ( endValue < startValue )
						{
							throw new SemanticException( endToken, "End index must be greater than the start one" );
						}
					}
					catch( Exception e )
					{
						throw new SemanticException( startToken, "constants is too large: " + start.getText() );
					}					

					if ( name1.size < 0 )
					{
						// it is a constant expression
						name = new Constant( BitLen.bitrange( first, name1.resultVarName, endValue, startValue ).toString() );
						return name;
					}
					
					name = new ExpressionResult();
					name.resultVarName = varsc.newVar();
					name.size = endValue - startValue + 1;
					name.expressionBody
					.append( name1.expressionBody )
					.append( "numbers:getbits( " )
						.append( name.resultVarName ).append( ", " )
						.append( name1.resultVarName ).append( ", " )
						.append( name1.size ).append( ", " )
						.append( end.getText() ).append( ", " )
						.append( start.getText() )
					.append( " )," )
					.append( eoln );
				}
		)
	 	| '^' { powToken = input.LT(1); } pow = INTEGER
			{
				try
				{
					powValue = Integer.decode( pow.getText() );
				}
				catch( Exception e )
				{
					throw new ConstantIsTooLarge( powToken, pow.getText() );
				}
				
				if ( name1.size < 0 )
				{
					// do calculations
					name = new Constant( BitLen.bitpower( first, name1.resultVarName, powValue ).toString() );
					return name;
				}
				
				name = new ExpressionResult();
				name.resultVarName = varsc.newVar();				
				name.size = name1.size * powValue;
				name.expressionBody
				.append( name1.expressionBody )
				.append( "numbers:pow( " )
					.append( name.resultVarName ).append( ", _, " )
					.append( name1.resultVarName ).append( ", " )
					.append( name1.size ).append( ", " )
					.append( pow.getText() )
				.append( " )," )
				.append( eoln );
			}
		)
	;
	
bit_term returns [ExpressionResult name]
	: '(' name1 = expr ')' 	{ name = name1; }
	| INTEGER 				{ name = new Constant( $INTEGER.text ); }
	|  name1 = knownId		{ name = name1; }
	;

// returns a list constructed by parameters like:  
//  X, SizeOfX, Y, SizeOfY, ...., Z, SizeOfZ (without initial ',' !!!)
// NB: calling function converts parameters to signed-unsigned format 
parameters returns [StringBuffer pars]
	: { pars = new StringBuffer();}
	( name = knownId { pars.append( new StringBuffer( name.resultVarName ).append( ", " ).append( name.size ) ); }
	   ( ',' name1 = knownId { pars.append( ", " ).append( name1.resultVarName ).append( ", " ).append( name1.size ); } )*
	)?
	;
	
knownId returns [ExpressionResult name]
	: ID
		{
			name = new ExpressionResult();
			name.resultVarName = new StringBuffer( varsc.getCurrent( $ID, $ID.text ) );
			name.size = varsc.getVar( $ID, $ID.text ).size;
		}
	;
	
comparesign returns [String predicateName, BitLen.compare bitlenMethod]
	: '>-' { $predicateName = "numbers:greaterSigned"; $bitlenMethod = BitLen.compare.GR; }
	| '<-' { $predicateName = "numbers:lessSigned"; $bitlenMethod = BitLen.compare.LS; }
	| '>=-' { $predicateName = "numbers:greaterORequalSigned"; $bitlenMethod = BitLen.compare.GRE; }
	| '<=-' { $predicateName = "numbers:lessORequalSigned"; $bitlenMethod = BitLen.compare.LSE; }
	| '>+' { $predicateName = "numbers:greaterUnsigned"; $bitlenMethod = BitLen.compare.GR; }
	| '<+' { $predicateName = "numbers:lessUnsigned"; $bitlenMethod = BitLen.compare.LS; }
	| '>=+' { $predicateName = "numbers:greaterORequalUnsigned"; $bitlenMethod = BitLen.compare.GRE; }
	| '<=+' { $predicateName = "numbers:lessORequalUnsigned"; $bitlenMethod = BitLen.compare.LSE; }
	| '=' { $predicateName = "numbers:equal" ; $bitlenMethod = BitLen.compare.EQ; }
	| '#' { $predicateName = "numbers:notequal"; $bitlenMethod = BitLen.compare.NEQ; }
	;
	
addsign returns [String predicateName, BitLen.additive bitlenMethod]
	: '+' { $predicateName = "numbers:sum"; $bitlenMethod = BitLen.additive.SUM; }
	| '-' { $predicateName = "numbers:sub"; $bitlenMethod = BitLen.additive.SUB; }
	;
	
mulsign returns [String predicateName]
	: '><+' { return "numbers:mulUnsigned"; }
	| '><-' { return "numbers:mulSigned"; }
	;
	
ID	: ('a'..'z'| 'A'..'Z' ) ('a'..'z' | '_' | 'A'..'Z' | '0'..'9')*;

INTEGER :  ('0' .. '9' )+;

WS  :  (' '|'\r'|'\t'|'\u000C'|'\n') {$channel=HIDDEN;}
    ;

COMMENT
    :   '/*' .* '*/' {$channel=HIDDEN;}
    ;
LINE_COMMENT
    : '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    ;
