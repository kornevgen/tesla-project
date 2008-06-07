grammar TeSLa;

options
{
	k = 1;
	language=Java;
}

@header {
package ru.kornevgen;
import ru.kornevgen.BitLen;
}
@lexer::header {
package ru.kornevgen; 
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
	
	public class SemanticException extends RuntimeException
	{
		private static final long serialVersionUID = -4479856257644853038L;
	
		public SemanticException( Token token, String message )
		{
			super( "line " + token.getLine() + ": " + message );
		}
	}
	
	StringBuffer ecl;
	
	VarsController varsc;
	PredicatesController predsc;
}

program[String path] returns [java.util.List<LogicalVariable> parameters]
	: { 
		varsc = new VarsController();
		predsc = new PredicatesController();
		
		ecl = new StringBuffer();
		
		ecl.append(":- module( main )." ).append(eoln);
		ecl.append(":- lib( ic ).").append(eoln);
		ecl.append(":- use_module( numbers ).").append(eoln);
//		ecl.append(":- use_module( functions ).").append(eoln);
//		ecl.append(":- use_module( predicates ).").append(eoln);
		ecl.append(eoln);
	  }
		signature
			{
				ecl.append( varsc.createSignature() );
				ecl.append( varsc.types() );
				
				parameters = varsc.getVarsCopy();
			}
		(operator)*
		situationOperator '.'
			{
				ecl	.append( varsc.printMedians( parameters ) ).append( "true." + eoln)
					.append( predsc.printPredicates() );
				
				try
				{
					java.io.FileWriter writer = new java.io.FileWriter( new java.io.File( path ) );
					try
					{
						writer.write( ecl.toString() );
					}
					finally
					{
						writer.close();
					}
				}
				catch( java.io.IOException e )
				{
					System.out.println( e );
				}
			}
	;

signature
	: ( var_rule )*
	;

var_rule
	: 'VAR' ID ':' INTEGER 
		{ varsc.addVar( $ID.text, Integer.parseInt( $INTEGER.text ) ); } ';'
	;
	
operator
	: ( assertOperator
	| assignOperator
	) ';'
	;
	
situationOperator
	: // 'SITUATION' ID 'WHEN' predicate = boolexpr 
	'SITUATION' predicate = boolexpr 'IS' ID
		{ ecl.append( predicate .append( varsc.getAllVarsAsParameters() ).append(",").append( eoln ));}
	;
	
assertOperator
	: 'ASSERT' predicate = boolexpr 
		{ ecl.append( predicate .append( varsc.getAllVarsAsParameters() ).append(",").append( eoln ));}
	;

assignOperator
	: ID ':=' name = expr 
		{
			String id;
			// each variable can't change its size!!!
			if ( varsc.isKnown( $ID.text ) )
			{
				if ( name.size != varsc.getVar( $ID.text ).size )
				{
					throw new SemanticException( $ID, "Incompatible sizes: size of " + $ID.text + " is " +  varsc.getVar( $ID.text ).size + " bit(s) and size of expression is " + name.size + "; these sizes must be the same" );
				}
				
				id = varsc.nextVersion( $ID.text );
			}
			else
			{
				varsc.addVar( $ID.text, name.size );
				id = varsc.getCurrent( $ID.text );			
			}

			ecl.append( varsc.getSizePredicate( varsc.getVar( $ID.text ) ) )
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
						name1.append( varsc.getAllVarsAsParameters() )
						.append( "; " )
						.append( name2 )
						.append( varsc.getAllVarsAsParameters() ),
						varsc.getAllVarsAsParameters() 
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
						name1.append( varsc.getAllVarsAsParameters() )
						.append( ", " )
						.append( name2 )
						.append( varsc.getAllVarsAsParameters() ),
						varsc.getAllVarsAsParameters() 
					); 
		}
	)
	;
	
boolexpr_brackets returns [StringBuffer predicateName]
options { backtrack = true; }
@init{ Token first = input.LT(1); }
	: '(' be = boolexpr ')' { predicateName = be; }
	| name1 = expr pred = comparesign name2 = expr
	{
			if ( name1.size < 0 )
			{
				if ( name2.size > 0 )
				{
					if ( BitLen.bitlen( name1.resultVarName ) > name2.size )
						throw new SemanticException( first, "size of constant " + name1.resultVarName + " must be not greater than " + name2.size ); 
						
					name1.size = name2.size;
				}
				else //both arguments are constants
				{
					// do calculations and return
					return predsc.newPredicate( 
						pred.bitlenMethod.c( name1.resultVarName, name2.resultVarName ),
						varsc.getAllVarsAsParameters() );
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

		predicateName = predsc.newPredicate( body, varsc.getAllVarsAsParameters() );
	}
	| ID '(' pars = parameters ')'
		{
			predicateName = predsc.newPredicate( 
				new StringBuffer( "predicates:'" ).append( $ID.getText() )
				.append( "'( " ).append( pars ).append( " )" ),
				varsc.getAllVarsAsParameters()
			);
		}
	;

// return variable with evaluation result
expr returns [ExpressionResult name]
	: name1 = addexpr { name = name1; }
	;
	
addexpr returns [ExpressionResult name]
@init{ Token first = input.LT(1); }
	: name1 = mulexpr (
	 { name = name1; }
	| pred = addsign name2 = addexpr
		{
			if ( name1.size < 0 )
			{
				if ( name2.size > 0 )
				{
					if ( BitLen.bitlen( name1.resultVarName ) > name2.size )
						throw new SemanticException( first, "size of constant " + name1.resultVarName + " must be not greater than " + name2.size ); 
						
					name1.size = name2.size;
				}
				else //both arguments are constants
				{
					// do calculations and return
					name = new ExpressionResult();
					name.resultVarName = pred.bitlenMethod.c( name1.resultVarName, name2.resultVarName );
					name.size = -1;
					return name;
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
	: name1 = arithmterm (
	 { name = name1; }
	| pred = mulsign name2 = mulexpr
		{
			if ( name1.size < 0 )
			{
				if ( name2.size > 0 )
				{
					if ( BitLen.bitlen( name1.resultVarName ) > name2.size )
						throw new SemanticException( first, "size of constant " + name1.resultVarName + " must be not greater than " + name2.size ); 
						
					name1.size = name2.size;
				}
				else //both arguments are constants
				{
					// do calculations and return
					name = new ExpressionResult();
					name.resultVarName = BitLen.mul( name1.resultVarName, name2.resultVarName );
					name.size = -1;
					return name;
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
					.append( name1.resultVarName ).append( ", " )
					.append( name2.resultVarName ).append( ", " )
					.append( name1.size )
				.append( " )," )
				.append( eoln );
		} 
	)
	;

arithmterm returns [ExpressionResult name]
	: ( bit_term ( '[' | '^' | '.' ) )=> name1 = bit_expr { name = name1; } 
	// may be error: may be right version is bit_bound_term '.' | bit_term '[' | bit_term '^'
	| name1 = knownId { name = name1; }
	| INTEGER
		{
			name = new ExpressionResult();
			name.resultVarName = new StringBuffer( $INTEGER.text );
			name.size = -1;
		}
	| '(' name2 = expr ')' { name = name2; }
	;

bit_expr returns [ExpressionResult name]
	: name1 = bit_concat_term { name = name1; } ;
	
bit_concat_term returns [ExpressionResult name]
@init{ Token N1 = input.LT(1); Token N2 = null; }
	: name1 = bit_bound_term
	(   { name = name1; }
	| '.' { N2 = input.LT(1); } name2 = bit_concat_term
		{
			name = new ExpressionResult();
			name.resultVarName = varsc.newVar();
			name.size = name1.size + name2.size;
			
			name.expressionBody =
				name1.expressionBody
				.append( name2.expressionBody )
											
				.append( "numbers:concat( " )
					.append( name.resultVarName ).append( ", _, " )
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
@init{ Token first = input.LT(1); Token powToken = null; Token endToken = null; Token startToken = null; }
	: name1 = bit_term
		(   { name = name1; }
		| '[' { endToken = input.LT(1); } end = INTEGER 
			{
				try
				{
					Long.decode( end.getText() );
				}
				catch( Exception e )
				{
					throw new SemanticException( endToken, "constants is too large: " + end.getText() );
				}
			}
			
			( ']' // one bit
				{
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
			|  '..' { startToken = input.LT(1); } start = INTEGER ']' // bit range
				{
					name = new ExpressionResult();
					name.resultVarName = varsc.newVar();
					try
					{
						long endValue = Long.decode( end.getText() );
						long startValue = Long.decode( start.getText() );
						if ( endValue < startValue )
						{
							throw new SemanticException( endToken, "End index must be greater than the start one" );
						}
						name.size = (int)(endValue - startValue + 1);
					}
					catch( Exception e )
					{
						throw new SemanticException( startToken, "constants is too large: " + start.getText() );
					}
					
					name.expressionBody
					.append( name1.expressionBody )
					.append( "numbers:getbits( " )
						.append( name.resultVarName ).append( ", _," )
						.append( name1.resultVarName ).append( ", " )
						.append( end.getText() ).append( ", " )
						.append( start.getText() )
					.append( " )," )
					.append( eoln );
				}
	
		) | '^' { powToken = input.LT(1); } pow = INTEGER
			{
				name = new ExpressionResult();
				name.resultVarName = varsc.newVar();
				
				try
				{
					long powValue = Long.decode( pow.getText() );
					name.size = (int)(name1.size * powValue);
				}
				catch( Exception e )
				{
					throw new SemanticException( powToken, "constant " + pow.getText() + " is too large" );
				}
				name.expressionBody
				.append( name1.expressionBody )
				.append( "numbers:power( " )
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
	: ('(' INTEGER ')')=> '(' newsize = INTEGER ')' name1 = bit_term2
			{
				name = new ExpressionResult();
				name.resultVarName = varsc.newVar();
				name.size = Integer.parseInt( newsize.getText() );
				name.expressionBody
					.append( name1.expressionBody )
					.append( "functions:'sign_extend'( " )
						.append( name.resultVarName ).append( ", " )
						.append( name1.resultVarName ).append( ", " )
						.append( name1.size ).append( ", " )
						.append( newsize.getText() )
					.append( " )," ).append( eoln );
			}
	| name1 = bit_term2 { name = name1; }
	;

bit_term2 returns [ExpressionResult name]
	: name1 = knownId { name = name1; }
	| INTEGER { name = new ExpressionResult(); name.resultVarName = new StringBuffer( $INTEGER.text ); name.size = BitLen.bitlen( name.resultVarName ); }
    | '(' name1 = bit_expr ')' { name = name1; }
	;

// returns a list constructed by parameters like:  
//  X, SizeOfX, Y, SizeOfY, ...., Z, SizeOfZ (without initial ',' !!!)
// NB: calling function converts parameters to signed-unsigned format 
parameters returns [StringBuffer pars]
	: { pars = new StringBuffer();}
	|  name = knownId { pars = new StringBuffer( name.resultVarName ); }
	   ( 
		',' pars1 = parameters { pars.append( ", " ).append( pars1 ); }
 	   )?
	;
	
knownId returns [ExpressionResult name]
	: ID
		{
			if ( ! varsc.isKnown( $ID.text ) )
				throw new SemanticException( $ID, "unknown variable " + $ID.text );
				
			name = new ExpressionResult();
			name.resultVarName = new StringBuffer( varsc.getCurrent( $ID.text ) );
			name.size = varsc.getVar( $ID.text ).size;
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
