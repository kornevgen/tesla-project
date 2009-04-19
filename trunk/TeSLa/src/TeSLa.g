grammar TeSLa;

options
{
	language=Java;
}

@header {
package ru.teslaprj.syntax;
import ru.teslaprj.Cache;
import ru.teslaprj.TLB;
import ru.teslaprj.scheme.ts.TLBSituation;
import ru.teslaprj.scheme.ts.TLBRefill;
import ru.teslaprj.scheme.ts.TLBExists;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ConstDefinition;
import java.util.Set;
import java.util.HashSet;
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
	
	boolean stopTranslation = false;
	
	StringBuffer ecl;
	
	VarsController varsc;
	PredicatesController predsc;
	
	Set<LogicalVariable> readlist;
	Set<LogicalVariable> writelist;
	Set<LogicalVariable> loadlogicallist;
	boolean loadReflect;
	
	String commandPrefix;
	
	List<Cache> cacheLevels;
	
	TLB tlb;
	TLBSituation tlbSituation;
	int physAddressBitLen;
	
	String virtualAddress = null;
	String physicalAddressAfterTranslation = null;
	String physicalAddressForMemOperation = null;
	String memValue = null;
	MemoryCommand memOperation;
	int memValueSize = 0;
	boolean isDataTLBused;
	boolean isDataCacheused;
	
	boolean refill;
	boolean store;
	
	@Override
	public void reportError( RecognitionException e )
	{
		throw new ParserException( e.getMessage() );
	}
	
	public class ParserException extends RuntimeException
	{
		private static final long serialVersionUID = 7672627512415645603L;

		public ParserException( String s )
		{
			super( s );
		}
	}
	
	public enum MemoryCommand
	{
		LOAD, STORE
	}
}

program[
      List<String> args
	, Scheme scheme
	, Command command
	, String prefix
	, List<Cache> cacheLevels
	, TLB tlb
	, TLBSituation tlbSituation
	, int physAddressBitLen
] returns
	 [ StringBuffer eclipseProgram
	 , List<LogicalVariable> signature
	 , MemoryCommand memoryOperation
	 , boolean hasAddressTranslation
	 , boolean isStoreAddressTranslation
	 , boolean isDataTLBused
	 , boolean isDataCacheused
	 , Set<String> readlist
	 , Set<String> loadlist
	 , int memValueSize
	 ]
@init {
List<LogicalVariable> parameters = new ArrayList<LogicalVariable>();
StringBuffer preArgs;
ecl = new StringBuffer();
varsc = new VarsController();
predsc = new PredicatesController();
preArgs = null;
memOperation = null;
this.cacheLevels = cacheLevels;
this.tlb = tlb;
this.tlbSituation = tlbSituation;
this.refill = (tlbSituation instanceof TLBRefill);
this.physAddressBitLen = physAddressBitLen;
this.readlist = new HashSet<LogicalVariable>();
this.writelist = new HashSet<LogicalVariable>();
this.loadlogicallist = new HashSet<LogicalVariable>();
}
	: signature
			{
				parameters = varsc.getVarsCopy();
				preArgs = 
					varsc.getAllVarsAsParametersWithStatus( 
						LogicalVariable.Status.SIGNATURE_RESULT, 
						LogicalVariable.Status.SIGNATURE_READONLY );

				if ( args.size() != parameters.size() )
				{
					throw new SemanticException( 
						null, 
						"command #" + scheme.getCommands().indexOf(command) +
						": count of arguments (" + args.size() + ") must be equal with " + 
						" count of test sutiation parameters (" + parameters.size() + ")");
				}

				// check args bitlens
				for( int i = 0; i < args.size(); i++ )
				{
					int argLen = scheme.getBitlen( args.get(i) );
					int varLen = parameters.get(i).getBitlen();
					if ( argLen != varLen )
					{
						throw new SemanticException( 
							null, 
							"command #" + scheme.getCommands().indexOf(command) +
							": bitlens of variable and argument are not equal: " + argLen + " and " + varLen );
					}
				}
				// check actual names of results var (must different)
				for( int i = 0; i < args.size(); i++ )
				{
					if ( parameters.get(i).getStatus() != LogicalVariable.Status.SIGNATURE_RESULT )
						continue;
						
					for( int j = i+1; j < args.size(); j++ )
					{
						if ( parameters.get(j).getStatus() != LogicalVariable.Status.SIGNATURE_RESULT )
							continue;
						
						if ( args.get(i).equals( args.get(j) ) )
						{
							throw new SemanticException( 
								null, 
								"command #" + scheme.getCommands().indexOf(command) +
								": RESULT parameters must correspond to the different args"
							 );
						}
					}
				}
			
				// if var is constant in scheme but not readonly then error
				for( int i = 0; i < args.size(); i++ )
				{
					if ( parameters.get(i).getStatus() != LogicalVariable.Status.SIGNATURE_READONLY
						&&
						scheme.getNameDefinition( args.get(i) ) instanceof ConstDefinition )
					{
						throw new SemanticException( 
							null, 
							"command #" + scheme.getCommands().indexOf(command) +
							": Parameter corresponded to constant '" + 
							parameters.get(i).getCanonicalName() + "' must be READONLY" );
					}
				}
				
				commandPrefix = prefix;
				
				stopTranslation = false;
			}
		(operator)*
			{
				StringBuffer postArgs = varsc.getAllVarsAsParametersWithStatus(
						LogicalVariable.Status.SIGNATURE_RESULT,
						LogicalVariable.Status.SIGNATURE_READONLY );
				StringBuffer optional = varsc.getAllVarsAsParametersWithStatus( 
						LogicalVariable.Status.OPTIONAL );
				if ( optional.length() > 0 )
					postArgs.append( ", " );
					
				retval.eclipseProgram = new StringBuffer(
					"'" + prefix + "::main'( _, " )
					.append( preArgs ).append(", ")
					.append( postArgs ).append( optional );
									
				if ( virtualAddress != null )
				{
					retval.eclipseProgram
						.append( ", " ).append( virtualAddress )
						.append( ", " ).append( physicalAddressAfterTranslation );
				}
				
				if ( memOperation != null )
				{
					retval.eclipseProgram
						.append( ", " ).append( physicalAddressForMemOperation )
						.append( ", " ).append( memValue );
				}
				
				// generate error if variable is RESULT but its postversion == its preversion
				if ( ! stopTranslation )
				{
					for( int i = 0; i < args.size(); i++ )
					{
						if ( parameters.get(i).getStatus() != LogicalVariable.Status.SIGNATURE_RESULT )
							continue;
							
						// if preversion == postversion /\ !stopTranslation than error!
              			if ( parameters.get(i).current().startsWith("_1") )
              			{
              				throw new SemanticException( 
              					null,
              					"command #" + scheme.getCommands().indexOf(command) + 
              					": RESULT parameter " + 
              					parameters.get(i).getCanonicalName() + 
              					" isn't changed" );
              			}
					}
				}
									
				retval.eclipseProgram.append( ") :- ").append( eoln )
					.append( ecl )
					.append( "true." ).append( eoln )
					.append( predsc.printPredicates() ).append(eoln);
				retval.signature = varsc.getSignature();
				retval.memoryOperation = memOperation;
				retval.hasAddressTranslation = (virtualAddress != null);
				retval.isStoreAddressTranslation = store;
				retval.readlist = new HashSet<String>();
				for( LogicalVariable v : readlist )
				{
					if ( parameters.contains( v ) )
						retval.readlist.add( args.get( parameters.indexOf( v ) ) );
				}
				retval.loadlist = new HashSet<String>();
				for( LogicalVariable v : loadlogicallist )
				{
					if ( parameters.contains( v ) )
						retval.loadlist.add( args.get( parameters.indexOf( v ) ) );
				}
				retval.memValueSize = this.memValueSize;
				retval.isDataTLBused = this.isDataTLBused;
				retval.isDataCacheused = this.isDataCacheused;
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
		| 'OPTIONAL' { status = LogicalVariable.Status.OPTIONAL; }	 )
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
	| procedure
	) ';'
	;
	
assertOperator
	: 'ASSERT' predicate = boolexpr 
		{
			if ( ! stopTranslation )
			{ 
				ecl.append( 
					predicate .append( "(" ).append( varsc.getAllVarsAsParameters() ).append("),")
					.append( eoln )
				);
			}
		}
	;

assignOperator
	: { loadReflect = false; }
	  ID '<-' name = expr 
		{
			if ( ! stopTranslation )
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
				
				writelist.add( varsc.getVar( $ID, $ID.text ) );
				
				if ( loadReflect )
				{
					loadlogicallist.add( varsc.getVar( $ID, $ID.text ) );
				}
	
				ecl.append( varsc.getSizePredicate( varsc.getVar( $ID, $ID.text ) ) )
				.append( name.expressionBody )
				.append( id ).append( " = " ).append( name.resultVarName ).append( "," ).append( eoln);
			}
		}
	;

procedure
@init{store = false; boolean mem = (memOperation != null); int size = -1; }
	: t = ( 'LoadMemory' { memOperation = MemoryCommand.LOAD; }
	      | 'StoreMemory' { memOperation = MemoryCommand.STORE; } ) 
			'(' 
				    value = ID
				',' ('DOUBLEWORD' {size=3;} | 'WORD' {size=2;} | 'HALFWORD' {size=1;} | 'BYTE' {size=0;} ) 
				',' addr  = ID
				',' vAddr = ID
				',' ( 'DATA'{isDataCacheused=true;} ) // | 'INSTRUCTION'{isDataCacheused=false;} )
			')'
		{
			if ( stopTranslation )
			{
				physicalAddressForMemOperation = "_";
				memValue = "_";
			}
			else
			{
				if ( mem )
				{
					throw new SemanticException( t, "memory operation is already used" );
				}				 
				
				physicalAddressForMemOperation = varsc.getCurrent( addr, addr.getText() );
				
				// check physVar size
				if ( varsc.getVar( addr, addr.getText() ).size != physAddressBitLen )
				{
					throw new SemanticException( addr, "Size of physical address must be " + physAddressBitLen );
				}
				
				int valueBitLen = (int)Math.pow( 2, size + 3 );
				
				// add new var for value and add to the signature
				if ( ! varsc.isKnown( value.getText() ) )
				{
					varsc.addVar( 
						  value.getText()
						, valueBitLen
						, LogicalVariable.Status.LOCAL
						, false );
				}
				else
				{
					 // check: sizeof( value ) == valueBitLen
					 if ( varsc.getVar( value, value.getText() ).size != valueBitLen )
					 {
					 	throw new SemanticException( value, "'" + value.getText() + "' must be of " + valueBitLen + " bits" );
					 }
				}
				memValue = varsc.getCurrent( value, value.getText() );
				
				if ( memOperation == MemoryCommand.LOAD )
				{
					writelist.add( varsc.getVar( value, value.getText() ) );
					loadlogicallist.add( varsc.getVar( value, value.getText() ) );
				}
				else
				{
					if ( ! writelist.add( varsc.getVar( value, value.getText() ) ) )
						readlist.add( varsc.getVar( value, value.getText() ) );
				}
				
				// if vAddr[size-1 .. 0] != 0 then throw new AddressException();
				
				this.memValueSize = size;				
			}			
		}
	| t1 = 'AddressTranslation'
		'('
			    phys = ID 
			',' virtual = ID
			',' ('DATA'{isDataTLBused=true;}|'INSTRUCTION'{isDataTLBused=false;})
			',' ('LOAD'|'STORE'{store=true;})
		')'
		{
			if ( virtualAddress != null )
			{
				throw new SemanticException( t1, "address translation is already used" );
			}
			
			virtualAddress = varsc.getCurrent( virtual, virtual.getText() );
			
			if ( varsc.getVar( virtual, virtual.getText() ).size != tlb.getSEGBITS() )
			{
				throw new SemanticException(
					virtual,
					"Size of virtual address variable '" +
					varsc.getVar( virtual, virtual.getText() ).getCanonicalName() +
					"' must be equal to " + tlb.getSEGBITS() );
			}
			
			// add new variable if needed
			if ( ! varsc.isKnown( phys.getText() ) )
			{
				varsc.addVar( 
					  phys.getText()
					, tlb.getPABITS()
					, LogicalVariable.Status.LOCAL
					, false );
			}
			else
			{
				if ( varsc.getVar( phys, phys.getText() ).size != tlb.getPABITS() )
				{
					throw new SemanticException( 
						phys, 
						"Size of physical address variable '" + 
						varsc.getVar( phys, phys.getText() ).getCanonicalName() +
						"' must be equal to " +  tlb.getPABITS() );
				}
			}
			
			if ( refill 
				|| ((TLBExists)tlbSituation).getValid() == 0 
				|| ( store && ((TLBExists)tlbSituation).getmoDify() == 0 ) )
			{
				stopTranslation = true;
				physicalAddressAfterTranslation = "_";
			}
			else
			{
				physicalAddressAfterTranslation = varsc.nextVersion( phys, phys.getText() );
				
				// pAddr[first] = vAddr[first]
				StringBuffer tmp = varsc.newVar(); 
				ecl.append( "numbers:getbits( " )
					.append( tmp ).append( ", " )
					.append( virtualAddress ).append( ", " )
					.append( tlb.getSEGBITS() ).append( ", " )
					.append( tlb.getPABITS() - tlb.getPFNBitLen() - 1 ).append( ", " )
					.append( "0 )," ).append( eoln );
					
				StringBuffer tmp2 = varsc.newVar(); 
				ecl.append( "numbers:getbits( " )
					.append( tmp2 ).append( ", " )
					.append( physicalAddressAfterTranslation ).append( ", " )
					.append( tlb.getPABITS() ).append( ", " )
					.append( tlb.getPABITS() - tlb.getPFNBitLen() - 1 ).append( ", " )
					.append( "0 )," ).append( eoln );
				
				ecl.append( "numbers:equal( " )
					.append( tmp ).append( ", " )
					.append( tmp2 ).append( ", " )
					.append( tlb.getPABITS() - tlb.getPFNBitLen() - 1 )
				.append( ")," ).append( eoln );	
				
				
				if ( ! writelist.contains( varsc.getVar( phys, phys.getText() ) ) )
					readlist.add( varsc.getVar( phys, phys.getText() ) );
				writelist.add( varsc.getVar( virtual, virtual.getText() ) );
			}
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
			if ( ! stopTranslation )
			{
				LogicalVariable var = varsc.getVar( $ID, $ID.text );
				if ( ! writelist.contains( var ) )
					readlist.add( var );
				if ( loadlogicallist.contains( var ) )
				  loadReflect = true;
			}
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
