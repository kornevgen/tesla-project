package ru.teslaprj.syntax;

import java.math.BigInteger;
import java.util.List;

import org.antlr.runtime.Token;


public class BitLen
{
	///THIS CONSTANT MUST BE EQUAL TO THE SAME IN NUMBERS.ecl !!!!!!
	private final static int WORD_VALUE = 32;
	
	public static int bitlen( StringBuffer number )
	{
		int pow = 0;
		StringBuffer bitpower = new StringBuffer( "1" );
		do
		{
			pow++;
			bitpower = bitshift( bitpower );
//			System.out.println( bitpower );
		} while ( greaterorequal( number, bitpower ) );
		
		return pow;
	}
	
	public static boolean greaterorequal( final StringBuffer number, final StringBuffer bitpower )
	{
		if ( number.length() > bitpower.length() )
			return true;
		
		if ( number.length() < bitpower.length() )
			return false;
		
		if ( number.toString().equals( bitpower.toString() ) )
			return true;
		
		assert number.length() == bitpower.length();
		
		for( int i = 0; i < number.length(); i++ )
		{
			if ( number.charAt( i ) > bitpower.charAt( i ) )
				return true;
			
			if ( number.charAt( i ) < bitpower.charAt( i ) )
				return false;
			
			assert number.charAt( i ) == bitpower.charAt( i );
		}
		
		// unreachable statement
		return false;
	}

	public static StringBuffer bitshift( final StringBuffer bitpower )
	{
		return sum( bitpower, bitpower );
	}
	
	public static StringBuffer sum( final StringBuffer arg1, final StringBuffer arg2 )
	{
		assert arg1.length() == arg2.length();
		
		StringBuffer result = new StringBuffer();
		int curry = 0;
		
		for( int i = arg1.length() - 1; i >= 0; i-- )
		{
			int current1 = arg1.charAt( i ) - '0';
			int current2 = arg2.charAt( i ) - '0';
			result.insert( 0, ( current1 + current2 + curry ) % 10 );
			curry = ( current1 + current2 + curry ) / 10 ;
		}
		
		if ( curry != 0 )
			result.insert( 0, curry );
		
		return result;
	}
	
	public static BigInteger toBigInteger( final StringBuffer s )
	{
		boolean isNegative = s.charAt( 0 ) == '-';
		
		if ( isNegative )
			s.deleteCharAt( 0 );
		
		BigInteger result = BigInteger.ZERO;
		int d = 18; // столько символов можно преобразовывать в число с помощью Long.parse
		
		for( int i = 0; i < s.length(); i += d )
		{
			// result = result * 10^d + x
			String s1 = s.substring( i, Math.min( i + d, s.length() ) );
			BigInteger x = BigInteger.valueOf( Long.parseLong( s1 )	);
			BigInteger basepower = BigInteger.valueOf( 10 ).pow( s1.length() );
			result = result.multiply( basepower ).add( x );
		}
		
		if ( isNegative )
			result = BigInteger.ZERO.subtract( result );
		
		return result;
	}
	
	public static StringBuffer mul( final StringBuffer arg1, final StringBuffer arg2 )
	{
		return new StringBuffer(
				toBigInteger( arg1 ).multiply( toBigInteger( arg2 ) ).toString()
			);
	}
	
	public static StringBuffer abit( final Token operation, final StringBuffer number, final int index )
	{
		BigInteger n = toBigInteger( number );
		if ( n.signum() < 0 )
			throw new SemanticException( operation, "negative value (" + number + ") is prohibited for calculating a bit value" );
		
		;
		
		return new StringBuffer( 
				n.shiftRight( index )
				.divideAndRemainder(BigInteger.valueOf(2))[1].toString()
		);
	}
	
	public static StringBuffer bitrange( final Token operation, final StringBuffer number, final int endIndex, final int startIndex )
	{
		BigInteger n = toBigInteger( number );
		if ( n.signum() < 0 )
			throw new SemanticException( operation, "negative value (" + number + ") is prohibited for calculating a bit value" );
		
		;
		
		return new StringBuffer( 
				n.shiftRight( startIndex )
				.divideAndRemainder(BigInteger.valueOf(2).pow(endIndex - startIndex + 1))[1].toString()
		);
	}
	
	public static StringBuffer bitpower( final Token operation, final StringBuffer number, final int pow )
	{
		BigInteger n = toBigInteger( number );
		if ( n.signum() < 0 )
			throw new SemanticException( operation, "negative value (" + number + ") is prohibited here");
		
		return new StringBuffer(
				n.pow( pow ).toString()
			);
	}
	
	public static StringBuffer bitconcat( final Token first, final Token second, final StringBuffer firstNumber, final StringBuffer secondNumber )
	{
		BigInteger f = toBigInteger( firstNumber );
		if ( f.signum() < 0 )
			throw new SemanticException( first, "negative value (" + firstNumber + ") is prohibited here");
		
		BigInteger s = toBigInteger( secondNumber );
		if ( s.signum() < 0 )
			throw new SemanticException( second, "negative value (" + secondNumber + ") is prohibited here");
		
		return new StringBuffer(
				f.shiftLeft( s.bitLength() ).add( s ).toString()
			);
	}
	
	public static StringBuffer sign_extend( final Token num, final StringBuffer number, final int new_size )
	{
		BigInteger n = toBigInteger( number );
		if ( n.signum() < 0 )
			throw new SemanticException( num, "negative value (" + number + ") is prohibited here");

		int nlen = bitlen( number );
		if ( new_size < nlen )
		{
			return new StringBuffer(
					n.shiftRight( nlen - new_size ).toString()
				);
		}
		else
		{
			return number;
		}
	}
	
	public static StringBuffer number2nlist( StringBuffer number, int bitlen )
	{
		BigInteger n = toBigInteger( number );
		StringBuffer result = new StringBuffer( " ]" );
		BigInteger pow2 = BigInteger.valueOf( 2 ).pow( WORD_VALUE );
		
		int chunks = 0;
		while ( n.compareTo( pow2 ) >= 0 )
		{
			BigInteger[] m = n.divideAndRemainder( pow2 );
			result.insert( 0, ", " + m[1].toString() );
			n = m[0];
			chunks ++;
		}
		
		result.insert(0, n.toString() );
		chunks ++;
		
		//сколько еще нужно chunk'ов?
		int expected_chunks = (int)Math.round( Math.ceil( (float)bitlen / WORD_VALUE ) );
		if ( expected_chunks > chunks )
		{
			for( int i = chunks; i < expected_chunks; i++ )
			{
				result.insert(0, "0, " );
			}
		}
		
		return result.insert(0, "[ " );
	}
	
	public static final StringBuffer TRUE = new StringBuffer( "true" );	
	public static final StringBuffer FAIL = new StringBuffer( "fail" );
	
	public enum compare
	{
		GR
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				if ( toBigInteger( arg1 ).compareTo( toBigInteger( arg2 ) ) > 0 )
					return TRUE;
				else
					return FAIL;
			}
		}
		,
		LS
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				if ( toBigInteger( arg1 ).compareTo( toBigInteger( arg2 ) ) < 0 )
					return TRUE;
				else
					return FAIL;
			}
		}
		,
		GRE
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				if ( toBigInteger( arg1 ).compareTo( toBigInteger( arg2 ) ) >= 0 )
					return TRUE;
				else
					return FAIL;
			}
		}
		,
		LSE
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				if ( toBigInteger( arg1 ).compareTo( toBigInteger( arg2 ) ) <= 0 )
					return TRUE;
				else
					return FAIL;
			}
		}
		,
		EQ
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				if ( arg1.toString().equals( arg2.toString() ) )
					return TRUE;
				else
					return FAIL;
			}
		}
		,
		NEQ
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				if ( ! arg1.toString().equals( arg2.toString() ) )
					return TRUE;
				else
					return FAIL;
			}
		}
		;
		public abstract StringBuffer c( StringBuffer arg1, StringBuffer arg2 );
	}
	
	public enum additive
	{
		SUM
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				return new StringBuffer(
						toBigInteger( arg1 ).add( toBigInteger( arg2 ) ).toString()
					);
			}
		}
		,
		SUB
		{
			public StringBuffer c( StringBuffer arg1, StringBuffer arg2 )
			{
				return new StringBuffer(
						toBigInteger( arg1 ).subtract( toBigInteger( arg2 ) ).toString()
					);
			}
		}
		;
		public abstract StringBuffer c( StringBuffer arg1, StringBuffer arg2 );
	}
	
    
    public static BigInteger string2bigint( String s )
    {
    	int ChunkLength = (int)Math.round( Math.floor( Math.log( Long.MAX_VALUE ) / Math.log(10) ) );
    	
    	BigInteger result = BigInteger.ZERO;
    	BigInteger doldlen = BigInteger.ONE;
    	BigInteger dstep = BigInteger.valueOf(10).pow( ChunkLength );
    	while( s.length() >= ChunkLength )
    	{
    		String chunk = s.substring( s.length() - ChunkLength );
    		//result = result + chunk * 10^oldlen
    		BigInteger value = BigInteger.valueOf(Long.parseLong(chunk)).multiply( doldlen );
    		result = result.add( value );

    		s = s.substring(0, s.length() - ChunkLength );
    		doldlen = doldlen.multiply( dstep );
    	}

		//result = result + chunk * 10^oldlen
    	if ( s.isEmpty() )
    		return result;
    	else
    		return result.add( BigInteger.valueOf(Long.parseLong(s)).multiply( doldlen ) );
    }
	

    public static BigInteger intlist2bigint( List<?> l )
    {
    	BigInteger result = BigInteger.ZERO;
    	BigInteger doldlen = BigInteger.ONE;
    	BigInteger dstep = BigInteger.valueOf(2).pow( WORD_VALUE );
    	for( int i = l.size() - 1; i >= 0; i-- )
    	{
    		//result = result + l[i] * 2^(i*C)
    		if ( l.get(i) instanceof Integer )
    			result = result.add( BigInteger.valueOf((Integer)l.get(i)).multiply( doldlen ) );
    		else if ( l.get(i) instanceof Long )
    			result = result.add( BigInteger.valueOf((Long)l.get(i)).multiply( doldlen ) );
    		else
    			throw new Error("unknown type of " + l.get(i) );

    		doldlen = doldlen.multiply( dstep );
    	}
    	
    	return result;

//    	BigInteger a = BigInteger.valueOf( arg.get(0) ); 
//    	BigInteger pow = BigInteger.valueOf( 2 ).pow( WORD_VALUE );
////    	System.out.print( "( " + arg.get(0) );
//    	if ( arg.size() > 1 )
//    		for( Object argValue : arg.subList(1, arg.size()) )
//    		{
//    			// a := a * 2^C + argValue
//    			a = a.multiply( pow ).add( BigInteger.valueOf( Long.parseLong((String)argValue) ));
////    			System.out.print( ", " + argValue );
//    		}
////    	System.out.println(" )" );
//    	values.get(i).setValue( a );
    }

}
