package ru.kornevgen;

import java.math.BigInteger;

public class BitLen
{
	public static int bitlen( StringBuffer number )
	{
		int pow = 0;
		StringBuffer bitpower = new StringBuffer( "1" );
		do
		{
			pow++;
			bitpower = bitshift( bitpower );
			System.out.println( bitpower );
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
	
	static BigInteger toBigInteger( final StringBuffer s )
	{
		boolean isNegative = s.charAt( 0 ) == '-';
		
		if ( isNegative )
			s.deleteCharAt( 0 );
		
		BigInteger result = BigInteger.ZERO;
		int d = 19; // столько символов можно преобразовывать в число с помощью Long.parse
		
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
}
