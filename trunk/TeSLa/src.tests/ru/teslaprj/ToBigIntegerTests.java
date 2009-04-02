package ru.teslaprj;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.text.DecimalFormat;
import java.text.NumberFormat;

import org.junit.Test;

import ru.teslaprj.syntax.BitLen;

public class ToBigIntegerTests {
	
	@Test
	public final void trans1()
	{
		for( long i = 0; i < 100; i++ )
			assertEquals( 
					BigInteger.valueOf( i ),
					BitLen.toBigInteger( new StringBuffer( new Long(i).toString() )  )
				);
	}
	
	@Test
	public final void trans2()
	{
		NumberFormat nf = new DecimalFormat();
		nf.setGroupingUsed( false );
		nf.setMinimumIntegerDigits( 5 );
		
		BigInteger A = BigInteger.valueOf( Long.parseLong( "8446744073709551616" ))
							.multiply( BigInteger.valueOf( 100000 ) );
		for( long i = 0; i < 100; i++ )
		{
			StringBuffer S = new StringBuffer( "8446744073709551616" ).append( nf.format(i)  );
			assertEquals( 
					BigInteger.valueOf( i ).add( A ),
					BitLen.toBigInteger( S )
				);
		}
	}
	
	@Test
	public final void trans4()
	{
		long i = 57894857L;

		assertEquals( 
				BigInteger.valueOf( i ),
				BitLen.toBigInteger( new StringBuffer( new Long(i).toString() )  )
			);
	}
	
	@Test
	public final void trans5()
	{
		assertEquals( 
				BigInteger.valueOf( -10 ),
				BitLen.toBigInteger( new StringBuffer( "-10" )  )
			);
	}
	
//	@Test
//	public final void trans3()
//	{
//		for( long i = 57894857; i < 67994857; i++ )
//			assertEquals( 
//					BigInteger.valueOf( i ),
//					BitLen.toBigInteger( new StringBuffer( new Long(i).toString() )  )
//				);
//	}	

}
