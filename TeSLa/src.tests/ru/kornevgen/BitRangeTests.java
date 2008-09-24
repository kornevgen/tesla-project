package ru.kornevgen;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.antlr.runtime.Token;
import org.junit.Test;

import ru.teslaprj.arithm.BitLen;
import ru.teslaprj.arithm.SemanticException;

public class BitRangeTests {
	
	@Test
	public final void bitrange1()
	{
		assertEquals(BitLen.bitrange(Token.SKIP_TOKEN, new StringBuffer("1"), 0, 0).toString(), "1" );
	}
	
	@Test
	public final void bitrange2()
	{
		assertEquals(BitLen.bitrange(Token.SKIP_TOKEN, new StringBuffer("2"), 0, 0).toString(), "0" );
	}
	
	@Test
	public final void bitrange3()
	{
		assertEquals(BitLen.bitrange(Token.SKIP_TOKEN, new StringBuffer("2"), 1, 0).toString(), "2" );
	}
	
	@Test
	public final void bitrange4()
	{
		assertEquals(BitLen.bitrange(Token.SKIP_TOKEN, new StringBuffer("2"), 17777, 0).toString(), "2" );
	}
	
	
	@Test
	public final void bitrange5()
	{
		assertEquals(BitLen.bitrange(Token.SKIP_TOKEN, new StringBuffer("2"), 17777, 677).toString(), "0" );
	}
	
	@Test
	public final void bitrange6()
	{
		try
		{
			BitLen.bitrange(Token.SKIP_TOKEN, new StringBuffer("-2"), 17777, 6787 );
		}
		catch( SemanticException e )
		{
			assertTrue( true );
		}
		catch( Exception e )
		{
			assertFalse( true );
		}
	}

}
