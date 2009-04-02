package ru.teslaprj;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.antlr.runtime.Token;
import org.junit.Test;

import ru.teslaprj.syntax.BitLen;
import ru.teslaprj.syntax.SemanticException;

public class ABitTests
{
	@Test
	public final void abit1()
	{
		assertEquals(BitLen.abit(Token.SKIP_TOKEN, new StringBuffer("1"), 0).toString(), "1" );
	}
	
	@Test
	public final void abit2()
	{
		assertEquals(BitLen.abit(Token.SKIP_TOKEN, new StringBuffer("2"), 0).toString(), "0" );
	}
	
	@Test
	public final void abit3()
	{
		assertEquals(BitLen.abit(Token.SKIP_TOKEN, new StringBuffer("2"), 1).toString(), "1" );
	}
	
	@Test
	public final void abit4()
	{
		assertEquals(BitLen.abit(Token.SKIP_TOKEN, new StringBuffer("2"), 17777).toString(), "0" );
	}
	
	@Test
	public final void abit5()
	{
		try
		{
			BitLen.abit(Token.SKIP_TOKEN, new StringBuffer("-2"), 17777 );
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
