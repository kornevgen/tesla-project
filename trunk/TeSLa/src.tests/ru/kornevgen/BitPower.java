package ru.kornevgen;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.Token;
import org.junit.Test;

import ru.teslaprj.syntax.BitLen;

public class BitPower
{
	@Test
	public final void bitpower1()
	{
		assertEquals(BitLen.bitpower(Token.SKIP_TOKEN, new StringBuffer("1"), 0).toString(), "1" );
	}

	@Test
	public final void bitpower2()
	{
		assertEquals(BitLen.bitpower(Token.SKIP_TOKEN, new StringBuffer("1"), 100000).toString(), "1" );
	}

	@Test
	public final void bitpower3()
	{
		assertEquals(BitLen.bitpower(Token.SKIP_TOKEN, new StringBuffer("2"), 0).toString(), "1" );
	}

	@Test
	public final void bitpower4()
	{
		assertEquals(BitLen.bitpower(Token.SKIP_TOKEN, new StringBuffer("3"), 2).toString(), "9" );
	}
}
