package ru.kornevgen;

import static org.junit.Assert.assertEquals;

import org.antlr.runtime.Token;
import org.junit.Test;

import ru.teslaprj.arithm.BitLen;

public class BitConcatTests
{
	@Test
	public final void bitpower1()
	{
		assertEquals(BitLen.bitconcat( Token.SKIP_TOKEN, Token.SKIP_TOKEN, new StringBuffer("0"), new StringBuffer("0")).toString(), "0" );
	}

	@Test
	public final void bitpower2()
	{
		assertEquals(BitLen.bitconcat( Token.SKIP_TOKEN, Token.SKIP_TOKEN, new StringBuffer("0"), new StringBuffer("1110")).toString(), "1110" );
	}

	@Test
	public final void bitpower3()
	{
		assertEquals(BitLen.bitconcat( Token.SKIP_TOKEN, Token.SKIP_TOKEN, new StringBuffer("26776"), new StringBuffer("0")).toString(), "26776" );
	}

	@Test
	public final void bitpower4()
	{
		assertEquals(BitLen.bitconcat( Token.SKIP_TOKEN, Token.SKIP_TOKEN, new StringBuffer("66"), new StringBuffer("2")).toString(), "266" );
	}
}
