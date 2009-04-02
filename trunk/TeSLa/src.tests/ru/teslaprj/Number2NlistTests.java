package ru.teslaprj;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import ru.teslaprj.syntax.BitLen;

public class Number2NlistTests
{
	@Test
	public final void b1()
	{
		assertEquals( "[ 1 ]", BitLen.number2nlist( new StringBuffer( "1" ), 0 ).toString() );
	}
	
	@Test
	public final void b2()
	{
		assertEquals( "[ 2 ]", BitLen.number2nlist( new StringBuffer( "2" ), 0 ).toString() );
	}
	
	@Test
	public final void b0()
	{
		assertEquals( "[ 0 ]", BitLen.number2nlist( new StringBuffer( "0" ), 0 ).toString() );
	}
	
	@Test
	public final void b01()
	{
		assertEquals( "[ 0, 0 ]", BitLen.number2nlist( new StringBuffer( "0" ), 64 ).toString() );
	}
	
	@Test
	public final void b64()
	{
		assertEquals( "[ 5498, 329379604478573 ]", BitLen.number2nlist( new StringBuffer( "12380724755245972077" ), 0 ).toString() );
	}
	
	@Test
	public final void b31()
	{
		assertEquals( "[ 1162968746 ]", BitLen.number2nlist( new StringBuffer( "1162968746" ), 0 ).toString() );
	}

	@Test
	public final void b5()
	{
		assertEquals( "[ 0, 0, 0, 0, 1 ]", BitLen.number2nlist( new StringBuffer( "1" ), 213 ).toString() );
	}	
}
