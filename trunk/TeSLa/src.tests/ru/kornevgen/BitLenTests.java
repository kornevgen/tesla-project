package ru.kornevgen;

import static org.junit.Assert.*;
import org.junit.Test;
import ru.kornevgen.BitLen;

public class BitLenTests
{
	@Test
	public final void b1()
	{
		assertEquals( 1, BitLen.bitlen( new StringBuffer( "1" ) ) );
	}
	
	@Test
	public final void b2()
	{
		assertEquals( 2, BitLen.bitlen( new StringBuffer( "2" ) ) );
	}
	
	@Test
	public final void b0()
	{
		assertEquals( 1, BitLen.bitlen( new StringBuffer( "0" ) ) );
	}
	
	@Test
	public final void b64()
	{
		assertEquals( 64, BitLen.bitlen( new StringBuffer( "12380724755245972077" ) ) );
	}
	
	@Test
	public final void b31()
	{
		assertEquals( 31, BitLen.bitlen( new StringBuffer( "1162968746" ) ) );
	}
	
	@Test
	public final void b46()
	{
		assertEquals( 46, BitLen.bitlen( new StringBuffer( "62584826190511" ) ) );
	}
}
