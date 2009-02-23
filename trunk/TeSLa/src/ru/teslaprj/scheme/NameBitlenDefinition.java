package ru.teslaprj.scheme;

public abstract class NameBitlenDefinition implements Definition
{
	protected String name;
	protected int bitlen;

	public NameBitlenDefinition( String name, int bitlen )
	{
		this.name = name;
		this.bitlen = bitlen;
	}
	
	public int getBitlen() {
		return bitlen;
	}

	public String getName() {
		return name;
	}
	
	@Override
	public String toString()
	{
		return name;
	}
}
