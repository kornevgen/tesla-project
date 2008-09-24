package ru.teslaprj.scheme;

public abstract class NameBitlenDefinition implements Definition
{
	private String name;
	private int bitlen;

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
}
