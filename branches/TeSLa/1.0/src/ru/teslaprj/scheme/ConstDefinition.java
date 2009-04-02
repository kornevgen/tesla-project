package ru.teslaprj.scheme;

public class ConstDefinition extends NameBitlenDefinition
{
	public ConstDefinition(String name, int bitlen) {
		super(name, bitlen);
	}
	
	@Override
	public String toString()
	{
		return "CONST " + name + " : " + bitlen + ";";
	}
}
