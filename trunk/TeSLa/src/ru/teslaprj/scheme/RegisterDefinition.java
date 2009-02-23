package ru.teslaprj.scheme;

public class RegisterDefinition extends NameBitlenDefinition
{
	public RegisterDefinition(String name, int bitlen) {
		super(name, bitlen);
	}
	
	@Override
	public String toString()
	{
		return "REGISTER " + name + " : " + bitlen + ";";
	}
}
