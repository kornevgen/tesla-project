package ru.kornevgen;

import java.math.BigInteger;

public class LogicalVariable
{
	private String baseName;
	private String currentName;
	private int currentVersion = 0;
	public int size;
	private BigInteger value = null; 
	
	public LogicalVariable( final String name, final int size )
	{
		assert name != null;
		assert ! name.equals("");
		baseName = name;
		this.size = size;
		nextVersion();
	}
	
	void nextVersion()
	{
		currentVersion++;
		currentName = "_" + currentVersion + baseName;
	}
	
	String current()
	{
		return currentName;
	}
	
	String next()
	{
		return "_" + (currentVersion+1) + baseName;
	}
	
	/** _1 + base name */
	String getBaseName()
	{
		return "_1" + baseName;
	}
	
	/** имя, как оно написано пользователем */
	public String getCanonicalName()
	{
		return baseName;
	}
	
	public BigInteger getValue() 
	{
		return value;
	}

	public void setValue( final BigInteger value) 
	{
		this.value = value;
	}
	
}