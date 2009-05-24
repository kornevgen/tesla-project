package ru.teslaprj.scheme.ts;

public abstract class CacheTestSituation extends ProcedureTestSituation
{
	int level;
	
	public CacheTestSituation( int level )
	{
		this.level = level;
	}
	
	/** уровень тега; 0 - для DATA-кэша */
	public int getLevel()
	{
		return level;
	}
}
