package ru.teslaprj.scheme.ts;

public abstract class CacheTestSituation extends ProcedureTestSituation
{
	int level;
	
	public CacheTestSituation( int level )
	{
		this.level = level;
	}
	
	/** ������� ����; 0 - ��� DATA-���� */
	public int getLevel()
	{
		return level;
	}
}
