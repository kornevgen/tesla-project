package ru.teslaprj.scheme.ts;

public interface CacheHit extends ProcedureTestSituation
{
	int getLevel();
	
	String getTagVar();
	
	String getSetVar();
}
