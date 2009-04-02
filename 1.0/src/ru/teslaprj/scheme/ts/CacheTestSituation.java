package ru.teslaprj.scheme.ts;

public interface CacheTestSituation extends ProcedureTestSituation
{
	/** уровень тега; 0 - для DATA-кэша */
	int getLevel();
	
	/** имя переменной - тега;
	 * null - тег может быть любой переменной */
	String getTagVar();
	
	/** имя переменной - сет;
	 * null - сет может быть любой переменной */
	String getSetVar();
}
