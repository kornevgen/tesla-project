package ru.teslaprj.scheme.ts;

public interface CacheMiss extends ProcedureTestSituation
{
	/** уровень тега, в котором происходит промах */
	int getLevel();
	
	/** имя переменной - тега;
	 * null - тег может быть любой переменной */
	String getTagVar();
	
	/** имя переменной - сет;
	 * null - сет может быть любой переменной */
	String getSetVar();
	
	/** имя переменной - вытесняемого тега;
	 * null - вытесняемый тег может быть любой переменной */
	String getVTagVar();
}
