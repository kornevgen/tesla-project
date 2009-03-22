package ru.teslaprj.scheme.ts;

public interface CacheMiss extends CacheTestSituation
{
	/** имя переменной - вытесняемого тега;
	 * null - вытесняемый тег может быть любой переменной */
	String getVTagVar();
}
