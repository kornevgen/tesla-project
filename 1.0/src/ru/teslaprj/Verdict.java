/**
 * 
 */
package ru.teslaprj;

import java.math.BigInteger;
import java.util.List;
import java.util.Map;

import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.Definition;

public interface Verdict
{
	Map<Definition, BigInteger> getDefinitionValues();
	/**
	 * @return [ set +> tag-list ]-list (by levels)
	 */
	List< Map<Long, List<Long>> > getCacheInitialization();

	Map<Integer, TLBRow> getTlbrows();

	Map<BigInteger, BigInteger> getMemory();
	
	Map<Command, Integer> getTLBIndexes();
}