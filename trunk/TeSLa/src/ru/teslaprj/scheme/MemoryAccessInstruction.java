package ru.teslaprj.scheme;

import ru.teslaprj.Cache;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.TLBSituation;

interface MemoryAccessInstruction
{
	String getTagset();
	boolean isLOAD();
	boolean isSTORE();
	boolean hasTLBSituation();
	boolean hasCacheSituation( Cache cacheLevel );
	TLBSituation getTLBSituation();
	CacheTestSituation getCacheSituation( Cache cacheLevel );
}
