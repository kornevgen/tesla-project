package ru.teslaprj.ranges;

import ru.teslaprj.ranges.ts.EvictingTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbMiss;

public interface TLBRangesVisitor
{
	void visitInitialTlbHit( InitialTlbHit range );
	void visitEvictingTlbHit( EvictingTlbHit range );
	void visitInitialTlbMiss( InitialTlbMiss range );
	void visitBlockTlbMiss( BlockRange.TLB range );
}
