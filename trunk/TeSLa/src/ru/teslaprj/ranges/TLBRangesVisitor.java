package ru.teslaprj.ranges;

import ru.teslaprj.ranges.ts.EvictingTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbMiss;
import ru.teslaprj.ranges.ts.UnusefulTlbMiss;
import ru.teslaprj.ranges.ts.UsefulTlbMiss;

public interface TLBRangesVisitor
{
	void visitInitialTlbHit( InitialTlbHit range );
	void visitEvictingTlbHit( EvictingTlbHit range );
	void visitInitialTlbMiss( InitialTlbMiss range );
	void visitUnusefulTlbMiss( UnusefulTlbMiss range );
	void visitUsefulTlbMiss( UsefulTlbMiss range );
}
