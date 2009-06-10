package ru.teslaprj.ranges;

import ru.teslaprj.ranges.ts.EvictingTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbHit;
import ru.teslaprj.ranges.ts.InitialTlbMiss;
import ru.teslaprj.ranges.ts.UnusefulTlbMiss;
import ru.teslaprj.ranges.ts.UsefulTlbMiss;

public interface TLBRangesVisitor
{
	void visitInitialTlbHit( InitialTlbHit range ) throws Inconsistent;
	void visitEvictingTlbHit( EvictingTlbHit range ) throws Inconsistent;
	void visitInitialTlbMiss( InitialTlbMiss range ) throws Inconsistent;
	void visitUnusefulTlbMiss( UnusefulTlbMiss range ) throws Inconsistent;
	void visitUsefulTlbMiss( UsefulTlbMiss range ) throws Inconsistent;
}
