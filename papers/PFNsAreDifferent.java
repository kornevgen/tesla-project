package ru.teslaprj.constraints.pfn;

import java.util.Set;

/**
 * alldifferent( pfn1, pfn2, ..., pfnN)
 */
public interface PFNsAreDifferent extends PFNConstraint
{
	Set<String> getPfns();
}
