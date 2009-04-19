package ru.teslaprj.constraints.pfn;

import java.util.Set;

public interface PFNInitialConstraint extends PFNConstraint
{
	String getPfn();
	Set<Argument> getArgs();
}
