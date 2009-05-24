package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.Range;
import ru.teslaprj.scheme.Command;

public class EvictingL1Hit extends Range {

	public EvictingL1Hit(Command cmd, Set<Command> evictings)
	{
		super(cmd);
	}

}
