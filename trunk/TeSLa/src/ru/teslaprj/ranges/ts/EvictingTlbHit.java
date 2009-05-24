package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.Range;
import ru.teslaprj.scheme.Command;

public class EvictingTlbHit extends Range {

	public EvictingTlbHit(Command cmd, Set<Command> evictings)
	{
		super(cmd);
	}

}
