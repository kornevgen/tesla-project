package ru.teslaprj.ranges.ts;

import java.util.Set;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.scheme.MemoryCommand;

public class EvictingL1Hit extends L1Range {

	public EvictingL1Hit(MemoryCommand cmd, Set<MemoryCommand> evictings)
	{
		super(cmd);
	}

}
