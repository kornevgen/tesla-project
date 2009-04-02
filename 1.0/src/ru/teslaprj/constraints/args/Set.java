package ru.teslaprj.constraints.args;

import ru.teslaprj.Cache;
import ru.teslaprj.constraints.Argument;
import ru.teslaprj.scheme.Command;

public interface Set extends Argument
{
	Cache getCache();

	Command getCommand();
}
