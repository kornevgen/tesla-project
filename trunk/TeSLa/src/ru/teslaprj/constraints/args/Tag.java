package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;
import ru.teslaprj.scheme.Command;

public interface Tag extends Argument
{
	int getLevel();

	Command getCommand();
}
