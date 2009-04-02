package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;
import ru.teslaprj.scheme.Command;

public interface VirtualAddress extends Argument
{
	Command getCommand();
}
