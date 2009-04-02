package ru.teslaprj.constraints.args;

import ru.teslaprj.constraints.Argument;

public interface PhysicalAddressPart extends Argument
{
	int getEndBit();
	
	int getStartBit();
}
