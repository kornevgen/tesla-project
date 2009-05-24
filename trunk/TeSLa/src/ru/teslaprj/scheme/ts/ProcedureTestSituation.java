package ru.teslaprj.scheme.ts;

import ru.teslaprj.ranges.tsiterators.CommonIterator;
import ru.teslaprj.scheme.Command;

public abstract class ProcedureTestSituation 
{
	public abstract CommonIterator iterator();

	Command cmd = null;
	public void setCommand( Command cmd )
	{
		this.cmd = cmd;
	}
	
	public Command getCommand()
	{
		return cmd;
	}
}
