package ru.teslaprj.scheme;

import java.util.ArrayList;
import java.util.List;

public class Assert implements Commandlike
{
	public Assert(
			List<String> args,
			String testSituation )
		throws CommandDefinitionError
	{
		
		if ( args == null )
			this.args = new ArrayList<String>();
		else
			this.args = args;
		
		if ( testSituation == null )
			throw new CommandDefinitionError("`testSituation` is null");
		
		this.testSituation = testSituation;
	}
	public List<String> getArgs() {
		return args;
	}
	public String getTestSituation() {
		return testSituation;
	}

	private List<String> args;
	private String testSituation;
	
	@Override
	public List<String> getAdditionalArgs() {
		return new ArrayList<String>();
	}
	
	@Override
	public String getCop()
	{
		return "";
	}
}
