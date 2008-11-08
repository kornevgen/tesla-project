package ru.teslaprj.scheme;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class Command implements Commandlike
{
	public Command(
			String cop,
			List<String> args,
			List<String> additionalArgs, 
			String testSituation,
			Map<String, Set<ProcedureTestSituation>> testSituationParameters )
		throws CommandDefinitionError
	{
		if ( additionalArgs == null )
			this.additionalArgs = new ArrayList<String>();
		else
			this.additionalArgs = additionalArgs;
		
		if ( args == null )
			this.args = new ArrayList<String>();
		else
			this.args = args;
		
		if ( cop == null )
			throw new CommandDefinitionError("`cop` is null");
		
		this.cop = cop;
		
		if ( testSituation == null )
			throw new CommandDefinitionError("`testSituation` is null");
		
		this.testSituation = testSituation;
		
		if ( testSituationParameters == null )
			this.testSituationParameters = new HashMap<String, Set<ProcedureTestSituation>>();
		else
		{
			for( String paramName : testSituationParameters.keySet() )
			{
				if ( paramName != "LoadMemory" )
					throw new CommandDefinitionError( "unknown test situation parameter: " + paramName );
			}
			this.testSituationParameters = testSituationParameters;
		}
	}
	public String getCop() {
		return cop;
	}
	public List<String> getArgs() {
		return args;
	}
	public List<String> getAdditionalArgs() {
		return additionalArgs;
	}
	public String getTestSituation() {
		return testSituation;
	}
	public Map<String, Set<ProcedureTestSituation>> getTestSituationParameters() {
		return testSituationParameters;
	}

	private String cop;
	private List<String> args;
	private List<String> additionalArgs;
	private String testSituation;
	private Map<String, Set<ProcedureTestSituation> > testSituationParameters;
}
