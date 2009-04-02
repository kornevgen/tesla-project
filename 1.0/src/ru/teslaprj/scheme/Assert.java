package ru.teslaprj.scheme;

import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class Assert extends Command
{
	public Assert(
			List<String> args,
			String testSituation,
			Map<String, Set<ProcedureTestSituation>> testSituationParameters
		)
		throws CommandDefinitionError
	{
		super("", args, testSituation, testSituationParameters );
	}
}
