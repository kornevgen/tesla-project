package ru.teslaprj.scheme;

import java.util.List;

public class Assert extends Command
{
	public Assert(
			Scheme scheme,
			List<String> args,
			String testSituation
		)
		throws CommandDefinitionError
	{
		super(scheme,"", args, testSituation);
	}
}
