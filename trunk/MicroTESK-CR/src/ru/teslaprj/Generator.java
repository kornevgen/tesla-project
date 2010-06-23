package ru.teslaprj;

public class Generator
{
	private Solver solver;
	private TextConstructor constructor;
	
	public void setTextConstructor( TextConstructor c )
	{
		constructor = c;
	}
	
	public void run(XMLTemplate template)
	{
		//TODO 1. вся ли конфигурация задана?
		if ( constructor == null )
			throw new IllegalStateException("TextConstructor isn't set yet");
		
		//TODO 2. вызвать ruby, который сгенерирует и решит ограничения
		solver.solve(template);
		
		//TODO 3. вызвать constructor
	}

}
