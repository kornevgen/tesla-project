package ru.teslaprj;

public class Generator
{
	private Solver solver = new Solver();
	private TextConstructor text_constructor = null;

	public Program generate( Template template, boolean with_binsearch )
		throws Unsat, Timeout
	{
		if (! is_initialized() )
			throw new IllegalStateException("Generator isn't initialized");
		
		return text_constructor.build_initialization(
					solver.solve(template, with_binsearch ) );
	}

	
	private boolean is_initialized()
	{
		return is_constructor_initialized()
			&& is_solver_initialized();
	}


	private boolean is_solver_initialized() {
		return solver.is_initialized();
	}


	private boolean is_constructor_initialized() {
		return text_constructor != null;
	}


	/**
	 * @param text_constructor the text_constructor to set
	 */
	public void setTextConstructor(TextConstructor text_constructor) {
		this.text_constructor = text_constructor;
	}
	
	public void setMicroprocessor( Microprocessor microprocessor )
	{
		solver.setMicroprocessor( microprocessor );
	}
	
	public void setPathToRubySources( String path )
	{
		solver.setPathToRubySources( path );
	}
	
	public void setJrubyHome( String path )
	{
		solver.setJrubyHome( path );
	}

	public void setJrubyLib( String path )
	{
		solver.setJrubyLib( path );
	}

	public void setJrubyShell( String path )
	{
		solver.setJrubyShell( path );
	}

	public void setJrubyScript( String path )
	{
		solver.setJrubyScript( path );
	}
}
