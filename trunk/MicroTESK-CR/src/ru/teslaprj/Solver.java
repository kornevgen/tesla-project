package ru.teslaprj;

import java.io.File;
import java.util.Map;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.jruby.RubyHash;

public class Solver
{
	private String path_to_ruby_sources;
	private String jruby_home;
	private String jruby_lib;
	private String jruby_shell;
	private String jruby_script;
	protected Microprocessor microprocessor;
	protected ScriptEngine rubyEngine;
	protected ScriptContext context;

	
	protected Map<Parameter, Value> solve( Template template, boolean with_binsearch )
		throws Unsat, Timeout
	{
		if ( ! is_initialized() )
			throw new IllegalStateException( "Solver isn't initialized" );

		try { initialize_rubyEngine();
		
		Map<Parameter, Value> init = ruby_cycle(template, all_max(template) );
		
		if ( with_binsearch )
		{
				// assert "solution exists"  --> start to minimize solution
				init = binsearch( template );
		}
		
		return init;
		
		} catch( ScriptException e ) { throw new IllegalStateException( e.getMessage() );  }
	}
	
	protected void initialize_rubyEngine()
		throws ScriptException
	{
        System.setProperty("jruby.base", jruby_home);
        System.setProperty("jruby.home", jruby_home);
        System.setProperty("jruby.lib", jruby_lib);
        System.setProperty("jruby.shell", jruby_shell);
        System.setProperty("jruby.script", jruby_script);

		ScriptEngineManager m = new ScriptEngineManager();
		rubyEngine = m.getEngineByName("jruby");
		System.setProperty("org.jruby.embed.class.path", jruby_home);
		context = rubyEngine.getContext();
		rubyEngine.eval("require 'rubygems'", context);
		rubyEngine.eval("gem 'jrexml'", context);
		rubyEngine.eval("$LOAD_PATH << '" + path_to_ruby_sources + "'" , context);

		rubyEngine.eval( "$instructionsPath = '" + microprocessor.getInstructionsPath() + "'", context );
		
		//TODO read from microprocessor.tables: rubyEngine.eval( "$L1ASSOC = " .... )
	}

	protected Map<Table, Integer> all_max(Template template)
	{
		// TODO Auto-generated method stub
		return null;
	}

	protected Map<Parameter, Value> binsearch(Template template) {
		// TODO Auto-generated method stub
		return null;
	}

	protected Map<Parameter, Value> ruby_cycle(
				Template template,
				Map<Table, Integer> init_lengths )
					throws Unsat, Timeout, ScriptException
	{
		for( Table t : init_lengths.keySet() )
		{
			rubyEngine.eval( "$initlength_" + t.getName() + " = " + init_lengths.get(t), context );
		}
		
		Object o = rubyEngine.eval(
					"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
						template.getXML() + "\")", context );
		if ( o == "unsat" )
			throw new Unsat();
		if ( o == "timeout" )
			throw new Timeout();
		
		Map<String, Long> model = parse_ruby_hash( (RubyHash)o );
		return filterModel( model, template );
	}
	
	

	private Map<Parameter, Value> filterModel(
			Map<String, Long> model,
			Template template )
	{
		// TODO Auto-generated method stub
		return null;
	}

	private Map<String, Long> parse_ruby_hash( RubyHash o)
	{
		// TODO parse as plain RubyHash
		return null;
	}

	protected void setMicroprocessor(Microprocessor microprocessor)
	{
		if ( ! new File( microprocessor.getInstructionsPath() ).exists() )
			throw new IllegalArgumentException( "Isn't exist: " + microprocessor.getInstructionsPath() );
		this.microprocessor = microprocessor;
	}

	/**
	 * 
	 * @return microprocessor is initialized and paths are initialized
	 */
	protected boolean is_initialized()
	{
		return microprocessor != null &&
				path_to_ruby_sources != null &&
				jruby_home != null &&
				jruby_lib != null &&
				jruby_shell != null &&
				jruby_script != null;
	}

	protected void setPathToRubySources(String path)
	{
		if ( ! new File( path ).exists() )
			throw new IllegalArgumentException( "Isn't exist: " + path );
		path_to_ruby_sources = path;
	}

	protected void setJrubyHome(String path)
	{
		if ( ! new File( path ).exists() )
			throw new IllegalArgumentException( "Isn't exist: " + path );
		jruby_home = path;
	}

	protected void setJrubyLib(String path)
	{
		if ( ! new File( path ).exists() )
			throw new IllegalArgumentException( "Isn't exist: " + path );
		jruby_lib = path;
	}

	protected void setJrubyShell(String path)
	{
		if ( ! new File( path ).exists() )
			throw new IllegalArgumentException( "Isn't exist: " + path );
		jruby_shell = path;
	}

	protected void setJrubyScript(String path)
	{
		if ( ! new File( path ).exists() )
			throw new IllegalArgumentException( "Isn't exist: " + path );
		jruby_script = path;
	}

}
