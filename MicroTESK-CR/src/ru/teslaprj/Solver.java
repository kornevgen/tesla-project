package ru.teslaprj;

import java.io.File;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

//import javax.script.ScriptContext;
//import javax.script.ScriptEngine;
//import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.jruby.RubyHash;
import org.jruby.embed.LocalVariableBehavior;
import org.jruby.embed.ScriptingContainer;

public class Solver
{
	private String path_to_ruby_sources;
	private String jruby_home;
	private String jruby_lib;
	private String jruby_shell;
	private String jruby_script;
	protected Microprocessor microprocessor;
//	protected ScriptEngine rubyEngine;
//	protected ScriptContext context;
	protected final ScriptingContainer container;
	protected Map<Table, Integer> init_lengths = null; 

	public Solver()
	{
		container = new ScriptingContainer(LocalVariableBehavior.PERSISTENT);
	}
	
	protected Map<Parameter, BigInteger> solve( Template template, boolean with_binsearch )
		throws Unsat, Timeout
	{
		if ( ! is_initialized() )
			throw new IllegalStateException( "Solver isn't initialized" );

		try { initialize_rubyEngine();
		
		init_lengths  = all_max(template);
		Map<Parameter, BigInteger> init = ruby_cycle(template );
		
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

//		ScriptEngineManager m = new ScriptEngineManager();
//		rubyEngine = m.getEngineByName("jruby");
//		System.setProperty("org.jruby.embed.class.path", jruby_home);
//		context = rubyEngine.getContext();
//		rubyEngine.eval("require 'rubygems'", context);
//		rubyEngine.eval("gem 'jrexml'", context);
//		rubyEngine.eval("$LOAD_PATH << '" + path_to_ruby_sources + "'" , context);
//		rubyEngine.eval( "$instructionsPath = '" + microprocessor.getInstructionsPath() + "'", context );

		container.runScriptlet( "$LOAD_PATH << '" + path_to_ruby_sources + "';" );

		//TODO если instructions_path оканчивается на нечетное количество File.separator,
		// добавить еще один File.separator
		container.put("$instructionsPath", microprocessor.getInstructionsPath() + File.separator);

		//TODO read from microprocessor.tables: rubyEngine.eval( "$L1ASSOC = " .... )
	}

	protected Map<Table, Integer> all_max(Template template)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * не забыть здесь выставлять поле init_lengths
	 * @param template
	 * @return
	 */
	protected Map<Parameter, BigInteger> binsearch(Template template) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * вместо Long пришлось использовать BigInteger из-за того, что Z3 выдает
	 * числа в беззнаковом виде, а в Java тип Long хранит числа в знаковом 
	 * представлении, поэтому не все числа удается сохранить в Long. А для
	 * других битовых размеров при переводе в знаковое представление надо
	 * знать этот размер, что усложнит архитектуру класса.
	 * @param template
	 * @return
	 * @throws Unsat
	 * @throws Timeout
	 * @throws ScriptException
	 */
	protected Map<Parameter, BigInteger> ruby_cycle(Template template)
					throws Unsat, Timeout, ScriptException
	{
		for( Table t : init_lengths.keySet() )
		{
			container.put( "$initlength_" + t.getName(), init_lengths.get(t) );
		}
		
		Object o = container.runScriptlet(
					"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
						template.getXML() + "\")" );
		if ( o == "unsat" )
			throw new Unsat();
		if ( o == "timeout" )
			throw new Timeout();
		
		return convertParametersValues( (RubyHash)o, template );
	}

	private Map<Parameter, BigInteger> convertParametersValues(RubyHash o,
			Template template)
	{
		Map<Parameter, BigInteger> result = new HashMap<Parameter, BigInteger>();
		for( Parameter p : template.getParameters( init_lengths ) )
		{
			String var;
			switch ( p.getType() )
			{
			case REGISTER : var = p.getName() + "_X"; break;
			case KEY: case CONST: var = p.getName(); break;
			default: throw new UnsupportedOperationException( "unknown type of template parameter");
			}
			
			if ( ! o.containsKey( var ) )
			{
				result.put(p, BigInteger.ZERO);
//				throw new IllegalStateException("unknown parameter '" + var + "'" );
			}
			else
				if ( o.get( var ) != null )
					result.put( p, new BigInteger((String)o.get( var ) ) );
		}
		return result;
	}
	
	public void setMicroprocessor(Microprocessor microprocessor)
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

	public Map<Table, Integer> getInitLengths()
	{
		return init_lengths;
	}

}
