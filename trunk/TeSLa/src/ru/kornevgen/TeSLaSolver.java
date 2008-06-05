package ru.kornevgen;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import com.parctechnologies.eclipse.CompoundTerm;
import com.parctechnologies.eclipse.EclipseEngine;
import com.parctechnologies.eclipse.EclipseEngineOptions;
import com.parctechnologies.eclipse.EclipseException;
import com.parctechnologies.eclipse.EmbeddedEclipse;
import com.parctechnologies.eclipse.Fail;

public abstract class TeSLaSolver 
{
	private final static int WORD_VALUE = 51;
	
	/**
	 * компилирует данный файл с описанием ситуации в 
	 * промежуточное представление
	 * 
	 * @param input		файл с описанием ситуации
	 * @param output	файл с промежуточным представлением
	 * @return			список параметров данной ситуации
	 * @throws IOException	возникает при проблемах с файлами
	 */
	public static List<LogicalVariable> compile( final String input, final String output )
		throws IOException, RuntimeException, RecognitionException
	{
		try
		{
			CharStream inputStream = new ANTLRFileStream( input );
			TeSLaLexer lexer = new TeSLaLexer( inputStream );
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			TeSLaParser parser = new TeSLaParser(tokens);
			return parser.program( output );
		}
		catch( IOException e )
		{
			throw e;
		}
	}
	
	/**
	 * запускает файл с промежуточным представлением ситуации 
	 * с учетом данных значений параметров
	 * 
	 * @param input			путь к файлу с промежуточным представлением ситуации
	 * @param fixedValues	список объектов с описанием параметров: если значение элемента этого списка не null, то оно будет использовано при нахождении значений остальных параметров
	 * @return				список объектов-параменных со значениями
	 * @throws Fail			возникает при отсутствии решения
	 * @throws IOException	ошибка ввода/вывода
	 * @throws EclipseException	другая ошибка инструмента ECLiPSe
	 */
	public static List<LogicalVariable> solve( final String input, final List<LogicalVariable> fixedValues )
		throws Fail, IOException, EclipseException
	{
		return generateValues( callECLiPSe( input, fixedValues ), fixedValues );
	}
	
	
    /**
     * запускает ECLiPSe с учетом заданных значений некоторых параметров и возвращает результат выполнения в виде ECLiPSe-терма
     * 
     * @param filePath			путь к файлу с ECLiPSe-программой
     * @param fixedValues		список переменных, возможно, с заданными значениями переменных
     * @return					терм с результатом работы ECLiPSe
     * @throws EclipseException	ошибка запуска ECLiPSe
     * @throws IOException		ошибка ввода/вывода
     */
    private static CompoundTerm callECLiPSe( final String filePath, final List<LogicalVariable> fixedValues ) 
    	throws EclipseException, IOException
    {
        // Object representing the Eclipse process
        EclipseEngine eclipse = null;

    	try
    	{
	        // Create some default Eclipse options
	        EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();
	
	        // Path of the Eclipse program
	        File eclipseProgram;
	
	        // Connect the Eclipse's standard streams to the JVM's
	        eclipseEngineOptions.setUseQueues(false);
	
	        // Initialise Eclipse
	        eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
		
	        // Set up the path of the example Eclipse program to be used.
	        eclipseProgram = new File( filePath );
	
	        // Compile the eclipse program.
	        eclipse.compile(eclipseProgram );
	
	        StringBuffer goal = new StringBuffer();
	        goal.append( "main:go(_" );
	        for( LogicalVariable var : fixedValues )
	        {
	        	if ( var.getValue() == null )
	        		goal.append( ",_" );
	        	else
	        		goal.append( "," + var.getValue() );
	        }
	        goal.append(")");
	        
	        CompoundTerm result = eclipse.rpc( goal.toString() );
	        
	        return (CompoundTerm) result.arg(2);
    	}
    	finally
    	{
	        // Destroy the Eclipse process
    		try { ((EmbeddedEclipse) eclipse).destroy(); } catch( IOException e ) {}
    	}
    }

    /**
     * строит список переменных со значениями по данному терму (конвертирует представление терма)
     * 
     * @param logicalResult	данный терм, который будет преобразовываться в список переменных
     * @param parameters	список переменных (будет возвращена копия этого списка, но с проставленными значениями переменных)
     * @return				список переменных, у каждой из которых проставлено значение переменных
     */
    private static List<LogicalVariable> generateValues( final CompoundTerm logicalResult, final List<LogicalVariable> parameters )
    {
    	List<LogicalVariable> values = new ArrayList<LogicalVariable>( parameters );
    			
		for( int i = 0; i < parameters.size(); i++ )
		{
			List arg = (List)logicalResult.arg( i + 2 );
			BigInteger a;
			if ( arg.get(0) instanceof Integer )
				a = BigInteger.valueOf( (Integer)arg.get( 0 ) );
			else
				a = BigInteger.valueOf( (Long)arg.get( 0 ) );
			BigInteger pow = BigInteger.valueOf( 2 ).pow( WORD_VALUE );
			System.out.print( "( " + arg.get(0) );
			if ( arg.size() > 1 )
				for( Object argValue : arg.subList(1, arg.size()) )
				{
					// a := a * 2^C + argValue
					if ( argValue instanceof Integer )
						a = a.multiply( pow ).add( BigInteger.valueOf( ((Integer)argValue).longValue() ));
					else
						a = a.multiply( pow ).add( BigInteger.valueOf( ((Long)argValue).longValue() ));
					System.out.print( ", " + argValue );
				}
			System.out.println(" )" );
			values.get(i).setValue( a );
		}
		
		return values;
    }
}