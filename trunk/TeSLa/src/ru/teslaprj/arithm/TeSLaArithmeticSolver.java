package ru.teslaprj.arithm;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import ru.teslaprj.arithm.TeSLaLexer;
import ru.teslaprj.arithm.TeSLaParser;

import com.parctechnologies.eclipse.CompoundTerm;
import com.parctechnologies.eclipse.EclipseEngine;
import com.parctechnologies.eclipse.EclipseEngineOptions;
import com.parctechnologies.eclipse.EclipseException;
import com.parctechnologies.eclipse.EmbeddedEclipse;
import com.parctechnologies.eclipse.Fail;

/**
 * @deprecated
 * @author kornevgen
 */
public abstract class TeSLaArithmeticSolver 
{
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
			return parser.program( output, false );
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
	 * не парсит описание тестовой ситуации полностью, а лишь извлекает из него параметры операции
	 * 
	 * @param input					путь к файлу с описанием тестовой ситуации
	 * @return						список переменных (его можно заполнять значениями)
	 * @throws IOException			если проблема с файлом описания тестовой ситуации (например, не существует)
	 * @throws RecognitionException если файл содержит нечто, что не является описанием тестовой ситуации
	 */
	public static List<LogicalVariable> getParameters( final String input )
		throws IOException, RecognitionException
	{
		try
		{
			CharStream inputStream = new ANTLRFileStream( input );
			TeSLaLexer lexer = new TeSLaLexer( inputStream );
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			TeSLaParser parser = new TeSLaParser(tokens);
			return parser.program( null, true );
		}
		catch( IOException e )
		{
			throw e;
		}
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
	        		goal.append( ", \"" + var.getValue() + "\"" );
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
			String arg = (String)logicalResult.arg( i + 2 );			
			values.get(i).setValue( BitLen.string2bigint( arg ) );
		}
		
		return values;
    }
}