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
	 * ����������� ������ ���� � ��������� �������� � 
	 * ������������� �������������
	 * 
	 * @param input		���� � ��������� ��������
	 * @param output	���� � ������������� ��������������
	 * @return			������ ���������� ������ ��������
	 * @throws IOException	��������� ��� ��������� � �������
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
	 * ��������� ���� � ������������� �������������� �������� 
	 * � ������ ������ �������� ����������
	 * 
	 * @param input			���� � ����� � ������������� �������������� ��������
	 * @param fixedValues	������ �������� � ��������� ����������: ���� �������� �������� ����� ������ �� null, �� ��� ����� ������������ ��� ���������� �������� ��������� ����������
	 * @return				������ ��������-���������� �� ����������
	 * @throws Fail			��������� ��� ���������� �������
	 * @throws IOException	������ �����/������
	 * @throws EclipseException	������ ������ ����������� ECLiPSe
	 */
	public static List<LogicalVariable> solve( final String input, final List<LogicalVariable> fixedValues )
		throws Fail, IOException, EclipseException
	{
		return generateValues( callECLiPSe( input, fixedValues ), fixedValues );
	}

	/**
	 * �� ������ �������� �������� �������� ���������, � ���� ��������� �� ���� ��������� ��������
	 * 
	 * @param input					���� � ����� � ��������� �������� ��������
	 * @return						������ ���������� (��� ����� ��������� ����������)
	 * @throws IOException			���� �������� � ������ �������� �������� �������� (��������, �� ����������)
	 * @throws RecognitionException ���� ���� �������� �����, ��� �� �������� ��������� �������� ��������
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
     * ��������� ECLiPSe � ������ �������� �������� ��������� ���������� � ���������� ��������� ���������� � ���� ECLiPSe-�����
     * 
     * @param filePath			���� � ����� � ECLiPSe-����������
     * @param fixedValues		������ ����������, ��������, � ��������� ���������� ����������
     * @return					���� � ����������� ������ ECLiPSe
     * @throws EclipseException	������ ������� ECLiPSe
     * @throws IOException		������ �����/������
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
     * ������ ������ ���������� �� ���������� �� ������� ����� (������������ ������������� �����)
     * 
     * @param logicalResult	������ ����, ������� ����� ����������������� � ������ ����������
     * @param parameters	������ ���������� (����� ���������� ����� ����� ������, �� � �������������� ���������� ����������)
     * @return				������ ����������, � ������ �� ������� ����������� �������� ����������
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