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
			return parser.program( output );
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