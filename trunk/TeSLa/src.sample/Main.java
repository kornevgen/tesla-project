//import java.io.IOException;
//import java.util.List;
//
//import org.antlr.runtime.RecognitionException;
//
//import ru.teslaprj.syntax.LogicalVariable;
//import ru.teslaprj.syntax.SemanticException;
//import ru.teslaprj.syntax.TeSLaParser;
//
//import com.parctechnologies.eclipse.EclipseException;
//import com.parctechnologies.eclipse.Fail;
//
public class Main
{
    public static void main(final String[] args)
    {
//    	final String path = ".\\clp\\main.ecl";
    	
    	if ( args.length < 1 )
    	{
    		System.out.println("One argument with input file path is needed");
    		return;
    	}
    	
//    	try
//    	{
//    		List<LogicalVariable> parameters = TeSLaArithmeticSolver.compile( args[0], path );
//
//    		parameters = TeSLaArithmeticSolver.solve( path, parameters );
//    		
//    		for( LogicalVariable var: parameters )
//    		{
//    			System.out.println( var.getCanonicalName() + " = " + var.getValue() );
//    		}
//    	}
//    	catch( Fail t )
//    	{
//    		System.out.println("no solutions");
//    	}
//    	catch( IOException e )
//    	{
//    		System.err.println("I/O error: " + e );
//    		e.printStackTrace( System.err );
//    	}
//    	catch( EclipseException e )
//    	{
//    		System.err.println("ECLiPSe error: " + e);
//    		e.printStackTrace( System.err );
//    	}
//    	catch( TeSLaParser.ParserException e )
//    	{
//    	}
//    	catch( SemanticException e )
//    	{
//    		System.err.println( "Semantic error: " + e.getMessage() );
//    	}
//    	catch( RecognitionException e )
//    	{
//    		System.err.println( "Error: " + e.getMessage() );
//    	}
//    	catch( Exception e )
//    	{
//    		System.err.println( "Internal error: " + e.getMessage() );
//    		e.printStackTrace( System.err );
//    	}
    }
}
