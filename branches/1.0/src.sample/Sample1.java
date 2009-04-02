import java.io.File;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Map;

import ru.teslaprj.Solver;
import ru.teslaprj.Verdict;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.RegisterDefinition;
import ru.teslaprj.scheme.Scheme;

public class Sample1
{
	public static void main( String[] args )
	{
		try
		{
			// 1. сформировать схему
			Scheme scheme = new Scheme() ;
			scheme.addDefinition( new RegisterDefinition( "x", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "z", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "t", 64 ) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "x", "y", "z" )
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "z", "x", "y" )
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "z", "x" )
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "z", "t" )
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "t", "x" )
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "y", "t" )
							, "noexception"
							, null
						) );
					
			// 2. вызвать решатель
			Solver solver = new Solver( 
					new File("src.sample" )
					, new File("clp")
				);

			Verdict verdict = solver.solve(scheme, null, null );
			
			// 3. распечатать ответ
			Map<Definition, BigInteger> values = verdict.getDefinitionValues();
			for( Definition def : values.keySet() )
			{
				System.out.println( def + " = " + values.get( def ) );	
			}
		}
		catch( Exception e )
		{
			e.printStackTrace();
		}
	}
}