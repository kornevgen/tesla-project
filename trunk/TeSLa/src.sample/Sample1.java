import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import ru.teslaprj.Cache;
import ru.teslaprj.Solver;
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
							, null
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "z", "x", "y" )
							, null
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "z", "x" )
							, null
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "z", "t" )
							, null
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "t", "x" )
							, null
							, "overflow"
							, null
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "y", "y", "t" )
							, null
							, "noexception"
							, null
						) );
					
			// 2. вызвать решатель
			Solver solver = new Solver( 
					new File("C:\\Documents and Settings\\kornevgen\\My Documents\\diss\\ConstGen\\src\\TeSLa\\src.sample" )
					, new File("C:\\Documents and Settings\\kornevgen\\My Documents\\diss\\ConstGen\\src\\TeSLa\\clp")
				);
			Solver.Verdict verdict = solver.solve(scheme, new ArrayList<Cache>() );
			
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