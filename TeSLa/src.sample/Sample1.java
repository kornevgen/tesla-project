import java.io.File;
import java.util.Arrays;

import ru.teslaprj.Solver;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.RegisterDefinition;
import ru.teslaprj.scheme.Scheme;

public class Sample1
{
	public static void main( String[] args )
	{
		try
		{
			// 1. ������������ �����
			Scheme scheme = new Scheme() ;
			scheme.addDefinition( new RegisterDefinition( "x", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "z", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "t", 64 ) );
			scheme.addCommand( 
					new Command(
							  scheme
							, "ADD"
							, Arrays.asList( "x", "y", "z" )
							, "overflow"
						) );
			scheme.addCommand( 
					new Command(
							  scheme
							, "ADD"
							, Arrays.asList( "z", "x", "y" )
							, "overflow"
						) );
			scheme.addCommand( 
					new Command(
							  scheme
							, "ADD"
							, Arrays.asList( "y", "z", "x" )
							, "overflow"
						) );
			scheme.addCommand( 
					new Command(
							  scheme
							, "ADD"
							, Arrays.asList( "y", "z", "t" )
							, "overflow"
						) );
			scheme.addCommand( 
					new Command(
							  scheme
							, "ADD"
							, Arrays.asList( "y", "t", "x" )
							, "overflow"
						) );
			scheme.addCommand( 
					new Command(
							  scheme
							, "ADD"
							, Arrays.asList( "y", "y", "t" )
							, "noexception"
						) );
					
			// 2. ������� ��������
			Solver solver = new Solver( 
					new File("src.sample" )
					, new File("clp")
				);

		//	Verdict verdict = 
				solver.solve(scheme, null, null );
			
//			// 3. ����������� �����
//			Map<Definition, BigInteger> values = verdict.getDefinitionValues();
//			for( Definition def : values.keySet() )
//			{
//				System.out.println( def + " = " + values.get( def ) );	
//			}
		}
		catch( Exception e )
		{
			e.printStackTrace();
		}
	}
}