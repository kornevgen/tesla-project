import java.io.File;
import java.math.BigInteger;
import java.util.*;

import ru.teslaprj.*;
import ru.teslaprj.scheme.*;

public class SUBSample 
{
	Scheme scheme;
	Solver solver;
	Verdict verdict;
	
	public SUBSample()
	{
		// Create scheme
		scheme = new Scheme();
		
		// 1. сформировать схему
		try 
		{
			// Add registers definitions to scheme
			scheme.addDefinition( new RegisterDefinition( "x", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "z", 64 ) );
			
			scheme.addCommand( 
				new Command(
						  scheme,
						  "SUB",                          // Instruction name
						  Arrays.asList( "x", "y", "z" ), // Params 
						  "overflow"                      // Test situation name
					) );
		} 
		catch (CommandDefinitionError e) 
		{
			e.printStackTrace();
		}
		
        solver = new Solver( 
                new File("src.sample" ), 
                new File("clp")
        		);
    
        // Postcondition
        if(solver == null) { throw new IllegalStateException("Solver is null"); }
        if(scheme == null) { throw new IllegalStateException("Scheme is null"); }
	}
	
	public void solve()
	{
        // 2. вызвать решатель
        try 
			{ //verdict = 
				solver.solve(scheme, null, null); }
		catch (Exception e) 
			{ e.printStackTrace(); }
		
		// Postcondition
		if (verdict == null) { throw new IllegalStateException("verdict is null"); }		
	}
	
	public void printVerdict()
	{
		// 3. распечатать ответ
		Map<Definition, BigInteger> values = verdict.getDefinitionValues();
		for( Definition def : values.keySet() )
		{
			System.out.println( def + " = 0x" + (values.get( def )).toString(16) );	
		}
	}
		
	public static void main( String[] args )
	{
		SUBSample subSample = new SUBSample();
		subSample.solve();
		subSample.printVerdict();
	}
}