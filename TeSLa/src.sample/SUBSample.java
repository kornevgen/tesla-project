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
	List<Cache> cacheLevels;
	
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
						  "SUB",                          // Instruction name
						  Arrays.asList( "x", "y", "z" ), // Params 
						  "overflow",                     // Test situation name
						  null                            // Test situation params
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
    
		cacheLevels = new ArrayList<Cache>();
		cacheLevels.add( new Cache()
		{
			@Override
			public int getSectionNumber()
			{
				return 4;
			}

			@Override
			public int getAddressBitLength() {
				return 32;
			}

			@Override
			public int getTagBitLength() {
				return 10;
			}

			@Override
			public int getSetNumberBitLength() {
				return 10;
			}

			@Override
			public long getTag(int section, int row) {
				// TODO Auto-generated method stub
				return 0;
			}
		} );

        // Postcondition
        if(solver == null) { throw new IllegalStateException("Solver is null"); }
        if(scheme == null) { throw new IllegalStateException("Scheme is null"); }
	}
	
	public void solve()
	{
        // 2. вызвать решатель
        try 
			{ verdict = solver.solve(scheme, cacheLevels, null); }/*new TLB(){

				@Override
				public int getBufferSize() {
					return 3;
				}

				@Override
				public int getSize() {
					return 5;
				}

				@Override
				public int getMaximumOfMask() {
					return 2;
				}

				@Override
				public int getRangeEndBit() {
					return 10;
				}

				@Override
				public int getRangeStartBit() {
					return 10;
				}

				@Override
				public int getVPNd2EndBit() {
					return 9;
				}

				@Override
				public int getVPNd2StartBit() {
					return 6;
				}

				@Override
				public int getVirtualAddressBitLen() {
					return 64;
				}

				@Override
				public int getPFNBitLen() {
					return 20;
				}

				@Override
				public int getPhysicalAddressBitLen() {
					return 32;
				}

				public int getASIDBitLen() {
					return 2;
				}}); } */
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