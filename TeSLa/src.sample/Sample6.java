import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.Solver;
import ru.teslaprj.TLB;
import ru.teslaprj.TLBRow;
import ru.teslaprj.Verdict;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ConstDefinition;
import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.RegisterDefinition;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;
import ru.teslaprj.scheme.ts.TLBHit;

public class Sample6
{
	public static void main( String[] args )
	{
		try
		{
			// 1. сформировать схему
			Scheme scheme = new Scheme() ;
			scheme.addDefinition( new RegisterDefinition( "x", 32 ) );
			scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "z", 64 ) );
			scheme.addDefinition( new RegisterDefinition( "u", 64 ) );
			scheme.addDefinition( new ConstDefinition( "c", 16 ) );
			Map<String, Set<ProcedureTestSituation>> m1 = new HashMap<String, Set<ProcedureTestSituation>>();
			Set<ProcedureTestSituation> m1ts = new HashSet<ProcedureTestSituation>();
			m1ts.add( new CacheMiss()
			{
				@Override
				public int getLevel() {
					return 1;
				}

				@Override
				public String getSetVar() {
					return null;
				}

				@Override
				public String getTagVar() {
					return null;
				}

				@Override
				public String getVTagVar() {
					return null;
				}
			});
			Set<ProcedureTestSituation> m11ts = new HashSet<ProcedureTestSituation>();
			m11ts.add( new TLBHit(){
			@Override
				public String getPhysicalAddressVar() {
					return null;
				}

				@Override
				public String getVirtualAddressVar() {
					return null;
				}

				@Override
				public Integer getG() {
					return null;
				}

				@Override
				public int getValid() {
					return 1;
				}

				@Override
				public int getmoDify() {
					return 1;
				}} );
			m1.put( "LoadMemory", m1ts );
			m1.put( "AddressTranslation", m11ts);
			scheme.addCommand( 
					new Command(
							  "LW"
							, Arrays.asList( "x", "y", "c" )
							, "noexc"
							, m1
						) );
			scheme.addCommand(
					new Command(
							  "SUB"
							, Arrays.asList( "z", "y", "u" )
							, "noexception"
							, null
						) );

			// 2. вызвать решатель
			Solver solver = new Solver( 
					new File("C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa\\src.sample" )
					, new File("C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa\\clp")
				);
			List<Cache> cacheLevels = new ArrayList<Cache>();
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
			} );
			Verdict verdict = solver.solve(scheme, cacheLevels, new TLB(){

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

				@Override
				public int getASIDBitLen() {
					return 3;
				}} );
			
			// 3. распечатать ответ
			Map<Definition, BigInteger> values = verdict.getDefinitionValues();
			for( Definition def : values.keySet() )
			{
				System.out.println( def + " = " + values.get( def ) );	
			}

			List<Map<Long, List<Long>>> caches = verdict.getCacheInitialization();
			for( Cache cache : cacheLevels )
			{
				int level = cacheLevels.indexOf( cache );
				Map<Long, List<Long>> sets = caches.get( level );
				for( long setNumber : sets.keySet() )
				{
					System.out.print( 
							"level " + ( level + 1 ) + 
							": set " + setNumber + ": _ " );
					for( Long tag : sets.get( setNumber ) )
					{
						System.out.print( ", " + tag );
					}
					System.out.println();
				}
			}
			
			Map<Integer, TLBRow> tlb = verdict.getTlbrows();
			for( TLBRow row : tlb.values() )
			{
				System.out.println( "tlb:" +
						" range = " + row.getRange() +
						", vpn/2 = " + row.getVPNd2() +
						", mask = " + row.getMask() + 
						", pfn0 = " + row.getPFN0() + 
						", pfn1 = " + row.getPFN1()
					);
			}
			
			Map<BigInteger, BigInteger> memory = verdict.getMemory();
			for( BigInteger address : memory.keySet() )
			{
				System.out.println( "memory[ " + address + " ] = " + memory.get(address) );
			}
		}
		catch( Exception e )
		{
			e.printStackTrace();
		}
	}
}