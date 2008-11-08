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
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ConstDefinition;
import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.RegisterDefinition;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class Sample3
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
			scheme.addDefinition( new ConstDefinition( "c", 16 ) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "x", "y", "z" )
							, null
							, "overflow"
							, null
						) );
			Map<String, Set<ProcedureTestSituation>> m1 = new HashMap<String, Set<ProcedureTestSituation>>();
			Set<ProcedureTestSituation> m1ts = new HashSet<ProcedureTestSituation>();
			m1ts.add( new CacheHit()
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
			});
			m1ts.add( new CacheMiss()
			{
				@Override
				public int getLevel() {
					return 2;
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
			m1.put( "LoadMemory", m1ts );
			scheme.addCommand( 
					new Command(
							  "LW"
							, Arrays.asList( "z", "x", "c" )
							, null
							, "noexception"
							, m1
						) );
			scheme.addCommand( 
					new Command(
							  "ADD"
							, Arrays.asList( "z", "y", "x" )
							, null
							, "overflow"
							, null
						) );
			Map<String, Set<ProcedureTestSituation>> m2 = new HashMap<String, Set<ProcedureTestSituation>>();
			Set<ProcedureTestSituation> m2ts = new HashSet<ProcedureTestSituation>();
			m2ts.add( new CacheHit()
			{
				@Override
				public int getLevel() {
					return 2;
				}

				@Override
				public String getSetVar() {
					return null;
				}

				@Override
				public String getTagVar() {
					return null;
				}
			});
			m2ts.add( new CacheMiss()
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
			m2.put( "LoadMemory", m2ts );
			scheme.addCommand( 
					new Command(
							  "LW"
							, Arrays.asList( "y", "z", "c" )
							, null
							, "noexception"
							, m2
						) );

			// 2. вызвать решатель
			Solver solver = new Solver( 
					new File("C:\\Documents and Settings\\kornevgen\\My Documents\\diss\\ConstGen\\src\\TeSLa\\src.sample" )
					, new File("C:\\Documents and Settings\\kornevgen\\My Documents\\diss\\ConstGen\\src\\TeSLa\\clp")
				);
			List<Cache> cacheLevels = new ArrayList<Cache>();
			cacheLevels.add( new Cache()
			{
				@Override
				public int getSectionNumber()
				{
					return 3;
				}

				@Override
				public int getAddressBitLength() {
					return 32;
				}

				@Override
				public int getTagBitLength() {
					return 4;
				}
			} );
			cacheLevels.add( new Cache()
			{
				@Override
				public int getSectionNumber()
				{
					return 5;
				}

				@Override
				public int getAddressBitLength() {
					return 32;
				}

				@Override
				public int getTagBitLength() {
					return 6;
				}
			} );
			Solver.Verdict verdict = solver.solve(scheme, cacheLevels );
			
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