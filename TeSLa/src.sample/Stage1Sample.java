import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.Solver;
import ru.teslaprj.TLB;
import ru.teslaprj.TLBRow;
import ru.teslaprj.scheme.ConstDefinition;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.RegisterDefinition;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.CacheTestSituation;
import ru.teslaprj.scheme.ts.TLBHit;
import ru.teslaprj.scheme.ts.TLBMiss;

public class Stage1Sample
{
	private static final int PFN_BITLEN = 24;
	private static final int SEGBITS = 40;
	private static final int PABITS = 36;

	public static void main( String[] args )
	{
		Solver solver = new Solver( new File("src.sample" ), new File("clp") );
		
		List<Cache> cacheLevels = new ArrayList<Cache>();
		final Set<Long> tagsets1 = new HashSet<Long>();
		Set<Integer> ttt = new HashSet<Integer>();
		Random r = new Random();
		final int[][] tags1 = new int[(int)Math.pow(2, 7)][4];
		for( int i = 0; i < (int)Math.pow(2,7); i++ )
		{
			for( int j = 0; j < 4; j++ )
			{
				tags1[i][j] = r.nextInt( (int)Math.pow(2,24) ); // <<i>> is set value for this tag
				tagsets1.add( tags1[i][j] * (long)Math.pow(2,7) + (long)i );
				ttt.add(tags1[i][j]);
			}
		}
		
		Cache cache1 = new Cache()
		{
			@Override
			public int getSectionNumber()
			{
				return 4;
			}

			@Override
			public int getAddressBitLength() {
				return 36;
			}

			@Override
			public int getTagBitLength() {
				return 24;
			}

			@Override
			public int getSetNumberBitLength() {
				return 7;
			}

			@Override
			public long getTag(int section, int set) {
				return tags1[set][section];
			}

			@Override
			public int getLevel() {
				return 1;
			}

			@Override
			public Cache.CACHETYPE getType() {
				return CACHETYPE.DATA;
			}

			@Override
			public Set<Long> getValidTagsets() {
				return tagsets1;
			}
		};
		cacheLevels.add( cache1 );
		
		// initialize TLB
		final int[] pfn0 = new int[48];
		final int[] pfn1 = new int[48];
		final int[] ranges = new int[48];
		for( int i = 0; i < 48; i++ )
		{
			pfn0[i] = r.nextInt( (int)Math.pow(2, PFN_BITLEN) );
			pfn1[i] = r.nextInt( (int)Math.pow(2, PFN_BITLEN) );
			if ( ttt.contains(pfn0[i]) )
				System.out.println(pfn0[i]);
			if ( ttt.contains(pfn1[i]) )
				System.out.println(pfn1[i]);
			ranges[i] = r.nextInt( 4 );
		}
		
		final int[] masks = new int[48];
		final long[] vpnd2s = new long[48]; 
		// генерируем так, чтобы не нарушилась консистентность TLB
		for( int range = 0; range < 4; range++ )
		{
			// индексы строк TLB, у которых поле r равно range
			List<Integer> indexes = new ArrayList<Integer>();
			for( int i = 0; i < 48; i++  )
			{
				if ( ranges[i] == range )
					indexes.add(i);
			}
			List<Long> vs = new ArrayList<Long>();
			for( int i = 0; i < indexes.size(); i++ )
			{
				// выбираем маску
				masks[indexes.get(i)] = PFN_BITLEN; //r.nextInt(9); //maxmask = 8
				
				// выбираем vpn/2
				long v;
				do
				{
					v = r.nextInt((int)Math.pow(2, SEGBITS - (PABITS-PFN_BITLEN)));//segbits - 1 - (pabits-pfnlength)
				} while( vs.contains(v) || v / (int)Math.pow(2, 20) != 63  );
				
				vs.add(v);
				vpnd2s[indexes.get(i)] = v;
			}
		}
		
		final TLBRow[] tlbRows = new TLBRow[48];
		for( int i = 0; i < 48; i++ )
		{
			final BigInteger p0 = new BigInteger( pfn0[i] + "" );
			final BigInteger p1 = new BigInteger( pfn1[i] + "" );
			final int range = ranges[i];
			final int mask = masks[i];
			final BigInteger v = new BigInteger( vpnd2s[i] + "" );
			
			tlbRows[i] = new TLBRow(){
				@Override
				public Integer getAsid() {
					return 0;
				}

				@Override
				public Integer getGlobal() {
					return 0;
				}

				@Override
				public Integer getMask() {
					return mask;
				}

				@Override
				public BigInteger getPFN0() {
					return p0;
				}

				@Override
				public BigInteger getPFN1() {
					return p1;
				}

				@Override
				public Integer getRange() {
					return range;
				}

				@Override
				public BigInteger getVPNd2() {
					return v;
				}

				@Override
				public int getValid0() {
					return 1;
				}

				@Override
				public int getValid1() {
					return 1;
				}

				@Override
				public int getmoDify0() {
					return 1;
				}

				@Override
				public int getmoDify1() {
					return 1;
				}};
		}
		
		final List<Integer> dtlb = Arrays.asList( 1, 2, 3, 0 );
		final List<Integer> itlb = Arrays.asList( 11, 21, 31, 10 );

		
			// 1. сформировать схему
			// перебор зависимостей:
			/**
			 * (x,y) - (s,t)
			 * (x,y) - (y,t) -- появляется ли тут косвенная адресация?
			 * (x,y) - (s,y)
			 * (x,y) - (x,t)
			 * (x,y) - (s,x)
			 * (x,y) - (x,x)
			 * (x,x) - (x,t)
			 * (x,y) - (y,y)
			 * (x,x) - (s,x)
			 */
			final int iterationsCount = 4*3*3*3*3;
			int iteratio = 0;
			int numberOfConsistent = 0;
//			for( int cts1 = 0; cts1 < 2; cts1++ )
//			for( int cts2 = 0; cts2 < 2; cts2++ )
//			for( int at1 = 0; at1 < 2; at1++ )
//			for( int at2 = 0; at2 < 2; at2++ )
			{
				try
				{
					Scheme scheme = new Scheme() ;
					scheme.addDefinition( new RegisterDefinition( "x", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
					scheme.addDefinition( new ConstDefinition( "cc", 16 ) );
					scheme.addDefinition( new ConstDefinition( "ccc", 16 ) );
					scheme.addDefinition( new ConstDefinition( "cccc", 16 ) );
					scheme.addDefinition( new ConstDefinition( "ccccc", 16 ) );
					scheme.addDefinition( new ConstDefinition( "cccccc", 16 ) );

					System.out.println( ++iteratio + " / " + iterationsCount + " :" );
					
					Map<Cache, CacheTestSituation> c1 = new HashMap<Cache, CacheTestSituation>();
					c1.put(cache1, new CacheHit(cache1));
					Map<Cache, CacheTestSituation> c2 = new HashMap<Cache, CacheTestSituation>();
					c2.put(cache1, new CacheHit(cache1));
					Map<Cache, CacheTestSituation> c3 = new HashMap<Cache, CacheTestSituation>();
					c3.put(cache1, new CacheHit(cache1));
					Map<Cache, CacheTestSituation> c4 = new HashMap<Cache, CacheTestSituation>();
					c4.put(cache1, new CacheMiss(cache1));
					Map<Cache, CacheTestSituation> c5 = new HashMap<Cache, CacheTestSituation>();
					c5.put(cache1, new CacheMiss(cache1));
					
					MemoryCommand command1 = new MemoryCommand(
							scheme, 
							"LD", 
							Arrays.asList("x", "y", "cc"), 
							"regular", 
							c1,
							new TLBMiss(dtlb.size()),
							true
						);
					command1.setValue("x0");
					scheme.addCommand( command1 );
					
					MemoryCommand command2 = new MemoryCommand(
							scheme, 
							"LD", 
							Arrays.asList("x", "y", "ccc"), 
							"regular", 
							c2,
							new TLBMiss(dtlb.size()),
							true
						);
					command2.setValue("x1");
					scheme.addCommand( command2 );
					
					MemoryCommand command3 = new MemoryCommand(
							scheme,
							"LD",
							Arrays.asList("x", "y", "cccc"),
							"regular",
							c3,
							new TLBHit(),
							true
						);
					command3.setValue("x2");
					scheme.addCommand( command3 );
					
					MemoryCommand command4 = new MemoryCommand(
							scheme,
							"LD",
							Arrays.asList("x", "y", "ccccc"),
							"regular",
							c4,
							new TLBMiss(dtlb.size()),
							true
						);
					command4.setValue("x3");
					scheme.addCommand( command4 );
					
					MemoryCommand command5 = new MemoryCommand(
							scheme,
							"LD",
							Arrays.asList("x", "y", "cccccc"),
							"regular",
							c5,
							new TLBHit(),
							true
						);
					command5.setValue("x4");
					scheme.addCommand(command5);
					
					//TLB build
					//TODO представленное тут вычисление границ vpn/2 работает только в Mapped сегменте!
					TLB tlb = new TLB(){

						@Override
						public int getDTLBSize() {
							return 4;
						}

						@Override
						public int getJTLBSize() {
							return 48;
						}

						@Override
						public int getMaximumOfMask() {
							return 8;
						}

						@Override
						public int getRangeEndBit() {
							return 63;
						}

						@Override
						public int getRangeStartBit() {
							return 62;
						}

						@Override
						public int getVPNd2EndBit() {
							return getSEGBITS() - 1;
						}

						@Override
						public int getVPNd2StartBit() {
							return 13;
						}

						@Override
						public int getSEGBITS() {
							return SEGBITS;
						}

						@Override
						public int getPFNBitLen() {
							return PFN_BITLEN;
						}

						@Override
						public int getPABITS() {
							return PABITS;
						}

						@Override
						public int getASIDBitLen() {
							return 8;
						}

						@Override
						public List<Integer> getDTLB() {
							return dtlb;
						}

						@Override
						public List<Integer> getITLB() {
							return itlb;
						}

						@Override
						public TLBRow getRow(int index) {
							return tlbRows[index];
						}

						};

					if ( 
						solver.solve(scheme, cacheLevels, tlb )
					)
						numberOfConsistent++;
					
					//TODO check verdict and change cache and TLB

					//TODO print answer, check answer
					// 3. распечатать ответ
//					System.out.println( ++iteratio + " / " + iterationsCount + " :" );
//					
//					Map<Definition, BigInteger> values = verdict.getDefinitionValues();
//					for( Definition def : values.keySet() )
//					{
//						System.out.println( def + " = " + values.get( def ) );	
//					}
//
////					List<Map<Long, List<Long>>> caches = verdict.getCacheInitialization();
////					for( Cache cache : cacheLevels )
////					{
////						int level = cacheLevels.indexOf( cache );
////						Map<Long, List<Long>> sets = caches.get( level );
////						for( long setNumber : sets.keySet() )
////						{
////							System.out.print( 
////									"level " + ( level + 1 ) + 
////									": set " + setNumber + ": _ " );
////							for( Long tag : sets.get( setNumber ) )
////							{
////								System.out.print( ", " + tag );
////							}
////							System.out.println();
////						}
////					}
//					
//					Map<Integer, TLBRow> tlbrows = verdict.getTlbrows();
//					for( TLBRow row : tlbrows.values() )
//					{
//						System.out.println( "tlb:" +
//								" range = " + row.getRange() +
//								", vpn/2 = " + row.getVPNd2() +
//								", mask = " + row.getMask() + 
//								", g = " + row.getGlobal() + 
//								", asid = " + row.getAsid() + 
//								", pfn0 = " + row.getPFN0() + 
//								", v0 = " + row.getValid0() +
//								", d0 = " + row.getmoDify0() +
//								", pfn1 = " + row.getPFN1() +
//								", v1 = " + row.getValid1() +
//								", d1 = " + row.getmoDify1()
//							);
//					}
//					
//					Map<BigInteger, BigInteger> memory = verdict.getMemory();
//					for( BigInteger address : memory.keySet() )
//					{
//						System.out.println( "memory[ " + address + " ] = " + memory.get(address) );
//					}
					System.out.println();
					System.out.println();					
				}
				catch( Exception e )
				{
					throw new Error(e);
				}
		}
			System.out.println("Consistent systems: " + numberOfConsistent + " ( " + (100*numberOfConsistent/iterationsCount) + "% )" );
	}
}