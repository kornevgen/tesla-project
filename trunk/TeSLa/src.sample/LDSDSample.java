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
import ru.teslaprj.scheme.ts.TLBSituation;

public class LDSDSample
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
		
		final int[][] tags2 = new int[(int)Math.pow(2, 14)][4];
		for( int i = 0; i < (int)Math.pow(2,14); i++ )
		{
			for( int j = 0; j < 4; j++ )
			{
				tags2[i][j] = r.nextInt( (int)Math.pow(2,17) );
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
			public long getTag(int section, int row) {
				return tags1[section][row];
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
		Cache cache2 = new Cache()
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
				return 17;
			}

			@Override
			public int getSetNumberBitLength() {
				return 14;
			}

			@Override
			public long getTag(int section, int row) {
				return tags2[section][row];
			}

			@Override
			public int getLevel() {
				return 2;
			}

			@Override
			public CACHETYPE getType() {
				return CACHETYPE.MIXED;
			}

			@Override
			public Set<Long> getValidTagsets() {
				throw new Error("getValidTagsets for L2");
			}
		};
//		cacheLevels.add( cache2 );
		
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
			List<List<String>> deps = new ArrayList<List<String>>();
			deps.add( Arrays.asList( "x", "y", "s", "t" ) );
			deps.add( Arrays.asList( "x", "y", "y", "t" ) );
			deps.add( Arrays.asList( "x", "y", "s", "y" ) );
			deps.add( Arrays.asList( "x", "y", "x", "t" ) );
			deps.add( Arrays.asList( "x", "y", "s", "x" ) );
			deps.add( Arrays.asList( "x", "y", "x", "x" ) );
			deps.add( Arrays.asList( "x", "x", "x", "t" ) );
			deps.add( Arrays.asList( "x", "y", "y", "y" ) );
			deps.add( Arrays.asList( "x", "x", "s", "x" ) );
			
			final int iterationsCount = 4*3*3*3*3;
			int iteratio = 0;
			int numberOfConsistent = 0;
			for( int cts1 = 0; cts1 < 3; cts1++ )
			for( int cts2 = 0; cts2 < 3; cts2++ )
			for( int at1 = 0; at1 < 2; at1++ )
			for( int at2 = 0; at2 < 2; at2++ )
			{
				for( List<String> dep : deps )
				{
					List<String> params1 = new ArrayList<String>( dep.subList(0, 2) );
					List<String> params2 = new ArrayList<String>( dep.subList(2, 4) );
					params1.add("cc");
					params2.add("ccc");
					
					try
					{
					Scheme scheme = new Scheme() ;
					scheme.addDefinition( new RegisterDefinition( "x", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "s", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "t", 64 ) );
					scheme.addDefinition( new ConstDefinition( "cc", 16 ) );
					scheme.addDefinition( new ConstDefinition( "ccc", 16 ) );

					Map<Cache, CacheTestSituation>[] cts = new HashMap[2];
					int idx = 0;
					for( int p : Arrays.asList(cts1, cts2))
					{
						cts[idx] = new HashMap<Cache, CacheTestSituation>();
						switch( p )
						{
						case 0: // l1Hit
							cts[idx].put( cache1, new CacheHit( cache1 ) );
							break;
						case 1: // l1Miss, l2Hit
							cts[idx].put( cache1, new CacheMiss( cache1 ));
							cts[idx].put( cache2, new CacheHit( cache2));
							break;
						case 2: // l1Miss, l2Miss
							cts[idx].put( cache1, new CacheMiss( cache1 ));
							cts[idx].put( cache2, new CacheMiss( cache2 ));
							break;
						default:
							continue;
						}
						idx++;
					}

					List<TLBSituation> ats = new ArrayList<TLBSituation>();
					for( int p : Arrays.asList( at1, at2 ) )
					{
						//TODO вообще-то еще бывает TLBRefill, AddressError, TLBModified, TLBInvalid
						switch( p )
						{
						case 0: //TLBHit
							ats.add( new TLBHit() );
							break;
						case 1: //TLBMiss
							ats.add( new TLBMiss(dtlb.size()) );
							break;
						default:
							continue;
						}
					}

					System.out.println( ++iteratio + " / " + iterationsCount + " :" );					
					
					MemoryCommand command1 = new MemoryCommand(
							scheme, 
							"LD", 
							params1, 
							"regular", 
							cts[0],
							ats.get(0),
							true
						);
					System.out.println(command1 + " " + cts[0].get(cache1).getClass().getSimpleName() + " "
							+ ats.get(0).getClass().getSimpleName());
					scheme.addCommand( command1 );
					MemoryCommand command2 = new MemoryCommand(
							scheme, 
							"SD", 
							params2, 
							"regular", 
							cts[1],
							ats.get(1),
							false
						);
					System.out.println(command2 + " " + cts[1].get(cache1).getClass().getSimpleName() + " "
							+ ats.get(1).getClass().getSimpleName());
					scheme.addCommand( command2 );

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
		}
			System.out.println("Consistent systems: " + numberOfConsistent + " ( " + (100*numberOfConsistent/iterationsCount) + "% )" );
	}
}