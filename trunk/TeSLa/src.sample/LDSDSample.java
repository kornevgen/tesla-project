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
import ru.teslaprj.Verdict;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ConstDefinition;
import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.RegisterDefinition;
import ru.teslaprj.scheme.Scheme;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;
import ru.teslaprj.scheme.ts.TLBHit;
import ru.teslaprj.scheme.ts.TLBMiss;

public class LDSDSample
{
	public static void main( String[] args )
	{
		List<Cache> cacheLevels = new ArrayList<Cache>();
		Random r = new Random();
		final int[][] tags1 = new int[(int)Math.pow(2, 7)][4];
		for( int i = 0; i < (int)Math.pow(2,7); i++ )
		{
			for( int j = 0; j < 4; j++ )
			{
				tags1[i][j] = r.nextInt( (int)Math.pow(2,24) );
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
		// TODO initialize cache
		cacheLevels.add( new Cache()
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
		} );
		cacheLevels.add( new Cache()
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
		} );

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
			
			for( int ts1 = 0; ts1 < 3; ts1++ )
			for( int ts2 = 0; ts2 < 3; ts2++ )
			for( int at1 = 0; at1 < 2; at1++ )
			for( int at2 = 0; at2 < 2; at2++ )
			{
				for( List<String> dep : deps )
				{
					List<String> params1 = new ArrayList<String>( dep.subList(0, 2) );
					List<String> params2 = new ArrayList<String>( dep.subList(2, 4) );
					params1.add("c1");
					params2.add("c2");
					
					try
					{
					Scheme scheme = new Scheme() ;
					scheme.addDefinition( new RegisterDefinition( "x", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "y", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "s", 64 ) );
					scheme.addDefinition( new RegisterDefinition( "t", 64 ) );
					scheme.addDefinition( new ConstDefinition( "c1", 16 ) );
					scheme.addDefinition( new ConstDefinition( "c2", 16 ) );

					List<Set<ProcedureTestSituation>> m1ts = new ArrayList<Set<ProcedureTestSituation>>();
					for( int p : Arrays.asList(ts1, ts2))
					{
						Set<ProcedureTestSituation> ts = new HashSet<ProcedureTestSituation>();
						switch( p )
						{
						case 0: // l1Hit
							ts.add( new CacheHit() {
								@Override
								public String getTagVar() {
									return null;
								}
							
								@Override
								public String getSetVar() {
									return null;
								}
							
								@Override
								public int getLevel() {
									return 1;
								}
							});
							break;
						case 1: // l1Miss, l2Hit
							ts.add( new CacheMiss() {
							
								@Override
								public String getTagVar() {
									return null;
								}
							
								@Override
								public String getSetVar() {
									return null;
								}
							
								@Override
								public int getLevel() {
									return 1;
								}
							
								@Override
								public String getVTagVar() {
									return null;
								}
							});
							ts.add( new CacheHit() {
							
								@Override
								public String getTagVar() {
									return null;
								}
							
								@Override
								public String getSetVar() {
									return null;
								}
							
								@Override
								public int getLevel() {
									return 2;
								}
							});
							break;
						case 2: // l1Miss, l2Miss
							ts.add( new CacheMiss() {
								
								@Override
								public String getTagVar() {
									return null;
								}
							
								@Override
								public String getSetVar() {
									return null;
								}
							
								@Override
								public int getLevel() {
									return 1;
								}
							
								@Override
								public String getVTagVar() {
									return null;
								}
							});
							ts.add( new CacheMiss() {
							
								@Override
								public String getTagVar() {
									return null;
								}
							
								@Override
								public String getSetVar() {
									return null;
								}
							
								@Override
								public int getLevel() {
									return 2;
								}

								@Override
								public String getVTagVar() {
									return null;
								}
							});
							break;
						default:
							continue;
						}
						
						m1ts.add( ts );
					}

					List<Set<ProcedureTestSituation>> m11ts = new ArrayList<Set<ProcedureTestSituation>>();
					for( int p : Arrays.asList( at1, at2 ) )
					{
						Set<ProcedureTestSituation> ts = new HashSet<ProcedureTestSituation>();
						
						//TODO вообще-то еще бывает TLBRefill, AddressError, TLBModified, TLBInvalid
						switch( p )
						{
						case 0: //TLBHit
							ts.add( new TLBHit() {
								@Override
								public int getmoDify() {
									return 1;
								}
							
								@Override
								public int getValid() {
									return 1;
								}
							
								@Override
								public Integer getG() {
									return null;
								}
							
								@Override
								public String getVirtualAddressVar() {
									return null;
								}
							
								@Override
								public String getPhysicalAddressVar() {
									return null;
								}
							});
							break;
						case 1: //TLBMiss
							ts.add( new TLBMiss() {
							
								@Override
								public int getmoDify() {
									return 1;
								}
							
								@Override
								public int getValid() {
									return 1;
								}
							
								@Override
								public Integer getG() {
									return null;
								}
							
								@Override
								public String getVirtualAddressVar() {
									return null;
								}
							
								@Override
								public String getPhysicalAddressVar() {
									return null;
								}
							});
							break;
						default:
							continue;
						}
						
						m11ts.add( ts );
					}
					
					Map<String, Set<ProcedureTestSituation>> m1 = new HashMap<String, Set<ProcedureTestSituation>>();
					m1.put( "LoadMemory", m1ts.get(0) );
					m1.put( "AddressTranslation", m11ts.get(0));

					Map<String, Set<ProcedureTestSituation>> m2 = new HashMap<String, Set<ProcedureTestSituation>>();
					m2.put( "StoreMemory", m1ts.get(1) );
					m2.put( "AddressTranslation", m11ts.get(1));

					scheme.addCommand( new Command("LD", params1, "regular", m1));
					//scheme.addCommand( new Command("SD", params2, "regular", m2));

					//TODO TLB build
					TLB tlb = new TLB(){

						@Override
						public int getBufferSize() {
							return 4;
						}

						@Override
						public int getJTLBSize() {
							return 48;
						}

						@Override
						public int getMaximumOfMask() {
							return 2;
						}

						@Override
						public int getRangeEndBit() {
							return 39;
						}

						@Override
						public int getRangeStartBit() {
							return 38;
						}

						@Override
						public int getVPNd2EndBit() {
							return 37;
						}

						@Override
						public int getVPNd2StartBit() {
							return 12;
						}

						@Override
						public int getVirtualAddressBitLen() {
							return 40;
						}

						@Override
						public int getPFNBitLen() {
							return 24;
						}

						@Override
						public int getPhysicalAddressBitLen() {
							return 36;
						}

						@Override
						public int getASIDBitLen() {
							return 8;
						}};

					
					//TODO cache build
					
					//TODO memory build
					
					//TODO solve!
					Solver solver = new Solver( new File("src.sample" ), new File("clp") );
					Verdict verdict = solver.solve(scheme, cacheLevels, tlb );
					
					//TODO check verdict and change cache

					//TODO print answer, check answer
					// 3. распечатать ответ
					Map<Definition, BigInteger> values = verdict.getDefinitionValues();
					for( Definition def : values.keySet() )
					{
						System.out.println( def + " = " + values.get( def ) );	
					}

//					List<Map<Long, List<Long>>> caches = verdict.getCacheInitialization();
//					for( Cache cache : cacheLevels )
//					{
//						int level = cacheLevels.indexOf( cache );
//						Map<Long, List<Long>> sets = caches.get( level );
//						for( long setNumber : sets.keySet() )
//						{
//							System.out.print( 
//									"level " + ( level + 1 ) + 
//									": set " + setNumber + ": _ " );
//							for( Long tag : sets.get( setNumber ) )
//							{
//								System.out.print( ", " + tag );
//							}
//							System.out.println();
//						}
//					}
					
					Map<Integer, TLBRow> tlbrows = verdict.getTlbrows();
					for( TLBRow row : tlbrows.values() )
					{
						System.out.println( "tlb:" +
								" range = " + row.getRange() +
								", vpn/2 = " + row.getVPNd2() +
								", mask = " + row.getMask() + 
								", g = " + row.getGlobal() + 
								", asid = " + row.getAsid() + 
								", pfn0 = " + row.getPFN0() + 
								", v0 = " + row.getValid0() +
								", d0 = " + row.getmoDify0() +
								", pfn1 = " + row.getPFN1() +
								", v1 = " + row.getValid1() +
								", d1 = " + row.getmoDify1()
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
						return;
					}
				}
			}			
	}
}