import java.io.IOException;
import java.util.List;
import java.util.Random;


public class App {
	
	static class TestData {
		public int vertexCount = 20;
		public double fullness = 0.5;
		public int cyclesCount = 3;
		public PriceGenerator priceGenerator = new RandomPriceGenerator(50);
	}
	
	public static void main(final String[] args) {

		final int[] vertexesCounts = {
				3, 20, 200, 998
		};

		final double[] fullness = {
				0.1, 0.5, 0.97
		};
		
		final int[] cyclesCount = {
				1, 2, 5, 10
		};
		
//		final int simpleGraphs = 1;
//		final int randomGraphs = 1;

		Integer testNumber = 3;
		final FileNameGenerator f = new FileNameGenerator(".dat");
		
		final TestData test1 = new TestData();
		test1.vertexCount = 20;
		test1.fullness = 0.97;
		test1.cyclesCount = 5;
		test1.priceGenerator = new RandomPriceGenerator(50);

		final TestData test2 = new TestData();
		test2.vertexCount = 200;
		test2.fullness = 0.5;
		test2.cyclesCount = 10;
		test2.priceGenerator = new SuperRandomPriceGenerator(1000);
		
		final TestData test3 = new TestData();
		test3.vertexCount = 200;
		test3.fullness = 0.97;
		test3.cyclesCount = 50;
		test3.priceGenerator = new RandomPriceGenerator(500);
		
		final TestData test4 = new TestData();
		test4.vertexCount = 998;
		test4.fullness = 0.5;
		test4.cyclesCount = 30;
		test4.priceGenerator = new SuperRandomPriceGenerator(600);

		final TestData test5 = new TestData();
		test5.vertexCount = 100;
		test5.fullness = 0.97;
		test5.cyclesCount = 15;
		test5.priceGenerator = new RandomPriceGenerator(50);

//		generate_test(test1, f, 3);
//		generate_test(test2, f, 4);
//		generate_test(test3, f, 5);
		generate_test(test4, f, 3);
		generate_test(test5, f, 4);
		
		
		return;
		

//		for(int i = 0; i < simpleGraphs; i++) {
//			for(int v : vertexesCounts) {
//				new SimpleGenerator().generate(v, f.get(testNumber++));
//			}
//		}
//		
//		for(int i = 0; i < randomGraphs; i++) {
//			for(int v : vertexesCounts) {
//				new RandomGenerator().generate(v, f.get(testNumber++));
//			}
//		}


		
		
//		boolean do_generation = false;
//		
//		for(final int vC : vertexesCounts) {
//			if (vC == 200) {
//				HamGenerator.random = new Random(System.currentTimeMillis());
//			}
//			for(final double fn : fullness) {
//				for(final int cC : cyclesCount) {
//					
//					do_generation = ! do_generation;
//					if (! do_generation) {
//						continue;
//					}
//					
//					if (vC == 3 && cC >= 3) {
//						continue;
//					}
//					
//					final Graph graph = new Graph(vC);
//					final HamGenerator generator = new HamGenerator(vC);
//					
//					final List<Integer> cycle1 = generator.nextHamilton();
//					final int price = 50;
//					
//					final PriceGenerator priceGenerator =
//							(HamGenerator.random.nextInt(10) > 7) 
//								//? new ConstantPriceGenerator(price)
//								//: (HamGenerator.random.nextInt(10) > 5)
//								? new RandomPriceGenerator(price) 
//								: new SuperRandomPriceGenerator(price);
//							
//					graph.addCycle(cycle1, priceGenerator);
//					
//					for(int i = 0; i < cC - 1; i++) {
//						final List<Integer> cycle = generator.nextHamilton();
//						if (cycle == null) {
//							System.err.println("Couldn't generate cycle for vC = " + vC + " and cC = " + cC);
//							break;
//						} else {
//							graph.addCycle(cycle, priceGenerator);
//						}
//					}
//					
//					while(graph.getEdgesCount() < fn * vC * vC) {
//						graph.addEdge(
//								HamGenerator.random.nextInt(vC),
//								HamGenerator.random.nextInt(vC),
//								priceGenerator.nextPrice());
//					}
//
//					try {
//						graph.printGraph(cycle1.get(0), f.get(testNumber++));
//						
//						System.out.println("Generated: vC = " + vC + " and cC = " + cC + ": all " + testNumber + " tests");
//					} catch (IOException e) {
//						e.printStackTrace();
//					}
//				}
//			}
//		}
		
	}

	private static void generate_test(final TestData test,
			final FileNameGenerator f,
			Integer testNumber) {
		
		final Graph graph = new Graph(test.vertexCount);
		final HamGenerator generator = new HamGenerator(test.vertexCount);
		
		final List<Integer> cycle1 = generator.nextHamilton();
//		final int price = 50;
//		
//		final PriceGenerator priceGenerator =
//				(HamGenerator.random.nextInt(10) > 7) 
//					//? new ConstantPriceGenerator(price)
//					//: (HamGenerator.random.nextInt(10) > 5)
//					? new RandomPriceGenerator(price) 
//					: new SuperRandomPriceGenerator(price);
				
		graph.addCycle(cycle1, test.priceGenerator);
		
		for(int i = 0; i < test.cyclesCount - 1; i++) {
			final List<Integer> cycle = generator.nextHamilton();
			if (cycle == null) {
				System.err.println("Couldn't generate cycle for vC = " + test.vertexCount + " and cC = " + test.cyclesCount);
				break;
			} else {
				graph.addCycle(cycle, test.priceGenerator);
			}
		}
		
		while(graph.getEdgesCount() < test.fullness * test.vertexCount * test.vertexCount) {
			graph.addEdge(
					HamGenerator.random.nextInt(test.vertexCount),
					HamGenerator.random.nextInt(test.vertexCount),
					test.priceGenerator.nextPrice());
		}

		try {
			graph.printGraph(cycle1.get(0), f.get(testNumber++));
			
			System.out.println("Generated: vC = " + test.vertexCount
					+ " and cC = " + test.cyclesCount + ": all " + testNumber + " tests");
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
//	public static void main2(String[] args) {
//		
//		final int vertexCount = 10;
//		final int price = 5;
//		
//		final int simpleGraphs = 1;
//		final int randomGraphs = 2;
//		final int fullGraphs = 1;
//		
//		final int[] vertexesCounts = {
//				3, 5, 7, 9, 10, 20, 30, 50, 100, 200, 300, 500, 1000
//		};
//		
//		// ������ �������������
//		
//		// ������ ��������� (������� � ������ / ������� / �������)
//
//		// ���������� ������ ������ ���������
//		
//		int testNumber = 1;
//		final FileNameGenerator f =
//				new FileNameGenerator(".dat");
//		
//		for(int i = 0; i < simpleGraphs; i++) {
//			for(int v : vertexesCounts) {
//				new SimpleGenerator().generate(v, f.get(testNumber++));
//			}
//		}
//		
//		for(int i = 0; i < randomGraphs; i++) {
//			for(int v : vertexesCounts) {
//				new RandomGenerator().generate(v, f.get(testNumber++));
//			}
//		}
//		
//		for(int i = 0; i < fullGraphs; i++) {
//			for(int v : vertexesCounts) {
//				new FullGraphsGenerator().generate(v, f.get(testNumber++));
//			}
//		}
//		
//	}

}
