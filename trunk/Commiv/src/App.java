import java.io.IOException;
import java.util.List;
import java.util.Random;


public class App {
	
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
		
		final int simpleGraphs = 1;
		final int randomGraphs = 1;

		int testNumber = 3;
		final FileNameGenerator f = new FileNameGenerator(".dat");

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

		
		for(final int vC : vertexesCounts) {
			if (vC == 200) {
				HamGenerator.random = new Random(System.currentTimeMillis());
			}
			for(final double fn : fullness) {
				for(final int cC : cyclesCount) {
					
					if (vC == 3 && cC >= 3) {
						continue;
					}
					
					final Graph graph = new Graph(vC);
					final HamGenerator generator = new HamGenerator(vC);
					
					final List<Integer> cycle1 = generator.nextHamilton();
					final int price = 50;
					
					final PriceGenerator priceGenerator =
							(HamGenerator.random.nextInt(10) > 7) 
								//? new ConstantPriceGenerator(price)
								//: (HamGenerator.random.nextInt(10) > 5)
								? new RandomPriceGenerator(price) 
								: new SuperRandomPriceGenerator(price);
							
					graph.addCycle(cycle1, priceGenerator);
					
					for(int i = 0; i < cC - 1; i++) {
						final List<Integer> cycle = generator.nextHamilton();
						if (cycle == null) {
							System.err.println("Couldn't generate cycle for vC = " + vC + " and cC = " + cC);
							break;
						} else {
							graph.addCycle(cycle, priceGenerator);
						}
					}
					
					while(graph.getEdgesCount() < fn * vC * vC) {
						graph.addEdge(
								HamGenerator.random.nextInt(vC),
								HamGenerator.random.nextInt(vC),
								priceGenerator.nextPrice());
					}

					try {
						graph.printGraph(cycle1.get(0), f.get(testNumber++));
						
						System.out.println("Generated: vC = " + vC + " and cC = " + cC + ": all " + testNumber + " tests");
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
		
	}
	
	public static void main2(String[] args) {
		
		final int vertexCount = 10;
		final int price = 5;
		
		final int simpleGraphs = 1;
		final int randomGraphs = 2;
		final int fullGraphs = 1;
		
		final int[] vertexesCounts = {
				3, 5, 7, 9, 10, 20, 30, 50, 100, 200, 300, 500, 1000
		};
		
		// разная разряженность
		
		// разная связность (близкая к полной / сильная / средняя)

		// количество циклов разной стоимости
		
		int testNumber = 1;
		final FileNameGenerator f =
				new FileNameGenerator(".dat");
		
		for(int i = 0; i < simpleGraphs; i++) {
			for(int v : vertexesCounts) {
				new SimpleGenerator().generate(v, f.get(testNumber++));
			}
		}
		
		for(int i = 0; i < randomGraphs; i++) {
			for(int v : vertexesCounts) {
				new RandomGenerator().generate(v, f.get(testNumber++));
			}
		}
		
		for(int i = 0; i < fullGraphs; i++) {
			for(int v : vertexesCounts) {
				new FullGraphsGenerator().generate(v, f.get(testNumber++));
			}
		}
		
	}

}
