import java.io.IOException;
import java.util.List;


public class FullGraphsGenerator {

	public void generate(final int vertexCount, final String filePath) {
		
		// generate hamiltonian
		final List<Integer> hamcycle = new HamGenerator().generateHamilton(vertexCount);

		final Graph result = new Graph(vertexCount);
		final int price = 50; 
		for(int i = 0; i < vertexCount; i++) {
			result.addEdge(hamcycle.get(i), hamcycle.get(i+1), price);
		}
		while (result.getEdgesCount() < 0.99 * vertexCount * vertexCount) {
			result.addEdge(HamGenerator.random.nextInt(vertexCount),
					HamGenerator.random.nextInt(vertexCount), price);
		}
		
		try {
			result.printGraph(hamcycle.get(0), filePath);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
