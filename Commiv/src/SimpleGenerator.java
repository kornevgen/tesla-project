import java.io.IOException;
import java.util.List;


public class SimpleGenerator {

	public void generate(final int vertexCount, final String filePath) {

		final int price = 40;
		
		final Graph test = new Graph(vertexCount);
		
		List<Integer> hamcycle = new HamGenerator().generateHamilton(vertexCount);
		assert hamcycle.size() == vertexCount + 1;
		assert hamcycle.get(0) == hamcycle.get(vertexCount);
		
		for(int i = 0; i < vertexCount; i++) {
			test.addEdge(hamcycle.get(i), hamcycle.get(i+1), price);
		}
		
		try {
			test.printGraph(0, filePath);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
}
