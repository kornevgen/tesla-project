import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;


public class App {
	
	public static void main(String[] args) {
		
		final int vertexCount = 10;
		final int price = 5;
		final Graph test = new Graph(vertexCount);
		
		List<Integer> hamcycle = generateHamilton(vertexCount);
		assert hamcycle.size() == vertexCount + 1;
		assert hamcycle.get(0) == hamcycle.get(vertexCount);
		
		for(int i = 0; i < vertexCount; i++) {
			test.addEdge(hamcycle.get(i), hamcycle.get(i+1), price);
		}
		
		try {
			test.printGraph(0, "001.dat");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static Random random = new Random();
	
	private static List<Integer> generateHamilton(int vertexCount) {
		final List<Integer> result = new ArrayList<>();

		final List<Integer> domain = new ArrayList<>();
		
		for(int i = 0; i < vertexCount; i++) {
			domain.add(i);
		}
		
		for(int i = 0; i < vertexCount; i++) {
			// choose random element of domain
			int index = random.nextInt(domain.size());
			int element = domain.get(index);
			
			// add it to result
			result.add(element);
			
			// remove it from domain
			domain.remove(index);
		}
		
		result.add(result.get(0)); // it is a cycle
		return result;
	}
}
