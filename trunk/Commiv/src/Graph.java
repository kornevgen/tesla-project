import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class Graph {
	
	public Graph(int vertexCount) {
		this.vertexCount = vertexCount;
		this.edgesCount = 0;
		this.edges = new HashMap<>();
		this.prices = new HashMap<>();
	}
	
	public void addEdge(int from, int to, int price) {
		assert 0 <= price;
		assert price <= 10000;
		assert 0 <= from;
		assert from < vertexCount;
		assert 0 <= to;
		assert to < vertexCount;
		
		if (edges.containsKey(from)) {
			if (edges.get(from).contains(to)) {
				// nothing
			} else {
				edges.get(from).add(to);
				prices.put(new Edge(from, to), price);
				edgesCount ++;
			}
		} else {
			edges.put(from, new HashSet<>(Arrays.asList(to))) ;
			prices.put(new Edge(from, to), price);
			edgesCount ++;
		}
	}
	
	public void printGraph(int start, final String filePath) throws IOException {
		assert 0 <= start;
		assert start < vertexCount;
		assert filePath != null;
		
		final PrintWriter writer = new PrintWriter(new FileWriter(filePath));
		
		writer.print(vertexCount);
		writer.print(" ");
		writer.print(edgesCount);
		writer.print(" ");
		writer.println(start);

		for(final Edge edge : prices.keySet()) {
			writer.print(edge.from);
			writer.print(" ");
			writer.print(edge.to);
			writer.print(" ");
			writer.println(prices.get(edge));
		}
		
		writer.close();
	}

	class Edge {
		public Edge(int from, int to) {
			this.from = from;
			this.to = to;
		}
		final int from;
		final int to;
	}
	
	//@ inv 0 <= edgesCount <= vertexCount * vertexCount;
	private final int vertexCount;
	private final Map<Integer, Set<Integer>> edges;
	private final Map<Edge, Integer> prices;
	private 	  int edgesCount;
}
