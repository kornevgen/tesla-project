import java.util.ArrayList;
import java.util.List;
import java.util.Random;


public class HamGenerator {
	
	public static Random random = new Random();
	
	public List<Integer> generateHamilton(int vertexCount) {
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
