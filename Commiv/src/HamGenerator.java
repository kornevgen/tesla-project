import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;


public class HamGenerator {

	public HamGenerator(int vertexCount) {
		alreadyGeneratedCycles = new HashSet<List<Integer>>();
		this.vertexCount = vertexCount;
	}
	
	public static Random random = new Random(System.currentTimeMillis());
	
	public List<Integer> nextHamilton() {
		final List<Integer> result = new ArrayList<Integer>();

		final Set<Integer> domain = new HashSet<Integer>();
		
		for(int i = 0; i < vertexCount; i++) {
			domain.add(i);
		}
		
		// flag := false
		// loop start
		// choose a random element from domain
		// add it to cycle
		// calculate not used elements as the next elements of this element
		// if these elements are empty, then
		//		next element will be choosen randomly from domain
		// else 
		//		next element will be choosen randomly from (domain \inter these elements)
		//		set flag (we are generating a new cycle)
		// loop end
		// return null if not flag
		
		Set<Integer> for_choosing = domain;
		boolean newCycleIsGenerated = false;
		while (! domain.isEmpty()) {
			
			final int element = choose(for_choosing, vertexCount);
			
			result.add(element);
			domain.remove(element);
			
			final Set<Integer> possibleNexts = calculatePossibleNexts(element, domain);
			
			if (possibleNexts.isEmpty()) {
				for_choosing = domain;
			} else {
				for_choosing = possibleNexts;
				newCycleIsGenerated = true;
			}
			
		}
		
		if (! newCycleIsGenerated) {
			return null;
		}
		
//		for(int i = 0; i < vertexCount; i++) {
//			// choose random element of domain
//			int index = random.nextInt(domain.size());
//			int element = domain.get(index);
//			
//			// add it to result
//			result.add(element);
//			
//			// remove it from domain
//			domain.remove(index);
//		}
		
		result.add(result.get(0)); // it is a cycle
		return result;
	}
	
	private Set<Integer> calculatePossibleNexts(int element, Set<Integer> domain) {
		final Set<Integer> nexts = new HashSet<Integer>(domain);
		
		
		for(final List<Integer> cycle : alreadyGeneratedCycles) {
			for(int i = 0; i < cycle.size(); i++) {
				if (cycle.get(i) == element) {
					if (i > 0) nexts.remove(cycle.get(i-1));
					if (i < cycle.size() - 1) nexts.remove(cycle.get(i+1));
				}
			}
		}
		
		return nexts;
	}

	private int choose(Set<Integer> set, int max) {
		assert max > 0;
		// assert all elements e of the set : 0 <= e < max

		final List<Integer> all = new ArrayList<Integer>();
		for(int i = 0; i < max; i++) {
			all.add(i);
		}
		while (! all.isEmpty()) {
			int element = all.remove(random.nextInt(all.size()));
			if (set.contains(element)) {
				return element;
			}
		}
		throw new NoSuchElementException();
	}

	private final int vertexCount;
	private final Set<List<Integer>> alreadyGeneratedCycles;

}
