import java.util.Arrays;
import java.util.BitSet;
import java.util.HashSet;
import java.util.Set;




public class App {

	private static int K = 3;
	
	private boolean examine(final BitSet bitset) {
		return
				( ! bitset.get(0) || bitset.get(1) || !bitset.get(2) )
				&&
				( ! bitset.get(2) || ! bitset.get(1) ) ;			
	}
	
	
	
	public static void main(String[] args) {
		for(BitSet bitset : new App().generate()) {
			System.out.println( bitset.toString() );
		}
	}

	public static BitSet min1 = new BitSet();
	public static BitSet min2 = new BitSet();
	public static BitSet min3 = new BitSet();
	public static BitSet min4 = new BitSet();
	static {
		min1.set(0, false); min1.set(1, false); min1.set(2, false);
		min2.set(0, true); min2.set(1, true); min2.set(2, false);
		min3.set(0, true); min3.set(0, false); min3.set(2, true);
		min4.set(0, false); min4.set(1, true); min4.set(2, true);
	}
	
	public static BitSet[] minimal = new BitSet[] {
		min1, min2, min3, min4
	};

	public Set<BitSet> generate() {
		Set<Integer> invalids = new HashSet<Integer>();
		Set<BitSet> current = new HashSet<BitSet>(Arrays.asList(minimal));
		while (true) {
			
			// get conflicts from current
			// conflicts = known + unknown
			// remove known from current
			// stop, if unknown is empty
			// add unknown to invalids
			// remove unknown from current
			// insert replaced unknown with "non-conflicts"\conflicts
			// stop, if nothing inserted
			
		}
		return current;
	}
	
}
