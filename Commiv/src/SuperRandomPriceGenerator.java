
public class SuperRandomPriceGenerator extends PriceGenerator {

	private final int maxprice;
	public SuperRandomPriceGenerator(int maxprice) {
		this.maxprice = maxprice;
	}
	
	@Override
	public int nextPrice() {
		int i1 = HamGenerator.random.nextInt(maxprice);
		if (i1 > maxprice / 2) {
			return maxprice / 2 + HamGenerator.random.nextInt(maxprice / 2);
		} else {
			return HamGenerator.random.nextInt(maxprice / 2);
		}
	}

}
