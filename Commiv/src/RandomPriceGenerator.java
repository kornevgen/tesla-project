
public class RandomPriceGenerator extends PriceGenerator {

	private final int maxprice;
	
	public RandomPriceGenerator(int maxprice) {
		this.maxprice = maxprice;
	}
	
	public int nextPrice() {
		return HamGenerator.random.nextInt(maxprice);
	}
}
