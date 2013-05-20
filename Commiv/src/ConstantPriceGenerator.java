
public class ConstantPriceGenerator extends PriceGenerator {

	private int price;
	
	public ConstantPriceGenerator(int maxprice) {
		this.price = maxprice;
	}
	
	@Override
	public int nextPrice() {
		return price;
	}

}
