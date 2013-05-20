
public class FileNameGenerator {
	
	public FileNameGenerator(final String suffix) {
		assert suffix != null;
		this.suffix = suffix;
	}
	
	public String get(int number) {
		assert 1 <= number;
		assert number < 1000;
		
		if (number < 10) {
			return "00" + number + suffix;
		} else if (number < 100) {
			return "0" + number + suffix;
		} else {
			return "" + number + suffix;
		}
	}

	private final String suffix;
}
