package ru.teslaprj.ranges.tsiterators;

import java.util.Iterator;
import java.util.List;

import ru.teslaprj.scheme.Command;

public class BlockIterator implements Iterator<int[]>
{
	private static long factorial( int n )
	{
		if ( n < 2 )
			return 1;
		long result = 1;
		while ( n > 0 )
		{
			result *= (n--);
		}
		return result;
	}
	
	// ������� ���������������������� ����� blockLength
	public BlockIterator( int blockLength, List<Command> commands )
	{
		assert blockLength <= commands.size();
		iterationsCount = factorial(commands.size()) / 
			( factorial(blockLength) * factorial(commands.size() - blockLength) );
		allCommands = commands;
		this.blockLength = blockLength;
	}
	
	final long iterationsCount;
	final int blockLength;
	int i = 0;
	List<Command> allCommands;
	int[] block;

	@Override
	public boolean hasNext() {
		return i < iterationsCount;
	}

	@Override
	public int[] next()
	{
		if ( i == iterationsCount )
			return null;
		i++;
		
		if ( i == 1 )
		{
			block = new int[blockLength];
			for( int j = 0; j < blockLength; j++ )
			{
				block[j] = j;
			}
			return block;
		}
		else
		{
			// ����, ��� ����� �������
			// ������� ���
			// ����������� � ���� ��� �����������
			int index = blockLength;
			while( --index >= 0 )
			{
				if ( block[index] < allCommands.size() - index + blockLength )
				{
					// ����� ������� index
					int t = ++block[index];
					int j = index+1;
					while( j < blockLength )
						block[j++] = ++t;
					return block;
				}
			}
			return null;
		}
	}

	@Override
	public void remove() {
	}
}
