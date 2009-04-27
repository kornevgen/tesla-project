package ru.teslaprj;

import java.util.List;
import java.util.Set;

public interface TLB
{
	/** ������ ������ ��� ����������� ����� TLB */
	int getMicroTLBSize();
	
	/** ���������� ����� � TLB */
	int getJTLBSize();
	
	/** ������� ������ ������������ ������ */
	int getSEGBITS();
	
	/** ������� ������ ����������� ������ */
	int getPABITS();
	
	int getRangeEndBit();
	
	int getRangeStartBit();
	
	/**
	 * oddbit_index = 2 * mask + vpmd2start - 1
	 * ���������� vpnd2 :: SEGBITS-1 .. oddbit_index-1
	 * vpnd2start = 13, mask = 0 (minimum) -> oddbit = 12 (vpnd2 :: SEGBITS-1 .. 13 - full)
	 * vpnd2start = 13, mask = 1           -> oddbit = 14 (vpnd2 :: SEGBITS-1 .. 15       )
	 */
	int getMaximumOfMask();
	
	int getVPNd2EndBit();
	
	int getVPNd2StartBit();
	
	int getPFNBitLen();
	
	int getASIDBitLen();
	
	TLBRow getRow( int index );

	/**
	 * ������� �����, �������� � DTLB, � ������� ���������� lru
	 */
	List<Integer> getDTLB();
	
	/**
	 * ������� �����, �������� � ITLB, � ������� ���������� lru
	 */
	List<Integer> getITLB();

	Set<Long> getMicroPfns();
}
