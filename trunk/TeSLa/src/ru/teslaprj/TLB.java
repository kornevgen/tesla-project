package ru.teslaprj;

public interface TLB
{
	/** ������ ������ ��� ����������� ����� TLB */
	int getBufferSize();
	
	/** ���������� ����� � TLB */
	int getSize();
	
	/** ������� ������ ������������ ������ */
	int getVirtualAddressBitLen();
	
	/** ������� ������ ����������� ������ */
	int getPhysicalAddressBitLen();
	
	int getRangeEndBit();
	
	int getRangeStartBit();
	
	int getMaximumOfMask();
	
	int getVPNd2EndBit();
	
	int getVPNd2StartBit();
	
	int getPFNBitLen();
}
