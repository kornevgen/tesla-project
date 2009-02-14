package ru.teslaprj;

public interface TLB
{
	/** размер буфера для кэширования строк TLB */
	int getBufferSize();
	
	/** количество строк в TLB */
	int getSize();
	
	/** битовый размер виртуального адреса */
	int getVirtualAddressBitLen();
	
	/** битовый размер физического адреса */
	int getPhysicalAddressBitLen();
	
	int getRangeEndBit();
	
	int getRangeStartBit();
	
	int getMaximumOfMask();
	
	int getVPNd2EndBit();
	
	int getVPNd2StartBit();
	
	int getPFNBitLen();
}
