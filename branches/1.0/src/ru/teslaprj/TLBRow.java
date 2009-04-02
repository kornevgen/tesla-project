package ru.teslaprj;

import java.math.BigInteger;

public interface TLBRow
{
	Integer getRange();
	BigInteger getVPNd2();
	Integer getMask();
	Integer getGlobal();
	Integer getAsid();
	
	BigInteger getPFN0();
	int getValid0();
	int getmoDify0();
	
	BigInteger getPFN1();
	int getValid1();
	int getmoDify1();
}
