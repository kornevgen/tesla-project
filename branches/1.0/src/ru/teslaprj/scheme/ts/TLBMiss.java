package ru.teslaprj.scheme.ts;

public interface TLBMiss extends TLBExists
{
	String getVirtualAddressVar();
	String getPhysicalAddressVar();
}
