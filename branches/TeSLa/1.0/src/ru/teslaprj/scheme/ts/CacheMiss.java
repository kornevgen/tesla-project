package ru.teslaprj.scheme.ts;

public interface CacheMiss extends CacheTestSituation
{
	/** ��� ���������� - ������������ ����;
	 * null - ����������� ��� ����� ���� ����� ���������� */
	String getVTagVar();
}
