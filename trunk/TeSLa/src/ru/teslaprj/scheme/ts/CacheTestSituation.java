package ru.teslaprj.scheme.ts;

public interface CacheTestSituation extends ProcedureTestSituation
{
	/** ������� ����; 0 - ��� DATA-���� */
	int getLevel();
	
	/** ��� ���������� - ����;
	 * null - ��� ����� ���� ����� ���������� */
	String getTagVar();
	
	/** ��� ���������� - ���;
	 * null - ��� ����� ���� ����� ���������� */
	String getSetVar();
}
