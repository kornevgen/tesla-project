package ru.teslaprj.scheme.ts;

public interface CacheMiss extends ProcedureTestSituation
{
	/** ������� ����, � ������� ���������� ������ */
	int getLevel();
	
	/** ��� ���������� - ����;
	 * null - ��� ����� ���� ����� ���������� */
	String getTagVar();
	
	/** ��� ���������� - ���;
	 * null - ��� ����� ���� ����� ���������� */
	String getSetVar();
	
	/** ��� ���������� - ������������ ����;
	 * null - ����������� ��� ����� ���� ����� ���������� */
	String getVTagVar();
}
