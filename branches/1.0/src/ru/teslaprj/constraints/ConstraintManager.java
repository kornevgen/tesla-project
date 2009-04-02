package ru.teslaprj.constraints;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.constraints.Constraint.Relation;
import ru.teslaprj.constraints.args.Tag;
import ru.teslaprj.scheme.Command;

/**
 * ����������� ����������� - ��� ����������� ���� <<diff = const>> (const::[0..1]).
 * ������������ ����������� - ��� ����������� ������������� ���� �� diff'�.
 * 
 * ��������� ����������� ����������� ������� �� ��������� �������.
 * 
 * ������� "���������" (��������� �����������-���������)
 * =====================================================
 * 0. ����������� ����������
 * 
 * 1. �.����������� "��������������":
 *      d(x,y) = 1 d(x,z) = 1, ����� d(y,z) = 1
 *      d(x,y) = 1 d(x,z) = 0, ����� d(y,z) = 0
 *      d(x,y) = 1 d(x,z) ���������� d(y,z)=1, ����� ( d(x,z) = 1 )
 *      d(x,y) = 1 d(x,z) ���������� d(y,z)=0, ����� ( d(x,z) = 0 )
 *      d(x,y) = 1 d(x,z) ���������� d(y,z) ����������, ����� ( d(x,z) = d(y,z) )
 *      d(x,y) ���������� d(x,z) ���������� d(y,z) ����������, ����� ( nonfree(d), d(x,y) = 1 -> d(x,z) = d(y,z) )
 * 
 * 2. ���� diff ����� ���� ���������������, �� � �.����������� "��������������" ( d(x,y) = d(y,x) )
 * 
 * 3. ���������� ����� ������ ������ ���� ���������� ( d(t1,t2) = 1 ):
 * 		d(t1',t2') = 1 �� ����(!) ������� � ������� ������ ����
 *      ���� d(s1,s2) = 1, �� d(t1',t2')=1 � d(s1',s2')=1 �� ���� ������� � ���� ���� ����������
 *      ���� ����� pfn �� ������ ����� ���� ����� ������, �� d(pfn1LM, pfn2LM) = 1
 *      ���� ����� pfn ������ ����� ���� ����� ������,
 *      	 d(s1,s2) = 1,
 *      		�� d(pfn1LM, pfn2LM) = 1
 *      ���� ����� pfn �� ������ ����� ���� ����� ������,
 *           ����� ���.�������� �� �������� ���� ����\���� pfn � ����� �������� ���������,
 *           	�� d( v1[���� ����\���� pfn], v2[���� ����\���� pfn]) = 1
 * 4. ������������ ����� ������ ������ ���� ���������� ( d(t1,t2) = 0 ):
 * 		d(t1',t2')=0 ��� ���� ������� � ������� ������ ����
 * 		d(p1LM, p2LM)=0
 * 
 * 5. ���������� ����� ������ ������ ���� ���������� ( d(s1,s2) = 1 ):
 * 		d(s1', s2') = 1 ��� ���� ������� � �� ������� ������ ����
 * 
 * 6. ������������ ����� ������ ������ ���� ���������� ( d(s1,s2) = 0 ):
 * 		d(s1',s2') = 0 ��� ���� ������� � �� ������� ������ ����
 * 		d(p1LM, p2LM) = 0
 * 		���� �� ���� ������ ��� ������ � pfn
 * 			 tlb-������� ���������� ���������,
 * 				�� d(pfn1LM, pfn2LM) = 0
 * 
 * 7. d(p1LM, p2LM) = const :
 * 		���� �������������� ���.������ ����� AT � ���.����� ����� LM ������������������, �� d(p1AT, p2AT) = const
 * 
 * 7�. d(p1LM, p2LM) = 1 :
 * 		d(t1,t2) = 1 �� ���� �������
 * 		d(s1,s2) = 1 �� ���� �������
 * 
 * 8. d(p1AT, p2AT) = const :
 * 		���� �������������� ���.������ ����� AT � ���.����� ����� LM ������������������, �� d(p1LM, p2LM) = const
 * 
 * 9. d(p1AT, p2AT) = 0 :
 * 		d(v1, v2) = 0
 * 
 * 10. d(v1,v2)=1
 * 		d(p1AT, p2AT) = 1
 * 		d(oddbit1, oddbit2) = 1
 * 		d(p1AT\���� �������������� �� ����.������, p2AT\���� �������������� �� ����.������) = 1
 * 		(�� �� ����, ���� �������������� �� AT � LM ������������������) => ��������� �����, �����, ������ ����� � �����
 *       
 * 11. d(pfn1LM, pfn2LM) = 0
 * 		���� ���� pfn ����������������� ����� AT � LM �����������������, �� d(pfn1AT, pfn2AT) = 0
 * 		d(p1LM, p2LM) = 0
 * 
 * 12. d(pfn1LM, pfn2LM) = 1
 * 		���� ���� pfn ����������������� ����� AT � LM �����������������, �� d(pfn1AT, pfn2AT) = 1
 * 		d(p1LM|pfn1LMbits, p2LM|pfn2LMbits) = 1 (��� ����� ���� ����, ����, ����� ����� � ����� �� ������ �������)
 * 
 * 13. d(pfn1AT, pfn2AT) = const
 * 		���� ���� pfn ����������������� ����� AT � LM �����������������, �� d(pfn1LM, pfn2LM) = const
 * 		���� tlb-������� ���������� ���������, �� d(oddbit1, oddbit2) = const
 * 
 * 14. d(oddbit1, oddbit2) = 0
 * 		d(v1,v2) = 0
 * 
 * 15. d(oddbit1, oddbit2) = 1
 * 		���� tlb-������� ���������� ���������, �� d(pfn1AT, pfn2AT) = 1
 * 
 * ������� �� ���, ����� �� ������� ������������� �����������. ��� ����� �������� � ������������ ���������
 * ��������� ���������!
 * 
 * ������ constraintManager ������������, ��� �� ������� ������ ��� ����������� �������������� ����������
 * ������� ����������. => �������� �������� ����������!
 * 
 * �.����������� �������� � ���� ���������� ������������� � ���������� ��������������� ����������.
 * 
 * �������� ���������� �������� �� ���� �.����������� ������ ��, � ������� ������ ���������� ��
 * ��������� ���������.
 * 
 * �������� �������� ��� ���������� - ������� ��������� �����������.
 * ������� �������� (����������� ��� ������� � �����������):
 * 1. ��� �.����������� ���� ( C(x,y) -> D(a,b) ; E(c,d) ) � ��������� ���������� ������ ������� ����
 * then-�����, ���� ��� ���������� � D ��������� �����������, � �����-������ ���������� � E �� ���������.
 * ���������� � else-������.
 * 2. ��� �.����������� ���� ( �(x,y) -> D(a,b)/\E(c,d) ; ...) � ��������� ���������� ������ ������� ����
 * ����� ���������, ���� ������ ����� �������� ������������� ����������.
 * 3. ��� �.����������� ���� ( C(x,y)\/D(a,b) -> E(c,d) ; ...) � ��������� ���������� ������ ������� ����
 * ����� �������, ���� ������ ����� �������� ������������� ����������.
 * 4. ���� ������������ ����� �.����������� ( C(x,y) -> D(a,b) ; true ) � ( D(a,b) -> E(c,d) ; true ) �
 * ���������� a,b �� ���������, � x,y,c,d ���������, �� � ��������� ���������� ������ ������� ����������
 * ( C(x,y) -> E(c,d) ; true )
 *
 * 
 * @author kornevgen
 *
 */
public class ConstraintManager
{
	private class DynamicConstraint
	{
		String stringForm;
		Set<String> usedDiffs;
	}
	
	private class StaticConstraint
	{
		String diff;
		int value;
	}
	
	List<DynamicConstraint> dynamicConstraints = new ArrayList<DynamicConstraint>();
	List<StaticConstraint> staticConstraints = new ArrayList<StaticConstraint>();
	Map<String, List<Argument>> diffVars = new HashMap<String, List<Argument>>();
	
	public void add( Constraint c )
	{
		StaticConstraint cc;
		//TODO convert c to static constraint form using diffVars
		
//		staticConstraints.add(cc);
	}
	
	public void closeConstraints()
	{
		//TODO closure of constraints
	}
	
	public void addDifferenceVar( Argument a, Argument b, String diff )
	{
		diffVars.put( diff, Arrays.asList( a, b ) );
	}
	
	/** diff +> value */
	public Map<String, Integer> getStaticConstraints()
	{
		//TODO ������, ��� 0� � 1� ������ ���������! (�� �� ����� ���� ������������ ������ ���� ������� �� ���)
		// TODO build static constraints (new diffs may appeared!)
		return new HashMap<String, Integer>();
	}

	///// GETTING CONSTRAINTS FOR SPECIAL ARGUMENT TYPES:
	
	public Map<List<Command>, Relation> getVirtualAddressesStaticConstraints()
	{
		// TODO Auto-generated method stub
		return new HashMap<List<Command>, Relation>();
	}

	public Map<List<Tag>, Relation> getTagStaticConstraints()
	{
		// TODO Auto-generated method stub
		return new HashMap<List<Tag>, Relation>();
	}
	
	/**
	 * 
	 * @param diffs	����������-diff'�, ������� ���� ����� ������� � ������������ �����������
	 * @return	��������� ������������� ���������� ������������ �����������
	 */
	public String getDynamicConstraints( Set<String> diffs )
	{
		return "";
	}
	
}
