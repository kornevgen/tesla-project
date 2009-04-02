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
 * Статическое ограничение - это ограничение вида <<diff = const>> (const::[0..1]).
 * Динамическое ограничение - это ограничение произвольного вида на diff'ы.
 * 
 * Начальные статические ограничения берутся из тестового шаблона.
 * 
 * Правила "замыкания" (получения ограничений-следствий)
 * =====================================================
 * 0. объединение диапазонов
 * 
 * 1. д.ограничения "транзитивности":
 *      d(x,y) = 1 d(x,z) = 1, тогда d(y,z) = 1
 *      d(x,y) = 1 d(x,z) = 0, тогда d(y,z) = 0
 *      d(x,y) = 1 d(x,z) неизвестно d(y,z)=1, тогда ( d(x,z) = 1 )
 *      d(x,y) = 1 d(x,z) неизвестно d(y,z)=0, тогда ( d(x,z) = 0 )
 *      d(x,y) = 1 d(x,z) неизвестно d(y,z) неизвестно, тогда ( d(x,z) = d(y,z) )
 *      d(x,y) неизвестно d(x,z) неизвестно d(y,z) неизвестно, тогда ( nonfree(d), d(x,y) = 1 -> d(x,z) = d(y,z) )
 * 
 * 2. если diff могут быть несимметричными, то и д.ограничения "симметричности" ( d(x,y) = d(y,x) )
 * 
 * 3. совпадение тегов одного уровня пары инструкций ( d(t1,t2) = 1 ):
 * 		d(t1',t2') = 1 на всех(!) уровнях с меньшей длиной тега
 *      если d(s1,s2) = 1, то d(t1',t2')=1 и d(s1',s2')=1 на всех уровнях у этой пары инструкций
 *      если длина pfn не больше длины тега этого уровня, то d(pfn1LM, pfn2LM) = 1
 *      если длина pfn больше длины тега этого уровня,
 *      	 d(s1,s2) = 1,
 *      		то d(pfn1LM, pfn2LM) = 1
 *      если длина pfn не больше длины тега этого уровня,
 *           между физ.адресами не меняются биты тега\биты pfn в обеих тестовых ситуациях,
 *           	то d( v1[биты тега\биты pfn], v2[биты тега\биты pfn]) = 1
 * 4. несовпадение тегов одного уровня пары инструкций ( d(t1,t2) = 0 ):
 * 		d(t1',t2')=0 для всех уровней с большей длиной тега
 * 		d(p1LM, p2LM)=0
 * 
 * 5. совпадение сетов одного уровня пары инструкций ( d(s1,s2) = 1 ):
 * 		d(s1', s2') = 1 для всех уровней с не большей длиной сета
 * 
 * 6. несовпадение сетов одного уровня пары инстуркций ( d(s1,s2) = 0 ):
 * 		d(s1',s2') = 0 для всех уровней с не меньшей длиной сета
 * 		d(p1LM, p2LM) = 0
 * 		если на этом уровне сет входит в pfn
 * 			 tlb-индексы инструкций совпадают,
 * 				то d(pfn1LM, pfn2LM) = 0
 * 
 * 7. d(p1LM, p2LM) = const :
 * 		если преобразование физ.адреса после AT в физ.адрес перед LM взаимнооднозначное, то d(p1AT, p2AT) = const
 * 
 * 7а. d(p1LM, p2LM) = 1 :
 * 		d(t1,t2) = 1 на всех уровнях
 * 		d(s1,s2) = 1 на всех уровнях
 * 
 * 8. d(p1AT, p2AT) = const :
 * 		если преобразование физ.адреса после AT в физ.адрес перед LM взаимнооднозначное, то d(p1LM, p2LM) = const
 * 
 * 9. d(p1AT, p2AT) = 0 :
 * 		d(v1, v2) = 0
 * 
 * 10. d(v1,v2)=1
 * 		d(p1AT, p2AT) = 1
 * 		d(oddbit1, oddbit2) = 1
 * 		d(p1AT\биты унаследованные из вирт.адреса, p2AT\биты унаследованные из вирт.адреса) = 1
 * 		(те же биты, если преобразование из AT в LM взаимнооднозначное) => равенство тегов, сетов, кусков тегов и сетов
 *       
 * 11. d(pfn1LM, pfn2LM) = 0
 * 		если биты pfn преобразовываются между AT и LM взаимнооднозначно, то d(pfn1AT, pfn2AT) = 0
 * 		d(p1LM, p2LM) = 0
 * 
 * 12. d(pfn1LM, pfn2LM) = 1
 * 		если биты pfn преобразовываются между AT и LM взаимнооднозначно, то d(pfn1AT, pfn2AT) = 1
 * 		d(p1LM|pfn1LMbits, p2LM|pfn2LMbits) = 1 (это могут быть теги, сеты, части тегов и сетов на разных уровнях)
 * 
 * 13. d(pfn1AT, pfn2AT) = const
 * 		если биты pfn преобразовываются между AT и LM взаимнооднозначно, то d(pfn1LM, pfn2LM) = const
 * 		если tlb-индексы инструкций совпадают, то d(oddbit1, oddbit2) = const
 * 
 * 14. d(oddbit1, oddbit2) = 0
 * 		d(v1,v2) = 0
 * 
 * 15. d(oddbit1, oddbit2) = 1
 * 		если tlb-индексы инструкций совпадают, то d(pfn1AT, pfn2AT) = 1
 * 
 * Следует за тем, чтобы не плодить эквивалентные ограничения. Это может привести к зацикливанию процедуры
 * получения замыкания!
 * 
 * Данный constraintManager предполагает, что на верхнем уровне все ограничения представляются неделимыми
 * частями конъюнкции. => проблема хранения дизъюнкций!
 * 
 * Д.ограничение хранится в виде строкового представления с множеством задействованных переменных.
 * 
 * Операция фильтрации выбирает из всех д.ограничений только те, в которые входят переменные из
 * заданного множества.
 * 
 * Основная проблема при фильтрации - пропуск возможных ограничений.
 * Сложные ситуации (практически все связаны с дизъюнкцией):
 * 1. Для д.ограничений вида ( C(x,y) -> D(a,b) ; E(c,d) ) в результат фильтрации должна входить даже
 * then-часть, если все переменные в D разрешены фильтрацией, а какие-нибудь переменные в E не разрешены.
 * Аналогично с else-частью.
 * 2. Для д.ограничений вида ( С(x,y) -> D(a,b)/\E(c,d) ; ...) в результат фильтрации должна входить даже
 * часть следствия, если другая часть содержит неразрешенные переменные.
 * 3. Для д.ограничений вида ( C(x,y)\/D(a,b) -> E(c,d) ; ...) в результат фильтрации должна входить даже
 * часть посылки, если другая часть содержит неразрешенные переменные.
 * 4. Если присутствуют такие д.ограничения ( C(x,y) -> D(a,b) ; true ) и ( D(a,b) -> E(c,d) ; true ) и
 * переменные a,b не разрешены, а x,y,c,d разрешены, то в результат фильтрации должна входить импликация
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
		//TODO учесть, что 0й и 1й уровни одинаковы! (но на самом деле задействован только один уровень из них)
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
	 * @param diffs	переменные-diff'ы, которые лишь могут входить в динамические ограничения
	 * @return	строковое представление конъюнкции динамических ограничений
	 */
	public String getDynamicConstraints( Set<String> diffs )
	{
		return "";
	}
	
}
