package ru.teslaprj.constraints;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import ru.teslaprj.constraints.Constraint.Relation;
import ru.teslaprj.constraints.args.Tag;
import ru.teslaprj.scheme.Command;

public class ConstraintManager
{
	public void add( Constraint c )
	{
		//TODO add constraint
	}
	
	public void closeConstraints()
	{
		//TODO closure of constraints
	}
	
	public Collection<Constraint> getConstraints()
	{
		//TODO get constraints - really need ?
		return new HashSet<Constraint>();
	}
	
	public String getDynamicConstraints()
	{
		// TODO build dynamic constraints
		return "";
	}
	
	public void addDifferenceVar( Argument a, Argument b, String diff )
	{
		// TODO add difference var
	}
	
	/** diff +> value */
	public Map<String, Integer> getStaticConstraints()
	{
		// TODO build static constraints (new diffs may appeared!)
		return new HashMap<String, Integer>();
	}

	public Map<List<Command>, Relation> getVirtualAddressesConstraints()
	{
		// TODO Auto-generated method stub
		return new HashMap<List<Command>, Relation>();
	}

	public Map<List<Tag>, Relation> getTagStaticConstraints() {
		// TODO Auto-generated method stub
		return new HashMap<List<Tag>, Relation>();
	}
}
