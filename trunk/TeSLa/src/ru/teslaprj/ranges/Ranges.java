package ru.teslaprj.ranges;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.scheme.MemoryCommand;
import yices.YicesLite;

public class Ranges
{
	final Map<MemoryCommand, L1Range> l1Ranges;
	final Map<MemoryCommand, TLBRange> tlbRanges;
	YicesLite yl;
	int context;
	final int tagsetLength;
	final int pfnLength;
	
	public Ranges(L1Range[] l1values, TLBRange[] tlbvalues)
	{
		l1Ranges = new HashMap<MemoryCommand, L1Range>();
		for( L1Range r : l1values )
		{
			l1Ranges.put(r.getCommand(), r);
			r.setContext(this);
		}
		tlbRanges = new HashMap<MemoryCommand, TLBRange>();
		for( TLBRange r : tlbvalues )
		{
			tlbRanges.put(r.getCommand(), r);
			r.setContext(this);
		}
	}

	public boolean isConsistency()
	{
		yl = new YicesLite();
		context = yl.yicesl_mk_context();
		
		yl.yicesl_read( context, "(define-type tagset (bitvector " + tagsetLength + "))" );
		yl.yicesl_read( context, "(define-type pfn (bitvector " + pfnLength + "))" );
		yl.yicesl_read( context, "(define getPfn :: (-> tagset pfn) " +
				"(lambda " +
					"(x :: tagset) " +
					"(bv-extract " + (tagsetLength-1) + " " + (tagsetLength-pfnLength) + " x)" +
				"))" );
		
		for( MemoryCommand cmd : l1Ranges.keySet() )
		{
			yl.yicesl_read( context, "(define " + cmd.getTagset() + " :: tagset)" );
		}
		
		for( MemoryCommand cmd : l1Ranges.keySet() )
		{
			assert tlbRanges.containsKey(cmd); // `Cached Mapped' case
			tlbRanges.get(cmd).visit( l1Ranges.get(cmd) );
		}
		
		int ttt = yl.yicesl_inconsistent(context);
		boolean result = (ttt == 0);
        yl.yicesl_del_context(context);

        return result;
	}

	public void postAssert( String _assert )
	{
		yl.yicesl_read( context, "(assert " + _assert + ")" );
	}
	
	public Set<Long> getLinterM()
	{
		// TODO Auto-generated method stub
		return null;		
	}

	public int getTagsetLength() {
		return tagsetLength;
	}

	public Set<Long> getLinterPFN() {
		// TODO Auto-generated method stub
		return null;
	}

	public Set<Long> getLinterPFNminusM() {
		// TODO Auto-generated method stub
		return null;
	}

	public Set<Long> getLinterPFN(int tagIndex) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * возвращает все более старые элементы в microDTLB
	 */
	public Set<Long> getMremains(int tagIndex) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * возвращает L \inter [{M_tagIndex}]
	 */
	public Set<Long> getLinterMremains(int tagIndex) {
		// TODO Auto-generated method stub
		return null;
	}

}
