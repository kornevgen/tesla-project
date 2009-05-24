package ru.teslaprj.ranges;

import java.util.HashMap;
import java.util.Map;

import ru.teslaprj.scheme.Command;

public class Ranges
{
	final Map<Command, Range> l1Ranges;
	final Map<Command, Range> tlbRanges;
	
	public Ranges(Range[] l1values, Range[] tlbvalues)
	{
		l1Ranges = new HashMap<Command, Range>();
		for( Range r : l1values )
		{
			l1Ranges.put(r.getCommand(), r);
		}
		tlbRanges = new HashMap<Command, Range>();
		for( Range r : tlbvalues )
		{
			tlbRanges.put(r.getCommand(), r);
		}		
	}

	public boolean isConsistency() {
		// проходимся по каждой команде и формируем ограничение для ее тегсета
//		for( Command cmd : l1Ranges.keySet() )
//		{
//			assert tlbRanges.containsKey(cmd);
//			String yicesAssert = getAssert( l1Ranges.get(cmd), tlbRanges.get(cmd) );
//			// write to the yices
//		}
		
		YicesLite yl=new YicesLite();
		int ctx=yl.yicesl_mk_context();
	//definig some bools
		yl.yicesl_read(ctx, "(define a::bool)");
		yl.yicesl_read(ctx, "(define b::bool)");
		yl.yicesl_read(ctx, "(define c::bool)");
		yl.yicesl_read(ctx, "(define d::bool)");
		yl.yicesl_read(ctx, "(define e::bool)");
		yl.yicesl_read(ctx, "(define f::bool)");
	//adding some bool expressions
		yl.yicesl_read(ctx, "(assert (and a b)");
		yl.yicesl_read(ctx, "(assert (and a (not b)))");
	//adding functions to count

	     yl.yicesl_read(ctx, "(define x1::int (if (a) (= x1 1)(= x1 0)))");//line 15
		yl.yicesl_read(ctx, "(define x2::int (if (f) (= x2 1)(= x2 0)))");
		yl.yicesl_read(ctx, "(define x3::int (if (b) (= x3 1)(= x3 0)))");
		yl.yicesl_read(ctx, "(define x4::int (if (c) (= x4 1)(= x4 0)))");
		yl.yicesl_read(ctx, "(define x5::int (if (d) (= x5 1)(= x5 0)))");
		yl.yicesl_read(ctx, "(define x6::int (if (e) (= x6 1)(= x6 0)))");

	        yl.yicesl_read(ctx, "(define x::bool (/=(+ x1 x2 x3 x4 x5 x6) 3)");
	        yl.yicesl_read(ctx, "(check)");

		
		return false;
	}

}
