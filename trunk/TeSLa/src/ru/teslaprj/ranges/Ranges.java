package ru.teslaprj.ranges;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.Cache;
import ru.teslaprj.TLB;
import ru.teslaprj.TLBRow;
import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.Scheme;
import yices.YicesLite;

public class Ranges
{
	final Map<MemoryCommand, L1Range> l1Ranges;
	final Map<MemoryCommand, TLBRange> tlbRanges;
	YicesLite yl;
	int context;
	final int tagsetLength;
	final int pfnLength;
	final int tlbAssoc;
	final Cache dataL1;
	final TLB tlb;
	
	public Ranges(L1Range[] l1values, TLBRange[] tlbvalues, Cache dataL1, TLB tlb)
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
		tlbAssoc = tlb.getDTLBSize();
		tagsetLength = dataL1.getTagBitLength() + dataL1.getSetNumberBitLength();
		pfnLength = tlb.getPFNBitLen();
		this.dataL1 = dataL1;
		this.tlb = tlb;
	}

	static int nnn = 0;
	public boolean isConsistency()
	{
		yl = new YicesLite();
		yl.yicesl_enable_log_file("system" + (++nnn) + ".txt" );
		System.out.print("system" + nnn + ": ");
		yl.yicesl_set_verbosity((short)0);
		yl.yicesl_enable_type_checker((short)1);
		yl.yicesl_set_output_file("output.txt");
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

        try
        {
	        BufferedWriter w = new BufferedWriter( 
	        		new FileWriter( new File( "system" + nnn + ".txt" ), true ) );
	        Scheme scheme = null;
	        for(MemoryCommand cmd : l1Ranges.keySet() )
	        {
	        	scheme = cmd.getScheme();
	        	break;
	        }
	        for( Command cmd : scheme.getCommands() )
	        {
	        	if ( l1Ranges.containsKey(cmd) && tlbRanges.containsKey(cmd) )
	        	{
	        		w.write("[ " + l1Ranges.get(cmd).print() + " || " +
	        				tlbRanges.get(cmd).print() + " ]" );
	        		w.newLine();
	        	}
	        }
	        
	        w.close();
        }
        catch( IOException e )
        {
        	System.out.println(e.getStackTrace());
        }
        
        return result;
	}

	public void postAssert( String _assert )
	{
		yl.yicesl_read( context, "(assert " + _assert + ")" );
	}
	
	public void postDefine( String name, String type, String expression )
	{
		yl.yicesl_read( context, "(define " + name + " :: " + type + " " + expression + ")" );
	}
	
	public Set<Long> getLinterM()
	{
		Set<Long> result = new HashSet<Long>();
		
		for( int p : tlb.getDTLB() )
		{
			TLBRow r = tlb.getRow(p);
			if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN0().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, tagsetLength - pfnLength ) == pfn )
						result.add(l);
				}
			}
			if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN1().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, tagsetLength - pfnLength ) == pfn )
						result.add(l);
				}
			}
		}
		
		return result;		
	}

	public int getTagsetLength()
	{
		return tagsetLength;
	}
	
	public int getPfnLength()
	{
		return pfnLength;
	}

	public Set<Long> getLinterPFN() {
		Set<Long> result = new HashSet<Long>();
		
		for( int p = 0; p < tlb.getJTLBSize(); p++ )
		{
			TLBRow r = tlb.getRow(p);
			if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN0().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, tagsetLength - pfnLength ) == pfn )
						result.add(l);
				}
			}
			if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN1().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, tagsetLength - pfnLength ) == pfn )
						result.add(l);
				}
			}
		}
		
		return result;		
	}

	public Set<Long> getLinterPFNminusM() {
		Set<Long> result = new HashSet<Long>();
		
		for( int p = 0; p < tlb.getJTLBSize(); p++ )
		{
			if ( tlb.getDTLB().contains(p) )
				continue;
			
			TLBRow r = tlb.getRow(p);
			if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN0().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, tagsetLength - pfnLength ) == pfn )
						result.add(l);
				}
			}
			if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN1().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, tagsetLength - pfnLength ) == pfn )
						result.add(l);
				}
			}
		}
		
		return result;		
	}

//	public Set<Long> getLinterPFN(int tagIndex) {
//		// TODO Auto-generated method stub
//		return null;
//	}

	/**
	 * ���������� ��� ����� ������ �������� � microDTLB
	 */
	public Set<Long> getMremains(int tagIndex) {
		Set<Long> result = new HashSet<Long>();
		for( int p = tagIndex + 1; p < tlb.getDTLBSize(); p++ )
		{
			result.add( tlb.getRow( tlb.getDTLB().get(p) ).getPFN0().longValue() );
			result.add( tlb.getRow( tlb.getDTLB().get(p) ).getPFN1().longValue() );
		}
		return result;
	}

	/**
	 * ���������� L \inter [{M_tagIndex}]
	 */
	public Set<Long> getLinterMremains(int tagIndex) {
		Set<Long> result = new HashSet<Long>();
		
			TLBRow r = tlb.getRow( tlb.getDTLB().get( tagIndex ) );
			if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN0().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, dataL1.getTagBitLength() + dataL1.getSetNumberBitLength() - tlb.getPFNBitLen() ) == pfn )
						result.add(l);
				}
			}
			if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				long pfn = r.getPFN1().longValue();
				for( long l : dataL1.getValidTagsets() )
				{
					if ( l / (long)Math.pow(2, dataL1.getTagBitLength() + dataL1.getSetNumberBitLength() - tlb.getPFNBitLen() ) == pfn )
						result.add(l);
				}
			}

		
		return result;		
	}

	public int getTLBAssociativity() {
		return tlbAssoc;
	}

	public Set<Long> getLinterMrange(int minimumM, int maximumM)
	{
		Set<Long> result = new HashSet<Long>();
		for( int m = minimumM; m <= maximumM; m++ )
		{
			result.addAll( getLinterMremains(m - 1) );
		}
		return result;
	}
	
	private int counter = 0;
	public synchronized long getUniqueNumber()
	{
		return ++counter;
	}

	public Set<Long> getMfull()
	{
		Set<Long> result = new HashSet<Long>();
		for( int rowIndex : tlb.getDTLB() )
		{
			TLBRow r = tlb.getRow( rowIndex );
			if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				result.add( r.getPFN0().longValue() );
			}
			if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				result.add( r.getPFN1().longValue() );
			}
		}
		return result;
	}

	public Set<Long> getPFNminusMfull()
	{
		Set<Long> result = new HashSet<Long>();
		for( int rowIndex = 0; rowIndex < tlb.getJTLBSize(); rowIndex++ )
		{
			if ( tlb.getDTLB().contains( rowIndex ) )
				continue;
			
			TLBRow r = tlb.getRow( rowIndex );
			if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				result.add( r.getPFN0().longValue() );
			}
			if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
			{
				result.add( r.getPFN1().longValue() );
			}
		}
		return result;
	}

	public Set<Long> getM(int i)
	{
		Set<Long> result = new HashSet<Long>();
		
		TLBRow r = tlb.getRow( tlb.getDTLB().get(i) );

		if ( r.getValid0() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
		{
			result.add( r.getPFN0().longValue() );
		}
		if ( r.getValid1() == 1 && r.getMask().intValue() == tlb.getPFNBitLen() )
		{
			result.add( r.getPFN1().longValue() );
		}
		
		return result;
	}

}
