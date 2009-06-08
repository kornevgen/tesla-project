package ru.teslaprj.ranges.ts;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ru.teslaprj.ranges.L1Range;
import ru.teslaprj.ranges.BlockRange.TLB;
import ru.teslaprj.scheme.MemoryCommand;
import ru.teslaprj.scheme.ts.TLBHit;

public class InitialL1Hit extends L1Range
{

	public InitialL1Hit(MemoryCommand cmd) {
		super(cmd);
	}

	@Override
	public void visitBlockTlbMiss(TLB range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterMremains(range.getTagIndex() - 1) )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		getContext().postAssert( constraint.append(")").toString() );
				
		List<MemoryCommand> remainedFromBlock = new ArrayList<MemoryCommand>( range.getBlock() );
		Set<Long> remainPfns = getContext().getMremains(range.getTagIndex() - 1);
		Set<String> visitedTagsets = new HashSet<String>();
		boolean weAreInBlock = false;
		int tlbAssoc = getContext().getTLBAssociativity();
		
		for( MemoryCommand cmd : range.getPreviousCommands() )
		{
			if ( cmd.getTLBSituation() instanceof TLBHit )
			{
				if ( remainedFromBlock.contains(cmd) )
				{
					// in-block constraint
					// m < assoc /\ ^cmd.getTS()^ \in remainPfns /\ ^cmd.getTS()^ \notin ^visitedBlockTagsets^ )
					if ( range.getTagIndex() == tlbAssoc )
						getContext().postAssert("false");
					else
					{
						constraint = new StringBuffer( "(or false " );
						for( long l : remainPfns )
						{
							constraint.append("(= " )
								.append("(getPfn ").append( getCommand().getTagset() )
								.append(") (mk-bv " ).append( getContext().getPfnLength() ).append(" " ).append(l)
							.append("))");
						}
						getContext().postAssert( constraint.append(")").toString() );
						
						for( String ts : visitedTagsets )
						{
							getContext().postAssert( new StringBuffer("(/= ")
								.append("(getPfn ").append( getCommand().getTagset() )
								.append(") (getPfn " ).append( ts ).append( "))").toString() );
						}
					}
				}
				else if ( weAreInBlock )
				{
					// constraint "внутри блока", но не в блоке
					// if ( m < assoc ) (^cmd.getTS()^ \notin remainPfns ) \/ ^cmd.getTS()^ \in ^visitedTagsets^ )
					constraint = new StringBuffer();
					if ( range.getTagIndex() < tlbAssoc )
					{
						if ( ! visitedTagsets.isEmpty() )
							constraint.append("(or " );
						
						constraint.append( "(and true ");
						for( long l : remainPfns )
						{
							constraint.append("(/= ")
									.append( "(getPfn ").append( cmd.getTagset() ).append( " )" )
									.append( "(mk-bv " ).append( getContext().getPfnLength() ).append( " " )
									.append(l).append("))");
						}
						constraint.append( " )");
						
						for( String ts : visitedTagsets )
						{
							constraint.append("(= ")
							.append( " (getPfn " ).append( cmd.getTagset() ).append( " )" )
							.append( " (getPfn " ).append( ts ).append( " )" )
							.append(")");
						}
						
						
						if ( ! visitedTagsets.isEmpty() )
							constraint.append(" )" );
						
						getContext().postAssert( constraint.toString() );
					}
					else
					{
						for( String ts : visitedTagsets )
						{
							getContext().postAssert( new StringBuffer("(= ")
							.append( " (getPfn " ).append( cmd.getTagset() ).append( " )" )
							.append( " (getPfn " ).append( ts ).append( " )" )
							.append(")").toString() );
						}
					}
				}
				else
				{
					// constraint "перед блоком"
					// if ( m < assoc ) ^cmd.getTS()^ \notin remainPfns
					if ( range.getTagIndex() < tlbAssoc )
					{
						for( long l : remainPfns )
						{
							getContext().postAssert(
									new StringBuffer("(/= ")
									.append( "(getPfn ").append( cmd.getTagset() ).append( " )" )
									.append( "(mk-bv " ).append( getContext().getPfnLength() ).append( " " )
									.append(l).append("))").toString()
								);
						}
					}
				}
			}
			
			if ( remainedFromBlock.contains(cmd) )
			{
				visitedTagsets.add( cmd.getTagset() );
				remainedFromBlock.remove(cmd);
				weAreInBlock = true;
			}
			
			if ( remainedFromBlock.isEmpty() )
				break;
		}

	}

	@Override
	public void visitEvictingTlbHit(EvictingTlbHit range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterPFN() )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
		
		constraint = new StringBuffer().append("(or false ");
		for( MemoryCommand cmd : range.getEvictings() )
		{
			constraint.append( "(= " )
				.append( "(getPfn " ).append( getCommand().getTagset() ).append( ")" )
				.append( "(getPfn " ).append( cmd.getTagset() ).append( ")" )
				.append(")");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
	}

	@Override
	public void visitInitialTlbHit(InitialTlbHit range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterM() )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
	}

	@Override
	public void visitInitialTlbMiss(InitialTlbMiss range)
	{
		StringBuffer constraint = new StringBuffer().append("(or false ");
		for( long l : getContext().getLinterPFNminusM() )
		{
			constraint.append( "(= " )
				.append( getCommand().getTagset() )
				.append( " (mk-bv " ).append( getContext().getTagsetLength() ).append( l )
				.append(" ) )");
		}
		constraint.append(")");
		getContext().postAssert( constraint.toString() );
		
		for( MemoryCommand cmd : range.getEvictings() )
		{
			getContext().postAssert( new StringBuffer( "(/= " )
				.append( "(getPfn " ).append( getCommand().getTagset() ).append( ")" )
				.append( "(getPfn " ).append( cmd.getTagset() ).append( ")" )
				.append(")").toString() );
		}
	}

	@Override
	public String print()
	{
		return "L1Hit( " + getCommand().getTagset() + " ) to the initial";
	}
}
