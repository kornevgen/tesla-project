package ru.teslaprj;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ru.teslaprj.scheme.Command;
import ru.teslaprj.scheme.ts.CacheHit;
import ru.teslaprj.scheme.ts.CacheMiss;
import ru.teslaprj.scheme.ts.ProcedureTestSituation;

public class CacheProblem
{
	protected interface Rangeable {}
	
	protected class InitialTagSet implements Rangeable
	{
		String var;
	}
	
	protected abstract class ProcessElement implements Rangeable
	{
		CurrentCacheState cacheState;
		Command command;
	}
	
	protected class Hit extends ProcessElement
	{
	}
	
	protected class Miss extends ProcessElement
	{
		Rangeable rangeStart;
		Collection<Rangeable> range;
	}
	
	protected class CurrentCacheState
	{
		List<InitialTagSet> initial;
		List<ProcessElement> vytesn;
		List<ProcessElement> vytesned;
	}
	
	Map<Cache, List<Rangeable>> processes = new HashMap<Cache, List<Rangeable>>();
	
	public CacheProblem( 
			List<Command> commands, 
			Map<Cache, List<String>> initials ) 
	{
		int level = 0;
		for( Cache cache : initials.keySet() )
		{
			level++;
			List<Rangeable> process = new ArrayList<Rangeable>();
			
			//fill 'process':
			//1. initial tagsets
			for( int i = 0; i < cache.getSectionNumber(); i++ )
			{
				InitialTagSet init = new InitialTagSet();
				init.var = initials.get(cache).get(i);
				process.add( init );
			}
			//2. scheme's test situations
			for( Command cmd : commands )
			{
				// get test situation for this cache
	    		Set<ProcedureTestSituation> pts;
	    		if ( cmd.getTestSituationParameters().containsKey("LoadMemory") )
	    			pts = cmd.getTestSituationParameters().get("LoadMemory");
	    		else if ( cmd.getTestSituationParameters().containsKey("StoreMemory") )
	    			pts = cmd.getTestSituationParameters().get("StoreMemory");
	    		else
	    			continue;
	    		
	    		for( ProcedureTestSituation ts : pts )
	    		{
	    			ProcessElement elem;
	    			if ( ts instanceof CacheHit )
	    			{
	    				// TODO где проверить, что для l1Hit нельзя указывать l2Hit?
	    				if ( ((CacheHit) ts).getLevel() != level )
	    					continue;
	    				elem = new Hit();
	    			}
	    			else if ( ts instanceof CacheMiss )
	    			{
	    				if ( ((CacheMiss) ts).getLevel() != level )
	    					continue;
	    				elem = new Miss();
	    				// выбрать конец диапазона для miss
	    				//select диапазоны вытеснения (проставить концы к началам и наоборот)
	    				//(каждый miss заканчивает диапазон - либо к команде, либо к эл-ту нач.региона!)	    				
	    				((Miss)elem).rangeStart = process.get(process.size() - cache.getSectionNumber());
	    				// после совершения выборе диапазона возникает ограничение на регионы тегсетов внутри диапазона
	    				// set a range
	    				Collection<Rangeable> range = new HashSet<Rangeable>();
	    				for( int i = process.size() - cache.getSectionNumber() + 1; i < process.size(); i++ )
	    				{
	    					// set a region: here is nothing to do
	    					range.add( process.get(i) );
	    				}	    				
	    				((Miss)elem).range = range;
	    			}
	    			else
	    				continue;
	    			
	    			// вычисляем состояние кэша
	    			if ( process.size() >= cache.getSectionNumber() )
	    			{
		    			CurrentCacheState c = new CurrentCacheState();
		    			c.initial = new ArrayList<InitialTagSet>();
		    			for( int i = 0; i < cache.getSectionNumber(); i++ )
		    			{
		    				c.initial.add( (InitialTagSet)process.get(i) );
		    			}
		    			c.vytesn = new ArrayList<ProcessElement>();
		    			c.vytesned = new ArrayList<ProcessElement>();
		    			for( int i = cache.getSectionNumber(); i < process.size(); i++ )
		    			{
		    				Rangeable e = process.get(i);
		    				if ( e instanceof Miss )
		    				{
		    					c.vytesn.add( (Miss)e );
		    					c.vytesned.add( (ProcessElement)((Miss)e).rangeStart );
		    				}
		    			}
		    			elem.cacheState = c;
	    			}
	    			
	    			elem.command = cmd;
	    			
    				process.add( elem );
	    		}
			}
			
			processes.put(cache, process);
		}
	}
	
	public Collection<Hit> getHits( Cache cache )
	{
		List<Hit> result = new ArrayList<Hit>();
		for( Rangeable elem : processes.get(cache) )
		{
			if ( elem instanceof Hit )
				result.add((Hit)elem);
		}
		return result;
	}

	public Collection<Miss> getMisses(Cache cache)
	{
		List<Miss> result = new ArrayList<Miss>();
		for( Rangeable elem : processes.get(cache) )
		{
			if ( elem instanceof Miss )
				result.add((Miss)elem);
		}
		return result;
	}

	public String findDiffVar(
			Map<List<Command>, String> tagsetDiffs,
			Map<String, Map<Command, String>> diffs,
			Rangeable e1,
			Rangeable e2) 
	{
		if ( e2 instanceof InitialTagSet )
		{
			InitialTagSet init2 = (InitialTagSet)e2;
			if ( e1 instanceof InitialTagSet )  // e1 instanceof InitialTagSet, e2 instanceof InitialTagSet
			{
				if ( e1.hashCode() == e2.hashCode() )
				{
					return "1";
				}
				else
				{
					return "0";
				}
			}
			else // e1 instanceof ProcessElement, e2 instanceof InitialTagSet
			{
				if ( ! diffs.containsKey(init2.var) )
					throw new Error( "unknown diff :(" );
				
				return diffs.get(init2.var).get(((ProcessElement)e1).command);
			}
		}
		else
		{
			ProcessElement elem2 = (ProcessElement)e2;
			if ( e1 instanceof InitialTagSet )
			{
				if ( ! diffs.containsKey(((InitialTagSet)e1).var) )
					throw new Error( "unknown diff :(" );
				
				return diffs.get(((InitialTagSet)e1).var).get(elem2.command);
			}
			else // e1 instanceof ProcessElement, e2 instanceof ProcessElement
			{
				Command c1 = ((ProcessElement)e1).command;
				Command c2 = ((ProcessElement)e2).command;
				if ( c1.hashCode() == c2.hashCode() )
					return "1";  // always equal
				
				if ( c1.hashCode() > c2.hashCode() )
				{
					Command c = c1;
					c1 = c2;
					c2 = c;
				}
				
				for( List<Command> cmds : tagsetDiffs.keySet() )
				{
					if ( cmds.get(0).equals(c1) && cmds.get(1).equals(c2) )
						return tagsetDiffs.get(cmds);
				}
				
				throw new Error( "unknown diff :(" );
			}
		}
	}

	public String findDiffVar(
			Map<List<Command>, String> diffs,
			Command c1, Command c2)
	{
		Command cmd1 = c1;
		Command cmd2 = c2;
		if ( c1.hashCode() < c2.hashCode() )
		{
			cmd1 = c2;
			cmd2 = c1;
		}
		for( List<Command> c : diffs.keySet() )
		{
			if ( c.get(0).equals( cmd1 ) && c.get(1).equals( cmd2 ) )
				return diffs.get(c);
		}
		throw new Error( "unknown diff" );
	}

}
