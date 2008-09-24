package ru.teslaprj;

import java.util.List;
import java.util.Map;

import ru.teslaprj.scheme.Definition;
import ru.teslaprj.scheme.Scheme;

public class Solver
{
	public Verdict solve( Scheme scheme, List<com.unitesk.micro.Cache> cacheState )
	{
		//TODO 1. translate `scheme` to .clp
		//TODO 2. run ECLiPSe and get results
		//TODO 3. analyze results
		return null;
	}
	
	public class Verdict
	{
		/**
		 * @return [ definition +> value ]
		 */
		public Map<Definition, Long> getDefinitionValues() {
			return definitionValues;
		}
		/**
		 * @return [ set +> tag-list ]-list (by levels)
		 */
		public List< Map<Long, List<Long>> > getCacheInitialization() {
			return cacheInitialization;
		}
		
		private Map<Definition, Long> definitionValues;
		private List< Map< Long, List<Long> > > cacheInitialization;
	}
}
