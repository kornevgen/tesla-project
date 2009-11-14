package com.unitesk.testfusion.core.tesla;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.jruby.RubyHash;

import com.unitesk.kmd64.model.GPR;
import com.unitesk.kmd64.model.KMD64;
import com.unitesk.kmd64.model.L1Cache;
import com.unitesk.kmd64.model.TLB;
import com.unitesk.kmd64.model.TLB48;
import com.unitesk.kmd64.model.TLB.Entry;
import com.unitesk.kmd64.model.deps.PADependency;
import com.unitesk.kmd64.model.deps.VADependency;
import com.unitesk.kmd64.model.isa.memory.LDInstruction;
import com.unitesk.kmd64.model.isa.memory.LWInstruction;
import com.unitesk.kmd64.model.isa.memory.MemoryInstruction;
import com.unitesk.kmd64.model.isa.memory.SBInstruction;
import com.unitesk.kmd64.model.isa.memory.SDInstruction;
import com.unitesk.kmd64.model.isa.memory.situation.LWSituation;
import com.unitesk.kmd64.model.isa.memory.situation.MemorySituation;
import com.unitesk.kmd64.model.isa.memory.situation.SBSituation;
import com.unitesk.kmd64.model.lib.LoadImmediate64Program;
import com.unitesk.kmd64.model.lib.SetKSeg0CachePolicyProgram;
import com.unitesk.kmd64.model.lib.WriteTLBEntryProgram;
import com.unitesk.testfusion.core.dependency.Dependencies;
import com.unitesk.testfusion.core.dependency.Dependency;
import com.unitesk.testfusion.core.model.Instruction;
import com.unitesk.testfusion.core.model.Operand;
import com.unitesk.testfusion.core.model.Program;
import com.unitesk.testfusion.core.model.register.Register;
import com.unitesk.testfusion.core.model.register.Register32;
import com.unitesk.testfusion.core.model.register.Register64;


public class Runner
{
	private final String jrubyHome;
	private final String load_path;
	private final String instructions_path;
	private Map<String, Operand> namesForOperands = new HashMap<String, Operand>();
	private Map<Operand, String> virtuals = new HashMap<Operand, String>();
	private Map<Operand, String> prephysicals = new HashMap<Operand, String>();
	private Map<Operand, String> physicals = new HashMap<Operand, String>();

	
	public Runner( String load_path, String jrubyHome, String instructions_path )
	{
		this.load_path = load_path;
		this.jrubyHome = jrubyHome;
		this.instructions_path = instructions_path;
        System.setProperty("jruby.base", jrubyHome);
        System.setProperty("jruby.home", jrubyHome);
        System.setProperty("jruby.lib", jrubyHome + "\\lib");
        System.setProperty("jruby.shell", "cmd.exe");
        System.setProperty("jruby.script", "jruby.bat");
	}
	
	/**
	 * Процедура получения значений операндов и инициализации
	 * с использованием разрешения ограничений
	 * 
	 * @param template	тестовый шаблон 
	 * @param state		начальное состояние модели микропропроцессора (только Комдив64!)
	 * @throws Unsat	данный тестовый шаблон является несовместным
	 * @throws Timeout	превышен допустимый на разрешение ограничений интервал времени
	 * @return программа инициализации MMU (без регистров!)
	 */
	public Program run( Program template, KMD64 state ) throws Unsat, Timeout
	{
		// проверить, что все ситуации isConstraint()
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			if ( template.getInstruction(i).getSituation().isConstructed() )
				return null;
		}
		
		//TODO проверить, что это короткий шаблон
		
		//TODO в xml-описании ситуаций обязательно сейчас должны быть описаны id'ы virtual, physical и prephysical
			
		// вызвать ruby-скрипт (он вызывает Z3)
		ScriptEngineManager m = new ScriptEngineManager();
		ScriptEngine rubyEngine = m.getEngineByName("jruby");
		System.setProperty("org.jruby.embed.class.path", jrubyHome);
		ScriptContext context = rubyEngine.getContext();
		try{
			rubyEngine.eval("require 'rubygems'", context);
			rubyEngine.eval("gem 'jrexml'", context);
			rubyEngine.eval("$LOAD_PATH << '" + load_path + "'" , context);

			rubyEngine.eval( "$L1ASSOC = " + state.getL1DCache().getSectionNumber() , context);
			rubyEngine.eval( "$TLBASSOC = " + TLB.DTLB_SIZE, context );
			rubyEngine.eval( "$TAGSETLEN = 31", context );
			rubyEngine.eval( "$PFNLEN = 24", context );
			rubyEngine.eval( "$TAGLEN = 24", context );
			rubyEngine.eval( "$SEGBITS = 40", context );
			rubyEngine.eval( "$PABITS = 36", context );
			rubyEngine.eval( "$MASK = 0", context ); // поле $MASK задает часть тегсета, соответствующая размеру страницы памяти (выбирается ОС)

			rubyEngine.eval( "require 'tesla-mips'" , context );

			rubyEngine.eval( "$instructionsPath = '" + instructions_path + "'", context );
			
			//TODO идеальный вариант:
			//	1. попробовать сгенерировать без инициализации; если получилось, ок
			//	2. попробовать сгенерировать с максимальной инициализацией; если не получилось, ок
			//	3. посчитать количество RowEqual и от него плясать при предложении минимальной initlength, а далее дихотомией искать вариант с минимальной инициализацией
			// реальный вариант:
			//	1. попробовать сгенерировать без инициализации; если получилось, ок
			//	2. попробовать сгенерировать с максимальной инициализацией

			//!!! проверить, что есть только Cached><Mapped -> тогда можно использовать CombinedSolver
			
			System.out.println("template:");
			System.out.println(getXML(template));
			System.out.println();
			
			System.out.println("data:");
			System.out.println(getXML(state));
			System.out.println();
			
			
			//TODO set $initlength 
			//TODO set $initlength_mtlb = (M > 0 ? w+1 : n)
//			rubyEngine.eval( "$initlength = 20", context );
//			rubyEngine.eval( "$initlength_mtlb = 20", context );			
//			rubyEngine.eval(
//					"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
//						getXML(template) + "\", \"" + d + "\")", context );
			
			int initlength = 0;
			int initlength_mtlb = 0;
			Object o = rubyEngine.eval(
					"Runner.new.run( MIPS_CombinedSolver.new, 0, \"" + 
						getXML(template) + "\", \"" + getXML(state) + "\")", context );
			
			if ( o instanceof String )
			{
				if ( o == "unsat" )
					throw new Unsat();
				else if ( o == "timeout" )
					throw new Timeout();
				else
					throw new IllegalArgumentException("unknown result '" + o + "'");
			}
			if ( o instanceof RubyHash )
			{
				RubyHash hash = (RubyHash)o;
				for( String name : namesForOperands.keySet() )
				{
					namesForOperands.get(name).setNumber(  Integer.parseInt( (String)hash.get( name + "_X" ) ) );
				}
				
				//составить программу инициализации
				Program init = new Program();
				if ( initlength > 0 )
				{
					//1. перевести микропроцессор в нужное состояние флагов
					init.append( new SetKSeg0CachePolicyProgram(state, state.getGPR(0), state.getGPR(1), 3) );
				}
				//кэш-память L1
				for( int i = 0; i < initlength; i++ )
				{
					if ( ! hash.containsKey("_its" + i) )
						throw new IllegalStateException("unknown key '_its" + i + "'");
					
					int tagset = Integer.parseInt( (String)hash.get("_its" + i) );
					//2. сформировать подходящий виртуальный адрес
					long vAddr = 19<<59 + tagset<<5;
					//3. обратиться по виртуальному адресу (Load в некоторый неиспользуемый регистр)
					init.append(new LoadImmediate64Program(state, state.getGPR(0), vAddr) );
					init.append(new LDInstruction(state.getGPR(0), (short)0, state.getGPR(0)) );
				}
				//инициализация TLB
				//TODO некоторые строки могут быть задействованы только в инициализации... - их тоже надо тут определить
				List<TLBline> lines = new ArrayList<TLBline>();
				for( int i = 0; i < template.countInstruction(); i++ )
				{
					Instruction instr = template.getInstruction(i);
					if ( !(instr instanceof MemoryInstruction) ) continue;
					MemoryInstruction minstr = (MemoryInstruction)instr;
					Operand arg = minstr.getOperand("base");
					String phys = prephysicals.get(arg); //TODO есть только для Cached-инструкций
					String virt = virtuals.get(arg);	//TODO есть только для Mapped-инструкций
					if (! hash.containsKey(phys) )
						throw new IllegalStateException(phys);
					if (! hash.containsKey(virt) )
						throw new IllegalStateException(virt);
					long pAddr = Long.parseLong((String)hash.get(phys));
					long vAddr = Long.parseLong((String)hash.get(virt));
					
					int r = (int)(vAddr >> 62); //TODO эти константы жестко завязаны на конкретное расположение полей в виртуальном адресе
					long vpnd2 = (vAddr >> 13) % (2^27);
					byte oddbit = (byte)((vAddr >> 12) % 2);
					long pfn = pAddr >> 12;
					
					TLBline l = TLBline.lookup(lines, r, vpnd2);
					if (l == null)
					{
						l = new TLBline();
						l.r = r;
						l.vpnd2 = vpnd2;
						switch(oddbit)
						{
							case 0: l.pfn0 = pfn; break;
							case 1: l.pfn1 = pfn; break;
							default: throw new IllegalArgumentException("oddbit must be 0 or 1");
						}
						lines.add(l);
					}
					else
					{
						switch(oddbit)
						{
							case 0: l.pfn0 = pfn; break;
							case 1: l.pfn1 = pfn; break;
							default: throw new IllegalArgumentException("oddbit must be 0 or 1");
						}						
					}
				}
				int linenumber = 0;
				for( TLBline line : lines )
				{
					Entry entry = new Entry(); //TODO fill `entry'
					init.append( new WriteTLBEntryProgram(state, entry, (linenumber++), state.getGPR(0)) );
				}
				//инициализация microTLB
				for( int i = 0; i < initlength_mtlb; i++ )
				{
					if ( ! hash.containsKey("_ivpnd" + i) )
						throw new IllegalStateException("unknown key '_ivpnd" + i + "'");
					if ( ! hash.containsKey("_ir" + i) )
						throw new IllegalStateException("unknown key '_ir" + i + "'");
					//TODO добавить _ir в tesla-mips
					
					int vpnd2 = Integer.parseInt( (String)hash.get("_ivpnd" + i) );
					int r = Integer.parseInt((String)hash.get("_ir" + i) );
					//2. сформировать подходящий виртуальный адрес
					long vAddr = r<<62 + vpnd2 << 13;
					//3. обратиться по виртуальному адресу (Load в некоторый неиспользуемый регистр)
					init.append(new LoadImmediate64Program(state, state.getGPR(0), vAddr) );
					init.append(new LDInstruction(state.getGPR(0), (short)0, state.getGPR(0)) );
				}
				// инициализируем основную память
				Map<Long, Long> memory_cells = new HashMap<Long, Long>();
				for( int i = 0; i < template.countInstruction(); i++ )
				{
					Instruction instr = template.getInstruction(i);
					if ( ! (instr instanceof MemoryInstruction) ) continue;
					MemoryInstruction minstr = (MemoryInstruction)instr;
					if ( ! minstr.isLoad() ) continue;
					
					if ( ! physicals.containsKey( minstr.getOperand("base") ) )
						throw new IllegalStateException();
					if ( ! hash.containsKey( physicals.get(minstr.getOperand("base")) ))
						throw new IllegalStateException( physicals.get(minstr.getOperand("base")) );
					Long pAddr = Long.parseLong(((String)hash.get(physicals.get(minstr.getOperand("base")))));
					
					if ( ! memory_cells.containsKey(pAddr) )
					{
						memory_cells.put(pAddr,  0l ); //TODO значение регистра rt данной инструкции (в данный момент!)
					}
				}
				//1. перевести микропроцессор в нужное состояние флагов: Unmapped
				init.append( new SetKSeg0CachePolicyProgram(state, state.getGPR(0), state.getGPR(1), 2) );
				for( long pAddr : memory_cells.keySet() )
				{
					//2. сформировать подходящий виртуальный адрес
					long vAddr = 18<<59 + pAddr;
					//3. загрузить значение в нужный регистр
					init.append(new LoadImmediate64Program(state, state.getGPR(1), memory_cells.get(pAddr)) );
					//4. обратиться по виртуальному адресу
					init.append(new LoadImmediate64Program(state, state.getGPR(0), vAddr) );
					init.append(new SDInstruction(state.getGPR(1), (short)0, state.getGPR(0)) );					
				}
				
				return init;
			}
			else
				throw new IllegalArgumentException("unrecognized return from ruby");
			
		} catch (ScriptException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public static void main(String[] args )
	{
		KMD64 k = new KMD64();
		for( int s = 0; s < k.getL1DCache().getSectionNumber(); s++ )
		{
			for( int r = 0; r < k.getL1DCache().getRowNumber(); r++ )
			{
				k.getL1DCache().setTag(s, r, new Random().nextInt(2^24));				
			}
		}
		for( int ln = 0; ln < TLB48.JTLB_SIZE; ln++ )
		{
			k.getTLB().writeEntry(new Entry(
					new Random().nextInt(2^27), false, 
					true, true, 3, new Random().nextInt(2^24), 
					true, true, 3, new Random().nextInt(2^24)), ln);
		}
		for( int i = 0; i < TLB48.DTLB_SIZE; i++ )
		{
			((TLB48)k.getTLB()).setDTLBIndex(i, i);
		}
		
		Program p = new Program();
		
		LWInstruction i1 = new LWInstruction();
		i1.getOperand("rt").setRegister(k.getGPR(0));
		i1.getOperand("base").setRegister(k.getGPR(1));
		i1.setSituation(new LWSituation());
		p.append( i1 );
		
		SBInstruction i2 = new SBInstruction();
		i2.getOperand("rt").setRegister(k.getGPR(2));
		i2.getOperand("base").setRegister(k.getGPR(0));
		i2.setSituation(new SBSituation());
		PADependency d = new PADependency(k, i1.getOperand("base"));
		d.init();
		i2.getOperand("base").registerBackwardDependency(d);
		p.append( i2 );
		
		try
		{
		new Runner(
//				 	"C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa-XP\\src\\ruby"
//					"C:\\Documents and Settings\\kornevgen2\\My Documents\\dissertation\\implementation\\TeSLa-XP\\src\\ruby"
					"/home/kornevgen/workspace/TeSLa-XP/src/ruby"				
//				, 	"C:\\Program Files\\jruby-1.4.0"
				,	"/usr/lib/jruby1.2"
//				,	"C:/Documents and Settings/kornevgen/Desktop/tesla.2008.09.24/TeSLa-XP/test/k64/"
//				,	"C:/Documents and Settings/kornevgen2/My Documents/dissertation/implementation/TeSLa-XP/test/k64/"
				, 	"/home/kornevgen/workspace/TeSLa-XP/test/k64/"
			).run(p, k);
		}
		catch( Unsat e ) {} catch( Timeout e ) {}
	}
	
	private String getXML( Program template )
	{
		namesForOperands.clear();
		
		StringBuffer xml = new StringBuffer("<template>");
		
		Set<Register> viewedRegs = new HashSet<Register>();
		Map<Operand, String> names = new HashMap<Operand, String>();
		
		//инструкции и допущения
		int number = 0;
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			Instruction instr = template.getInstruction(i);
			StringBuffer args = new StringBuffer();
			
			// добавить новые аргументы
			for( int j = 0; j < instr.countOperand(); j++ )
			{
				Operand arg = instr.getOperand(j);
				if ( arg.isRegister() )
				{
					if ( !(arg.getRegister() instanceof GPR) )
						throw new IllegalArgumentException("only GPR is supported");
					
					GPR r = (GPR)arg.getRegister();
					if ( ! viewedRegs.contains(r) )
					{
						String name = "reg" + r.getNumber();
						int length;// = arg.getContentType().getWidth();						
						if ( arg.getRegister() instanceof Register32 )
							length = 32;
						else if ( arg.getRegister() instanceof Register64 )
							length = 64;
						else
							throw new IllegalArgumentException("only 32- and 64- bits registers are supported");
						
						xml.append("<register name='" + name + "' length='" + length + "'/>");
						viewedRegs.add(r);
						names.put(arg, name);
						args.append("<argument name='" + name + "'/>");
						namesForOperands.put(name, arg);
					}
					else
					{
						String name = null;
						for( Operand o : names.keySet() )
						{
							if (o.getRegister() == r)
								name = names.get(o);
						}
						if ( name == null )
							throw new IllegalStateException();
						names.put(arg, name);
						args.append("<argument name='" + name + "'/>");
						namesForOperands.put(name, arg);
					}
				}
				else if ( arg.isImmediate() )
				{
					String name = "of" + (number++);
					int length = arg.getContentType().getWidth(); 
					xml.append("<constant name='" + name + "' length='" + length + "'/>");
					names.put(arg, name);
					args.append("<argument name='" + name + "'/>");
					namesForOperands.put(name, arg);
				}
				else
					throw new IllegalArgumentException();				
			}
			
			//translate instruction
			xml.append("<instruction name='" + instr.getName() + "'>" ).append(args);
			xml.append("<situation><branch name='" + instr.getSituation().getName() + "'/>");
			if ( instr.getSituation() instanceof MemorySituation )
			{
				MemorySituation situation = (MemorySituation)instr.getSituation();
				xml.append("<access>");
				xml.append("<cache level='1' type='DATA' id='" + (situation.l1Hit ? "l1Hit" : "l1Miss") + "' />");
				xml.append("<microtlb type='DATA' id='" + (situation.mtlbHit ? "mtlbHit" : "mtlbMiss") + "' />");
				xml.append("</access>");
			}
			xml.append("</situation>");
			if ( instr.getSituation() instanceof MemorySituation )
			{			
				String virtual = "virt" + (number++);
				String prephysical = "prephys" + (number++);
				String physical = "phys" + (number++);
				virtuals.put(instr.getOperand("base"), virtual);
				prephysicals.put(instr.getOperand("base"), prephysical);
				physicals.put(instr.getOperand("base"), physical);
				xml.append("<external name='" + virtual + "' id='virtual'/>");
				xml.append("<external name='" + prephysical + "' id='prephysical'/>");
				xml.append("<external name='" + physical + "' id='physical'/>");
			}
			xml.append("</instruction>");
			
			// translate dependencies as assumes
			/**виды зависимостей:
			 * 1) по виртуальным адресам
			 * 2) по физическим адресам
			 * 3) на сам адрес (на строку кэша или TLB)
			 * 4) произвольные ограничения на параметры инструкций и строки
			 */
			//dependencies: у операндов
			if ( instr.getOperand("base") != null )
			{
				Dependencies deps = instr.getOperand("base").getBackwardDependencies();
				for( Dependency dep : deps )
				{
					if ( dep instanceof PADependency )
					{
						PADependency padep = (PADependency)dep;
						for (String depName : Arrays.asList(
								(padep.l1RowEqual?"l1RowEqual":"l1RowNotEqual"),
								(padep.l1TagEqual?"l1TagEqual":"l1TagNotEqual")
							) )
						{
							xml.append("<assume name='" + depName + "'>");
							xml.append("<argument name='" + physicals.get(instr.getOperand("base")) + "'/>");
							xml.append("<argument name='" + physicals.get(padep.getDeterminantOperand()) + "'/>");
							xml.append("</assume>");
						}
					}
					else if ( dep instanceof VADependency )
					{
						VADependency vadep = (VADependency)dep;
						for (String depName : Arrays.asList(
								(vadep.tlbEntryEqual?"tlbEntryEqual":"l1EntryNotEqual")
							) )
						{
							xml.append("<assume name='" + depName + "'>");
							xml.append("<argument name='" + virtuals.get(instr.getOperand("base")) + "'/>");
							xml.append("<argument name='" + virtuals.get(vadep.getDeterminantOperand()) + "'/>");
							xml.append("</assume>");
						}
					}
				}
			}
		}
		
		return xml.append("</template>").toString();
	}

	private String getXML( KMD64 state )
	{
		StringBuffer xml = new StringBuffer("<data>");

		xml.append("<cache level='1' mode='DATA'>");
		L1Cache cache = state.getL1DCache();
		for( int row = 0; row < cache.getRowNumber(); row++ )
		{
			xml.append("<set value='" + row + "'>");
			for( int s = 0; s < cache.getSectionNumber(); s++ )
			{
				xml.append("<tag value='" + cache.getTag(s, row) + "'/>");
			}
			xml.append("</set>");
		}
		xml.append("</cache>");
		
		xml.append("<microtlb>");
		TLB48 tlb = (TLB48)state.getTLB();
		Set<Entry> microtlb = new HashSet<Entry>();
		for( int ln = 0; ln < TLB48.DTLB_SIZE; ln++ )
		{
			Entry entry = tlb.getDTLBEntry(ln);
			if (entry == null ) continue;
			xml.append("<line range='" + entry.hi.getR() + 
					"' vpndiv2='" + entry.hi.getVPN2() + 
					"' mask='" + 0 +
					//ASID is not processed yet
					"' pfn0='" + entry.lo0.getPFN() +
					"' CCA0='" + getCCA(entry.lo0.getC()) +
					"' pfn1='" + entry.lo1.getPFN() +
					"' CCA1='" + getCCA(entry.lo1.getC()) + "'/>" );
			microtlb.add(entry);
		}
		xml.append("</microtlb>");
		
		xml.append("<tlb>");
		for( int ln = 0; ln < TLB48.JTLB_SIZE; ln++ )
		{
			Entry entry = tlb.getEntry(ln);
			if ( entry == null ) continue;
			if ( microtlb.contains(entry) ) continue;
			xml.append("<line range='" + entry.hi.getR() + 
					"' vpndiv2='" + entry.hi.getVPN2() + 
					"' mask='" + 0 + 
					//ASID is not processed yet
					"' pfn0='" + entry.lo0.getPFN() +
					"' CCA0='" + getCCA(entry.lo0.getC()) +
					"' pfn1='" + entry.lo1.getPFN() +
					"' CCA1='" + getCCA(entry.lo1.getC()) + "'/>" );
		}
		xml.append("</tlb>");
		
		return xml.append("</data>").toString();
	}
	
	private String getCCA( int attribute )
	{
		switch(attribute)
		{
		case 2:
			return "Uncached";
		case 3:
			return "Cached";
		default:
			throw new IllegalArgumentException("unknown CCA attribute '" + attribute + "'");
		}
	}
}

class TLBline
{
	int r;
	long vpnd2;
	long pfn0 = 0;
	long pfn1 = 0;
	
	public static TLBline lookup( List<TLBline> lines, int r, long vpnd2 )
	{
		for( TLBline line : lines )
		{
			if (line.r == r && line.vpnd2 == vpnd2)
				return line;
		}
		return null;
	}
}
