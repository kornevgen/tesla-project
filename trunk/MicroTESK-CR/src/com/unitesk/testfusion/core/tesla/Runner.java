package com.unitesk.testfusion.core.tesla;

import java.io.File;
import java.math.BigInteger;
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
import com.unitesk.kmd64.model.KMD64Context;
import com.unitesk.kmd64.model.KMD64OperandType;
import com.unitesk.kmd64.model.L1Cache;
import com.unitesk.kmd64.model.TLB;
import com.unitesk.kmd64.model.TLB48;
import com.unitesk.kmd64.model.TLB64;
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
import com.unitesk.testfusion.core.model.ContentType;
import com.unitesk.testfusion.core.model.Instruction;
import com.unitesk.testfusion.core.model.Operand;
import com.unitesk.testfusion.core.model.Program;
import com.unitesk.testfusion.core.model.register.Register;
import com.unitesk.testfusion.core.model.register.Register32;
import com.unitesk.testfusion.core.model.register.Register64;
import com.unitesk.testfusion.core.situation.Situation;


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
		for( String path : Arrays.asList(load_path, jrubyHome, instructions_path) )
		{
			if (! new File(path).exists() )
				throw new IllegalArgumentException("Path '" + path + "' doesn't exist");
		}
		
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
	 * @return программа инициализации MMU (без регистров!) и null, если построение невозможно
	 */
	public Program run( Program template, KMD64 state, KMD64Context kcontext ) throws Unsat, Timeout
	{
		// проверить, что все ситуации isConstraint()
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			Instruction instr = template.getInstruction(i);
			if ( instr.getSituation().isConstructed() )
				return null;
//			for( int j = 0; j < instr.countOperand(); j++)
//			{
//				for( Dependency dep : instr.getOperand(j).getForwardDependencies() )
//				{
//					if ( dep //TODO где тут isConstructed ?
//				}
//			}
		}
		
		// проверить, что это короткий шаблон
		int n = 0;
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			if (template.getInstruction(i).getSituation() instanceof MemorySituation )
				n++;
		}
		if ( n > L1Cache.SECTION_NUMBER )
			throw new IllegalArgumentException("only `short' templates are supported now");
		
		//в xml-описании ситуаций обязательно сейчас должны быть описаны
		// id'ы virtual, physical и prephysical  --> помещено в ruby-код
			
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
			
			
			String template_xml = getXML(template);
			String state_xml = getXML(state);
//			System.out.println("template:");
//			System.out.println(template_xml);
//			System.out.println();			
//			System.out.println("data:");
//			System.out.println(state_xml);
//			System.out.println();
			
			Combined:
			{
				for( int i = 0; i < template.countInstruction(); i++ )
				{
					//if instruction is not Cached >< Mapped, ommit combined solver
					Situation s = template.getInstruction(i).getSituation(); 
					if (!( s instanceof MemorySituation) ) continue;
					MemorySituation ms = (MemorySituation)s;
					if ( ms.addressError || !ms.isMapped || !ms.isCached )
						break Combined;
				}
				
				// проверить отсутствие invalid-тегов
				for( int s = 0; s < L1Cache.SECTION_NUMBER; s++ )
				for( int r = 0; r < L1Cache.ROW_NUMBER; r++ )
				{
					if ( state.getL1DCache().getTag(s, r) == L1Cache.INVALID_TAG )
						break Combined;
				}
				for( int i = 0; i < TLB.DTLB_SIZE; i++ )
				{
					if ( state.getTLB().getEntry(i).lo0.getV() == 0 ||
							state.getTLB().getEntry(i).lo1.getV() == 0 )
						break Combined;
				}

				Object o = rubyEngine.eval(
						"Runner.new.run( MIPS_CombinedSolver.new, 0, \"" + 
							template_xml + "\", \"" + state_xml + "\")", context );
				
				if ( o instanceof RubyHash )
				{
					return buildInitialization(template, state, 0, 0, (RubyHash)o, kcontext);
				}
			}
			
			int max = calculateMaxInitlength(template);
			int min = calculateMinInitlength(template);

			int initlength_mtlb = calculateInitlength_mtlb(template);
			rubyEngine.eval( "$initlength_mtlb = " + initlength_mtlb, context );			
			
			//сначала проверить на максимуме
			rubyEngine.eval( "$initlength = " + max, context );
			Object o = rubyEngine.eval(
						"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
							template_xml + "\", \"" + state_xml + "\")", context );
			if ( o == "unsat" )
				throw new Unsat();
			if ( o == "timeout" )
				throw new Timeout();
			
			Program init = null;
			while (max >= min)
			{
			  int initlength = (max + min)/2;
			  rubyEngine.eval( "$initlength = " + initlength, context );
			  o = rubyEngine.eval(
						"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
							template_xml + "\", \"" + state_xml + "\")", context );
			  
			  if (o == "timeout" || o == "unsat" )
			    min = initlength + 1;
			  else
			  {
			    max = initlength - 1;
			    init = buildInitialization(template, state, initlength,
						initlength_mtlb, (RubyHash)o, kcontext);
			  }
			}
			
			return init;
			
		} catch (ScriptException e) {
			e.printStackTrace();
			return null;
		}
	}

	private int calculateMinInitlength(Program template)
	{
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			Situation s = template.getInstruction(i).getSituation();
			if (! ( s instanceof MemorySituation) ) continue;
			MemorySituation ms = (MemorySituation)s;
			if ( ! ms.l1Hit )
				return 1 + L1Cache.SECTION_NUMBER;
		}
		return 1;
	}

	private int calculateMaxInitlength(Program template)
	{
		Set<Set<Instruction>> classes = new HashSet<Set<Instruction>>();
		Set<Instruction> viewed = new HashSet<Instruction>();
		
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			Instruction instr = template.getInstruction(i);
			if (! ( instr instanceof MemoryInstruction) ) continue;
			if ( viewed.contains( instr ) ) continue;
			
			Dependencies deps = instr.getOperand("base").getForwardDependencies();
			Set<Instruction> theclass = new HashSet<Instruction>();
			theclass.add( instr );
			viewed.add( instr );
			for( Dependency dep : deps )
			{
				if ( !( dep instanceof PADependency ) ) continue;
				PADependency pdep = (PADependency)dep;
				if ( ! pdep.l1RowEqual ) continue;
				theclass.add( pdep.getDependentInstruction() );
				viewed.add( pdep.getDependentInstruction() );
			}
			classes.add( theclass );
		}
		
		int max = 0;
		for( Set<Instruction> theclass : classes )
		{
			max += initlength(theclass);
		}
		
		return max;
	}
	
	private int initlength(Set<Instruction> theclass )
	{
		for( Instruction instr : theclass )
		{
			if (! ((MemorySituation)instr.getSituation()).l1Hit)
				return L1Cache.SECTION_NUMBER + 1;
		}
		
		return theclass.size();
	}

	/**
	 * функция вычисления количества инициализирующих тегов
	 * 
	 * @param template
	 * @return (M > 0 ? w+1 : N)
	 */
	private int calculateInitlength_mtlb(Program template)
	{
		int N = 0; // количество инструкций с виртуальными адресами 
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			Instruction instr = template.getInstruction(i);
			if ( instr instanceof MemoryInstruction )
			{
				N++;
				MemoryInstruction minstr = (MemoryInstruction)instr;
				if (! ((MemorySituation)minstr.getSituation()).mtlbHit)
					return TLB64.MTLB_SIZE + 1;
			}
		}
		
		return N;
	}

	private Program buildInitialization(
			Program template, KMD64 state,
			int initlength, int initlength_mtlb,
			RubyHash hash, KMD64Context context)
	{
		for( String name : namesForOperands.keySet() )
		{
			ContentType type = namesForOperands.get(name).getContentType();
			String value = (String)hash.get( name + "_X" );
			if ( type == ContentType.WORD )
			{
				namesForOperands.get(name).setIntegerValue( (int)parseUnsignedLong(value, 32));
			}
			else if ( type == ContentType.DOUBLE_WORD || type == ContentType.DATA_ADDRESS )
			{
				namesForOperands.get(name).setLongValue( parseUnsignedLong(value, 64) );
			}
			else if ( type == ContentType.OFFSET )
			{
				//value представляет собой беззнаковую строку, а short в Java знаковый...
				namesForOperands.get(name).setShortValue( (short)parseUnsignedLong(value, 16));
			}
			else if ( type == ContentType.BYTE )
			{
				namesForOperands.get(name).setByteValue( (byte)parseUnsignedLong(value, 8));
			}
			else
				throw new IllegalArgumentException("unexpected content type");
		}
		
		// взять неиспользуемые регистры
		GPR temp1 = (GPR)context.getRegister( KMD64OperandType.GPR_REGISTER );
		GPR temp2 = (GPR)context.getRegister( KMD64OperandType.GPR_REGISTER );
		
		//составить программу инициализации
		Program init = new Program();
		if ( initlength > 0 )
		{
			//1. перевести микропроцессор в нужное состояние флагов
			init.append( new SetKSeg0CachePolicyProgram(state, temp1, temp2, 3) );
			//кэш-память L1
			for( int i = 1; i <= initlength; i++ )
			{
				if ( ! hash.containsKey("_its" + i) )
					throw new IllegalStateException("unknown key '_its" + i + "'");
				
				int tagset = Integer.parseInt( (String)hash.get("_its" + i) );
				//2. сформировать подходящий виртуальный адрес
				long vAddr = 19<<59 + tagset<<5;
				//3. обратиться по виртуальному адресу (Load в некоторый неиспользуемый регистр)
				init.append(new LoadImmediate64Program(state, temp1, vAddr) );
				init.append(new LDInstruction(temp1, (short)0, temp1) );
			}
			//TODO восстановить KSeg0 (или не восстанавливать, если он потом понадобится! но в мультипроцессном режиме кто-то может KSeg0 изменить, а мы об этом не узнаем)
		}
		//инициализация TLB
		if ( initlength_mtlb > 0 )
		{
			//TODO ошибка: если убрать if (initlength_mtlb > 0), то при
			// initlength_mtlb == 0 этот код будет исполняться
			// и даже даст некую инициализацию, хотя не должен бы!
			List<TLBline> lines = new ArrayList<TLBline>();
			for( int i = 0; i < template.countInstruction(); i++ )
			{
				Instruction instr = template.getInstruction(i);
				if ( !(instr instanceof MemoryInstruction) ) continue;
				MemoryInstruction minstr = (MemoryInstruction)instr;
				Operand arg = minstr.getOperand("base");
				String phys = prephysicals.get(arg); 
				String virt = virtuals.get(arg);	
				if (! hash.containsKey(phys) )
					throw new IllegalStateException(phys);
				if (! hash.containsKey(virt) )
					throw new IllegalStateException(virt);
				long pAddr = Long.parseLong((String)hash.get(phys));
				long vAddr = Long.parseLong((String)hash.get(virt));
				
				int r = (int)(vAddr >> 62);
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
			//некоторые строки могут быть задействованы только в инициализации... - их тоже надо тут определить
			for( int i = initlength+1; i <= initlength_mtlb+initlength; i++ )
			{
				if ( ! hash.containsKey("_ivpnd" + i) )
					throw new IllegalStateException("unknown key '_ivpnd" + i + "'");
				
				int vpnd2 = Integer.parseInt( (String)hash.get("_ivpnd" + i) );
				
				if (TLBline.lookup(lines, 0, vpnd2) == null)
				{
					TLBline l = new TLBline();
					l.r = 0;
					l.vpnd2 = vpnd2;
					lines.add(l);
				}
			}
			
			int linenumber = 0;
			for( TLBline line : lines )
			{
				Entry entry = new Entry(
						(int)line.vpnd2, // vpn/2
						false, // g
			            true, true, 3, (int)line.pfn0,// v0, d0, c0, pfn0,
			            true, true, 3, (int)line.pfn1 // v1, d1, c1, pfn1
					);
				init.append( new WriteTLBEntryProgram(state, entry, (linenumber++), temp1) );
			}
			//инициализация microTLB
			for( int i = initlength+1; i < initlength+initlength_mtlb; i++ )
			{
				if ( ! hash.containsKey("_ivpnd" + i) )
					throw new IllegalStateException("unknown key '_ivpnd" + i + "'");
				
				int vpnd2 = Integer.parseInt( (String)hash.get("_ivpnd" + i) );
				int r = 0;
				//2. сформировать подходящий виртуальный адрес
				long vAddr = r<<62 + vpnd2 << 13;
				//3. обратиться по виртуальному адресу (Load в некоторый неиспользуемый регистр)
				init.append(new LoadImmediate64Program(state, temp1, vAddr) );
				init.append(new LDInstruction(temp1, (short)0, temp1) );
			}
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
			
			if (! hash.containsKey("valueof-"+physicals.get(minstr.getOperand("base"))) )
				throw new IllegalStateException( "valueof-"+physicals.get(minstr.getOperand("base")) );
			
			if ( ! memory_cells.containsKey(pAddr) )
			{
				//значение регистра rt данной инструкции (в данный момент!)
				memory_cells.put(pAddr, parseUnsignedLong((String)hash.get("valueof-"+physicals.get(minstr.getOperand("base"))), 64));
			}
		}
		//1. перевести микропроцессор в нужное состояние флагов: Unmapped
		if ( ! memory_cells.isEmpty() )
		{
			init.append( new SetKSeg0CachePolicyProgram(state, temp1, temp2, 2) );
			for( long pAddr : memory_cells.keySet() )
			{
				//2. сформировать подходящий виртуальный адрес
				long vAddr = 18<<59 + pAddr;
				//3. загрузить значение в нужный регистр
				init.append(new LoadImmediate64Program(state, temp1, memory_cells.get(pAddr)) );
				//4. обратиться по виртуальному адресу
				init.append(new LoadImmediate64Program(state, temp2, vAddr) );
				init.append(new SDInstruction(temp1, (short)0, temp2) );					
			}
			//TODO восстановить значение KSeg0
		}
		
		return init;
	}
	
	private long parseUnsignedLong(String value, int width)
	{
		BigInteger bint = new BigInteger(value);
		if ( width < 64 && bint.compareTo(new BigInteger("2").pow(width)) >= 0 )
			throw new NumberFormatException();
		
		if ( bint.compareTo(new BigInteger("2").pow(width-1)) < 0 )
			return bint.longValue();
		else
			return bint.subtract(new BigInteger("2").pow(width)).longValue();
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
		MemorySituation ms1 = new LWSituation();
		ms1.addressError = false;
		ms1.isMapped = true;
		ms1.isCached = true;
		i1.setSituation(ms1);
		p.append( i1 );
		
		SBInstruction i2 = new SBInstruction();
		i2.getOperand("rt").setRegister(k.getGPR(2));
		i2.getOperand("base").setRegister(k.getGPR(0));
		PADependency d = new PADependency(k, i1.getOperand("base"));
		d.init();
		i2.getOperand("base").registerBackwardDependency(d);
		MemorySituation ms2 = new SBSituation();
		ms2.addressError = false;
		ms2.isMapped = true;
		ms2.isCached = true;
		i2.setSituation(ms2);
		p.append( i2 );
		
//		SBInstruction i3 = new SBInstruction();
//		i3.getOperand("rt").setRegister(k.getGPR(2));
//		i3.getOperand("base").setRegister(k.getGPR(0));
//		i3.setSituation(new SBSituation());
//		p.append( i3 );
		
		KMD64Context c = new KMD64Context(k);
		c.reset();
		
		try
		{
			Program init = new Runner(
				 	"C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa-XP\\src\\ruby"
//					"C:\\Documents and Settings\\kornevgen2\\My Documents\\dissertation\\implementation\\TeSLa-XP\\src\\ruby"
//					"/home/kornevgen/workspace/TeSLa-XP/src/ruby"				
				, 	"C:\\Program Files\\jruby-1.4.0"
//				,	"/usr/lib/jruby1.2"
				,	"C:/Documents and Settings/kornevgen/Desktop/tesla.2008.09.24/TeSLa-XP/test/k64/"
//				,	"C:/Documents and Settings/kornevgen2/My Documents/dissertation/implementation/TeSLa-XP/test/k64/"
//				, 	"/home/kornevgen/workspace/TeSLa-XP/test/k64/"
			).run(p, k, c );
			System.out.println( init.toString() );
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
