package com.unitesk.testfusion.core.tesla;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.unitesk.kmd64.model.GPR;
import com.unitesk.kmd64.model.KMD64;
import com.unitesk.kmd64.model.L1Cache;
import com.unitesk.kmd64.model.TLB;
import com.unitesk.kmd64.model.TLB48;
import com.unitesk.kmd64.model.TLB.Entry;
import com.unitesk.kmd64.model.deps.PADependency;
import com.unitesk.kmd64.model.deps.VADependency;
import com.unitesk.kmd64.model.isa.memory.LWInstruction;
import com.unitesk.kmd64.model.isa.memory.SBInstruction;
import com.unitesk.kmd64.model.isa.memory.situation.LWSituation;
import com.unitesk.kmd64.model.isa.memory.situation.MemorySituation;
import com.unitesk.kmd64.model.isa.memory.situation.SBSituation;
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
	
	//TODO возвращать программу инициализации
	public void run( Program template, KMD64 state )
	{
		// проверить, что все ситуации isConstraint()
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			if ( template.getInstruction(i).getSituation().isConstructed() )
				return;
		}
			
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
			rubyEngine.eval( "$MASK = 0", context ); // поле $MASK задает часть тегсета, соответствующа€ размеру страницы пам€ти (выбираетс€ ќ—)

			rubyEngine.eval( "require 'tesla-mips'" , context );

			rubyEngine.eval( "$instructionsPath = '" + instructions_path + "'", context );
			
			//TODO идеальный вариант:
			//	1. попробовать сгенерировать без инициализации; если получилось, ок
			//	2. попробовать сгенерировать с максимальной инициализацией; если не получилось, ок
			//	3. посчитать количество RowEqual и от него пл€сать при предложении минимальной initlength, а далее дихотомией искать вариант с минимальной инициализацией
			// реальный вариант:
			//	1. попробовать сгенерировать без инициализации; если получилось, ок
			//	2. попробовать сгенерировать с максимальной инициализацией

			// проверить, что есть только Cached><Mapped -> тогда можно использовать CombinedSolver
			
			System.out.println("template:");
			System.out.println(getXML(template));
			System.out.println();
			
			System.out.println("data:");
			System.out.println(getXML(state));
			System.out.println();
			
			
			//TODO set $initlength and $initlength_mtlb
//			rubyEngine.eval( "$initlength = 20", context );
//			rubyEngine.eval( "$initlength_mtlb = 20", context );			
//			rubyEngine.eval(
//					"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
//						getXML(template) + "\", \"" + d + "\")", context );
			
			//TODO set $initlength and $initlength_mtlb
			Object o = rubyEngine.eval(
					"Runner.new.run( MIPS_CombinedSolver.new, 0, \"" + 
						getXML(template) + "\", \"" + getXML(state) + "\")", context );
			if ( o instanceof String )
			{
				//TODO обработать unsat и timeout
			}
			System.out.println(o);
			
		} catch (ScriptException e) {
			e.printStackTrace();
		}
		
		// TODO прочитать результат работы скрипта и составить программу инициализации
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
		
		new Runner(
//				 	"C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa-XP\\src\\ruby"
					"C:\\Documents and Settings\\kornevgen2\\My Documents\\dissertation\\implementation\\TeSLa-XP\\src\\ruby"
				, 	"C:\\Program Files\\jruby-1.4.0"
//				,	"C:/Documents and Settings/kornevgen/Desktop/tesla.2008.09.24/TeSLa-XP/test/k64/"
				,	"C:/Documents and Settings/kornevgen2/My Documents/dissertation/implementation/TeSLa-XP/test/k64/"
			).run(p, k);
		
	}
	
	private String getXML( Program template )
	{
		StringBuffer xml = new StringBuffer("<template>");
		
		Set<Register> viewedRegs = new HashSet<Register>();
		Map<Operand, String> names = new HashMap<Operand, String>();
		Map<Operand, String> virtuals = new HashMap<Operand, String>();
		Map<Operand, String> physicals = new HashMap<Operand, String>();
		
		//инструкции и допущени€
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
					}
				}
				else if ( arg.isImmediate() )
				{
					String name = "of" + (number++);
					int length = arg.getContentType().getWidth(); 
					xml.append("<constant name='" + name + "' length='" + length + "'/>");
					names.put(arg, name);
					args.append("<argument name='" + name + "'/>");
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
				String physical = "phys" + (number++);
				virtuals.put(instr.getOperand("base"), virtual);
				physicals.put(instr.getOperand("base"), physical);
				xml.append("<external name='" + virtual + "' id='virtual'/>");
				xml.append("<external name='" + physical + "' id='physical'/>");
			}
			xml.append("</instruction>");
			
			// translate dependencies as assumes
			/**виды зависимостей:
			 * 1) по виртуальным адресам
			 * 2) по физическим адресам
			 * 3) на сам адрес (на строку кэша или TLB)
			 * 4) произвольные ограничени€ на параметры инструкций и строки
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


