package com.unitesk.testfusion.core.tesla;

import java.util.HashSet;
import java.util.Set;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.unitesk.kmd64.model.GPR;
import com.unitesk.kmd64.model.KMD64;
import com.unitesk.kmd64.model.TLB;
import com.unitesk.kmd64.model.isa.memory.LWInstruction;
import com.unitesk.kmd64.model.isa.memory.SBInstruction;
import com.unitesk.testfusion.core.context.GeneratorContext;
import com.unitesk.testfusion.core.model.Instruction;
import com.unitesk.testfusion.core.model.Operand;
import com.unitesk.testfusion.core.model.Program;
import com.unitesk.testfusion.core.model.register.Register;
import com.unitesk.testfusion.core.model.register.Register32;
import com.unitesk.testfusion.core.model.register.Register64;
import com.unitesk.testfusion.core.type.OffsetType;


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
	public void run( Program template, KMD64 state, GeneratorContext genContext )
	{
		// TODO подготовить входные параметры для ruby-скрипта
		
		// TODO вызвать ruby-скрипт (он вызывает Z3)
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

			rubyEngine.eval("require 'tesla-mips'" , context);

			rubyEngine.eval( "$instructionsPath = '" + instructions_path + "'", context );

			//TODO set $initlength and $initlength_mtlb
			rubyEngine.eval( "$initlength = 20", context );
			rubyEngine.eval( "$initlength_mtlb = 20", context );
			
			//TODO translate to XML
//			final String t = "<template />";
			
			//TODO translate to XML
			final String d = "<data />";
			
			System.out.println("template:");
			System.out.println(getXML(template));
			
			rubyEngine.eval(
					"Runner.new.run( MIPS_FullMirrorSolver.new, 0, \"" + 
						getXML(template) + "\", \"" + d + "\")", context );
			
		} catch (ScriptException e) {
			e.printStackTrace();
		}
		
		// http://www.docjar.com/docs/api/org/jruby/Ruby.html
		
		// TODO прочитать результат работы скрипта и составить программу инициализации
	}
	
	public static void main(String[] args )
	{
		KMD64 k = new KMD64();
		Program p = new Program();

		LWInstruction i1 = new LWInstruction();
		i1.getOperand("rt").setRegister(k.getGPR(0));
		i1.getOperand("base").setRegister(k.getGPR(1));
		p.append( i1 );
		
		SBInstruction i2 = new SBInstruction();
		i2.getOperand("rt").setRegister(k.getGPR(2));
		i2.getOperand("base").setRegister(k.getGPR(0));
		p.append( i2 );
		
//		i1.getOperand("offset").setr
		
		GeneratorContext c = new GeneratorContext();
//		c.initOperandType(KMD64OperandType.GPR_REGISTER, new RegisterSet() );
//		c.useRegister( k.getGPR(0) );
//		c.useRegister( k.getGPR(1) );
//		c.useRegister( k.getGPR(2) );
		
		new Runner(
				 	"C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa-XP\\src\\ruby"
				, 	"C:\\Program Files\\jruby-1.4.0"
				,	"B:/"
			).run(p, k, c);
		
	}
	
	private String getXML( Program template )
	{
		StringBuffer xml = new StringBuffer("<template>");
		
		Set<Register> viewedRegs = new HashSet<Register>();
		
		//TODO имена придумать самому!
		
		//TODO инструкции и допущения
		for( int i = 0; i < template.countInstruction(); i++ )
		{
			Instruction instr = template.getInstruction(i);
			// добавить новые аргументы
			for( int j = 0; j < instr.countOperand(); j++ )
			{
				Operand arg = instr.getOperand(j);
				if ( arg.isRegister() )
				{
					if ( !(arg.getRegister() instanceof GPR) )
						throw new IllegalArgumentException("only GPR is supported");
					
					GPR r = (GPR)arg.getRegister();
					int length;
					if (arg.getRegister() instanceof Register32)
						length = 32;
					else if ( arg.getRegister() instanceof Register64 )
						length = 64;
					else
						throw new IllegalArgumentException("only 32-bits or 64-bits registers are supported");
					
					if ( ! viewedRegs.contains(r) )
					{
						xml.append("<register name='reg" + r.getNumber() + "' length='" + length + "'/>");
						viewedRegs.add(r);
					}					
				}
				else if ( arg.isImmediate() )
				{
					int length; //TODO = ???
					if ( ! viewedImms.contains(arg) )
					{
						xml.append("<constant name='" + arg + "' length='" + length + "'/>");
						viewedRegs.add(r);
					}
				}
				else
					throw new IllegalArgumentException();
				
			}
		}
		
		return xml.append("</template>").toString();
	}
}


