package com.unitesk.testfusion.core.tesla;

import java.util.Collections;

import com.unitesk.kmd64.model.KMD64;
import com.unitesk.testfusion.core.model.Program;

public class Runner
{
	//TODO ���������� ��������� �������������
	public void run( Program template, KMD64 state )
	{
		// ����������� ������� ��������� ��� ruby-�������
		
		// ������� ruby-������ (�� �������� Z3)
		org.jruby.Ruby r = org.jruby.javasupport.JavaEmbedUtils.initialize(Collections.EMPTY_LIST);
		r.evalScriptlet("puts 'hhh'");
		// http://www.docjar.com/docs/api/org/jruby/Ruby.html
		
		// ��������� ��������� ������ ������� � ��������� ��������� �������������
	}
	
	public static void main(String[] args )
	{
		new Runner().run(null, null);
	}
}
