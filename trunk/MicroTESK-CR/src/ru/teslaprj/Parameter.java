package ru.teslaprj;

public interface Parameter
{
		String getName();
		
		public static enum Type { REGISTER, KEY, CONST };
		
		Type getType(); 
}
