package ru.teslaprj;

public abstract class Parameter
{
		public abstract String getName();
		
		public static enum Type { REGISTER, KEY };
		
		public abstract Type getType(); 
}
