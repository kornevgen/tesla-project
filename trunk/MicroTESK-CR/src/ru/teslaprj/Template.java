package ru.teslaprj;

import java.util.Set;

public interface Template {

	String getXML();
	
	Set<Parameter> getParameters();

}
