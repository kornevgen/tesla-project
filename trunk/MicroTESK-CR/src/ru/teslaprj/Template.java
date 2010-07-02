package ru.teslaprj;

import java.util.Map;
import java.util.Set;

public interface Template {

	String getXML();
	
	Set<Parameter> getParameters();

	Set<Parameter> getParameters(Map<Table, Integer> init_lengths);

}
