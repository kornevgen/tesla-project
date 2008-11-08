package ru.teslaprj.scheme;

import java.util.List;

public interface Commandlike
{
	String getCop();
	List<String> getArgs();
	List<String> getAdditionalArgs();
	String getTestSituation();
}
