package ru.teslaprj;

import java.math.BigInteger;
import java.util.Map;

public interface TextConstructor
{

	Program build_initialization(
			Map<Parameter, BigInteger> solve,
			Map<Table, Integer> initlengths,
			Template template );

	Program createEmptyProgram();
}
