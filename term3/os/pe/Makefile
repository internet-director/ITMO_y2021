pe-objs += main.o

all:
	g++ main.cpp -o pe-parser
	#build parser
clean:
	rm pe-parser
	#cleaned

validation-pe-tests: all
	python3 -m tests ValidatingPeTestCases -f

import-dll-tests: all
	python3 -m tests ImportDllTestCases -f

import-function-tests: all
	python3 -m tests ImportFunctionTestCases -f

export-function-tests: all
	python3 -m tests ExportFunctionTestCases -f
