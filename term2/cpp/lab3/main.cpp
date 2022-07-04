#include <exception>
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include "phonebook.h"
#include "quicksort.h"
#include "return_codes.h"
#include <memory>

template <typename T>
int read(std::ifstream& file, std::vector<T>& vec) {
	size_t size;
	file >> size;
	try {
		vec.resize(size);
	}
	catch (std::bad_alloc& e) {
		std::cerr << "Error alloc memory!\n";
		return ERROR_MEMORY;
	}

	for (size_t i = 0; i < size; i++) {
		file >> vec[i];
	}

	return 0;
}

template <typename T>
int start(std::ifstream& in, const char* out_name, bool sort) {
	int code;
	std::vector<T> vec;
	code = read<T>(in, vec);
	in.close();

	if (code) {
		return code;
	}

	if (sort) {
		quicksort<T, true>(vec, vec.size() - 1);
	} else {
		quicksort<T, false>(vec, vec.size() - 1);
	}

	std::ofstream out(out_name);
	if (!out.is_open()) {
		std::cerr << "Can't open output file!" << std::endl;
		return ERROR_FILE_NOT_FOUND;
	}

	for (size_t i = 0; i < vec.size(); i++) {
		out << vec[i] << "\n";
	}
	out.close();
	return 0;
}

int main(int argc, char* argv[]) {
	if (argc != 3) {
		std::cerr << "Invalid arguments!\nExpected: lab3 <input file> <output file>" << std::endl;
		return ERROR_INVALID_PARAMETER;
	}

	std::ifstream input(argv[1]);
	if (!input.is_open()) {
		std::cerr << "Can't open input file!" << std::endl;
		return ERROR_FILE_NOT_FOUND;
	}

	std::string type, sortType;
	int code = 0;
	input >> type >> sortType;

	bool sort = (sortType == "descending") ? true : false;

	if (type == "int") {
		code = start<int>(input, argv[2], sort);
	}
	else if (type == "float") {
		code = start<float>(input, argv[2], sort);
	}
	else if (type == "phonebook") {
		code = start<phonebook>(input, argv[2], sort);
	}
	else {
		std::cerr << "Error invalid data type!" << std::endl;
		input.close();
		return ERROR_NOT_IMPLEMENTED;
	}
	return code;
}