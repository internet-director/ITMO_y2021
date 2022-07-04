#include "LN.h"
#include "return_codes.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stack>

LN get_one(std::stack<LN>& stack) {
	LN a = stack.top();
	stack.pop();
	return a;
}

std::pair<LN, LN> get_pair(std::stack<LN>& stack) {
	std::pair<LN, LN> pair;
	pair.first = stack.top();
	stack.pop();
	pair.second = stack.top();
	stack.pop();
	return pair;
}

int main(int argc, char* argv[]) {
	if (argc != 3)
	{
		std::cerr << "Invalid arguments!\nExpected: lab3 <input file> <output file>" << std::endl;
		return ERROR_INVALID_PARAMETER;
	}

	std::ifstream input(argv[1]);
	if (!input.is_open())
	{
		std::cerr << "Can't open input file!" << std::endl;
		return ERROR_FILE_NOT_FOUND;
	}
	std::string str;
	std::stack<LN> stack;

	while (std::getline(input, str)) {
		try {
			if (str == "+") {
				auto a = get_pair(stack);
				stack.push(a.second + a.first);
			}
			else if (str == "-") {
				auto a = get_pair(stack);
				stack.push(a.second - a.first);
			}
			else if (str == "*") {
				auto a = get_pair(stack);
				stack.push(a.second * a.first);
			}
			else if (str == "/") {
				auto a = get_pair(stack);
				stack.push(a.second / a.first);
			}
			else if (str == "%") {
				auto a = get_pair(stack);
				stack.push(a.second % a.first);
			}
			else if (str == "+=") {
				auto a = get_pair(stack);
				stack.push(a.second += a.first);
			}
			else if (str == "-=") {
				auto a = get_pair(stack);
				stack.push(a.second -= a.first);
			}
			else if (str == "*=") {
				auto a = get_pair(stack);
				stack.push(a.second *= a.first);
			}
			else if (str == "/=") {
				auto a = get_pair(stack);
				stack.push(a.second /= a.first);
			}
			else if (str == "%=") {
				auto a = get_pair(stack);
				stack.push(a.second %= a.first);
			}
			else if (str == "==") {
				auto a = get_pair(stack);
				stack.push(a.second == a.first);
			}
			else if (str == "!=") {
				auto a = get_pair(stack);
				stack.push(a.second != a.first);
			}
			else if (str == ">=") {
				auto a = get_pair(stack);
				stack.push(a.second >= a.first);
			}
			else if (str == "<=") {
				auto a = get_pair(stack);
				stack.push(a.second <= a.first);
			}
			else if (str == ">") {
				auto a = get_pair(stack);
				stack.push(a.second > a.first);
			}
			else if (str == "<") {
				auto a = get_pair(stack);
				stack.push(a.second < a.first);
			}
			else if (str == "~") {
				auto a = get_one(stack);
				stack.push(~a);
			}
			else if (str == "_") {
				auto a = get_one(stack);
				stack.push(-a);
			}
			else {
				stack.push(LN(str));
			}
		}
		catch(std::bad_alloc& e) {
			std::cerr << "Can't realize memory!" << std::endl;
			input.close();
			return ERROR_MEMORY;
		}
	}

	input.close();

	std::ofstream output(argv[2]);

	if (!output.is_open())
	{
		std::cerr << "Can't open output file!" << std::endl;
		return ERROR_FILE_NOT_FOUND;
	}

	while (!stack.empty()) {
		LN r = get_one(stack);
		output << r << "\n";
	}
	output.close();
	return 0;
}


/*--------------------------------------------------------------------------------------------------------*/

/*	
	Vector<int> d;
	Vector<int> n;
	for (int i = 0; i < 12; i++)
		n.push_back(i);

	d = n;
	std::cout << n.size() << " " << n.capacity() << "\n";
	std::cout << d.size() << " " << d.capacity() << "\n\n";
	for (int i = 0; i < 12; i++)
		std::cout << n[i] << " ";
	std::cout << "\n";
	for (int i = 0; i < 12; i++)
		std::cout << d[i] << " ";
*/