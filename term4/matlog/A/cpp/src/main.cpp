#include <iostream>
#include <functional>
#include "Expression.h"

using Expr = expression*;
using Impl = implication*;

class Parse {
	const char* str = nullptr;

public:
	Parse() = default;
	Parse(const char* str) {
		init(str);
	}

	void init(const char* str) {
		this->str = str;
	}

	Expr expr() {
		Expr left = dij();
		if (skip('-')) {
			str++;
			left = new implication(left, expr());
		}
		return left;
	}

private:
	Expr dij() {
		Expr left = con();
		while (skip('|')) {
			left = new disjunction(left, con());
		}
		return left;
	}

	Expr con() {
		Expr left = nt();
		while (skip('&')) {
			left = new conjunction(left, nt());
		}
		return left;
	}

	Expr nt() {
		if (skip('(')) {
			Expr left = expr();
			skip(')');
			return left;
		}
		if (skip('!')) {
			return new negation(nt());
		}
		return new variable(getVar());
	}

	std::string getVar() {
		skip();
		std::string name;
		while (isVar(*str)) name.push_back(*str++);
		return name;
	}

	bool isVar(char c) {
		if (c >= 'A' && c <= 'Z') return true;
		if (c >= '0' && c <= '9') return true;
		if (c == 39) return true;
		return false;
	}

	bool skip(char c) {
		skip();
		bool ret = *str == c;
		if (ret) str++;
		return ret;
	}

	void skip() {
		while (std::isspace(*str)) str++, skip();
	}
};

//!A&!B->!(A|B)
//(->,(&,(!A),(!B)),(!(|,A,B)))

//P1’->!QQ->!R10&S|!T&U&V

constexpr bool test = false;
bool testing(std::string str, std::string correct) {
	Parse parser(str.c_str());
	str = parser.expr()->prefix_form();
	bool res = str == correct;
	if (!res) {
		std::cout << str << std::endl << correct;
	}
	return res;
}

int main() {
	Parse parser;	
	std::string str, tmp;
	if (!test) {
		while (std::getline(std::cin, tmp) && !tmp.empty()) str += tmp;
		parser.init(str.c_str());
		std::cout << parser.expr()->prefix_form() << std::endl;
	}
	else {
		if (!testing("	\r\n! Z0123456789 ->\r	   \r\r		! A'    &			(\r  !			B		 \r-> C    \r&\rD\r)\r		 ", "(->,(!Z0123456789),(&,(!A'),(->,(!B),(&,C,D))))")) return 0;
		if(!testing("! A&!B->!(A|B)", "(->,(&,(!A),(!B)),(!(|,A,B)))")) return 0;
		if(!testing("P1'->!QQ->!R10&S|!T&U&V", "(->,P1',(->,(!QQ),(|,(&,(!R10),S),(&,(&,(!T),U),V))))")) return 0;
		std::cout << "done";
	}
	

	return 0;
}