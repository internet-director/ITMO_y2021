#pragma once
#include "Vector.h"
#include <fstream>

class String
{
public:
	String() {}
	String(const char c);
	String(const Vector<char>& c);

	Vector<char> arr;
};

std::ostream& operator << (std::ostream& os, const String& str);