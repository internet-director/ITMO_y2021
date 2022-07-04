#include "String.h"

String::String(const char c)
{
	arr.push_back(c);
	arr.push_back(0);
}

String::String(const Vector<char>& c)
{
	arr = c;
	arr.push_back(0);
}

std::ostream& operator << (std::ostream& os, const String& str)
{
	size_t index = 0;
	if (str.arr[0] == '-') {
		index++;
		os << "-";
	}
	if (str.arr.size() > 2) {
		while (str.arr[index++] == '0');
		index--;
	}
	return os << (str.arr.data() + index);
}