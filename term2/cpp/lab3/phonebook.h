#pragma once
#include <fstream>
#include <string>

class phonebook
{
public:
	phonebook();
	phonebook(std::string sur, std::string name, std::string patr, uint64_t number);

	bool operator>(phonebook& b) const;
	bool operator<(phonebook& b) const;
	bool operator>=(phonebook& b) const;
	bool operator<=(phonebook& b) const;
	bool operator==(phonebook& b) const;
	bool operator!=(phonebook& b) const;

	std::string sur, name, patr;
	uint64_t number;
private:
	int comp(phonebook& b) const;
};

std::istream& operator >> (std::istream& in, phonebook& ts);
std::ostream& operator << (std::ostream& os, const phonebook& ts);