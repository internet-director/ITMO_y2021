#include "phonebook.h"

phonebook::phonebook()
{
}

phonebook::phonebook(std::string sur, std::string name, std::string patr, uint64_t number)
{
	this->number = number;
	this->name = name;
	this->patr = patr;
	this->sur = sur;
}

bool phonebook::operator>(phonebook& b) const
{
	return (comp(b) > 0) ? 1 : 0;
}

bool phonebook::operator<(phonebook& b) const
{
	return (comp(b) < 0) ? 1 : 0;
}

bool phonebook::operator>=(phonebook& b) const
{
	return (comp(b) >= 0) ? 1 : 0;
}

bool phonebook::operator<=(phonebook& b) const
{
	return (comp(b) <= 0) ? 1 : 0;
}

bool phonebook::operator==(phonebook& b) const
{
	return !comp(b);
}

bool phonebook::operator!=(phonebook& b) const
{
	return comp(b);
}

int phonebook::comp(phonebook& b) const
{
	if (sur != b.sur) {
		return (!(sur > b.sur)) ? -1 : 1;
	}
	if (name != b.name) {
		return (!(name > b.name)) ? -1 : 1;
	}
	if (patr != b.patr) {
		return (!(patr > b.patr)) ? -1 : 1;
	}
	if (number != b.number) {
		return (!(number > b.number)) ? -1 : 1;
	}
	return 0;
}


std::istream& operator >> (std::istream& in, phonebook& ph)
{
	return in >> ph.sur >> ph.name >> ph.patr >> ph.number;
}

std::ostream& operator << (std::ostream& os, const phonebook& ph)
{
	return os << ph.sur << " " << ph.name << " " << ph.patr << " " << ph.number;
}