#pragma once
#include "Vector.h"
#include <string_view>
#include <fstream>

class LN
{
	Vector<uint64_t> digits;
	bool _negate = false;
	bool nan = false;

public:
	LN() {}
	LN(long long val);
	LN(const char* str);
	LN(const std::string_view str);
	LN(LN&& l);
	LN(const LN& l);
	LN(const size_t sz, bool negate, bool nan);
	LN(const Vector<uint64_t>& digits, bool negate, bool nan);

	LN operator-() const;
	LN operator~() const;
	const uint64_t operator[](size_t i) const { return digits[i]; }
	uint64_t& operator[](size_t i) { return digits[i]; }

	LN operator=(const LN& a);
	LN operator=(LN&& a);
	LN operator+=(const LN& a);
	LN operator-=(const LN& a);
	LN operator*=(const LN& a);
	LN operator/=(const LN& a);
	LN operator%=(const LN& a);

	bool operator==(const LN& a) const;
	bool operator!=(const LN& a) const;
	bool operator>(const LN& a) const;
	bool operator<(const LN& a) const;
	bool operator>=(const LN& a) const;
	bool operator<=(const LN& a) const;

	friend LN operator+(const LN& a, const LN& b);
	friend LN operator-(const LN& a, const LN& b);
	friend LN operator*(const LN& a, const LN& b);
	friend LN operator/(const LN& a, const LN& b);
	friend LN operator%(const LN& a, const LN& b);

	friend std::ostream& operator << (std::ostream& os, const LN& str);

	void clear();
	void r_shift();
	void fix_zero();
	LN abs() const;
	size_t size() const { return digits.size(); }
	bool negate() const { return _negate; }
	bool isNaN() const { return nan; }
	bool isNull() const;

private:
	void resize(const size_t sz);
	int comp(const LN& a) const;
};

LN operator"" _ln(unsigned long long val);
LN operator"" _ln(const char* val, size_t sz);