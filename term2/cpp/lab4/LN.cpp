#include "LN.h"

LN::LN(long long val)
{
	if (val < 0) {
		this->_negate = true;
		val *= -1ll;
	}
	if (val >= (uint64_t)1e9) {
		digits.resize(2);
		digits[0] = val * (negate() ? -1 : 1);
		if (digits[0] >= (uint64_t)1e9) {
			digits[1] = digits[0] / (uint64_t)1e9;
			digits[0] %= (uint64_t)1e9;
		}
		return;
	}
	digits.push_back(val);
}
LN::LN(const char* str)
{
	long long null_sz = -1, sz = str_length(str);
	if (!sz) return;
	if (str[0] == '-') {
		null_sz++;
		this->_negate = true;
	}
	if (!mem::memcmp(str + null_sz, "NaN", 3)) {
		this->nan = true;
		digits.push_back(0);
		return;
	}
	while (str[++null_sz] == '0' && sz > null_sz);
	if (null_sz == sz) {
		digits.push_back(0);
		return;
	}

	for (long long i = sz - 1; i >= null_sz; i -= 9) {
		char sub[10];
		size_t start = (i < 9) ? null_sz : (i - 8);
		size_t size = (i < 9) ? (i - null_sz + 1) : 9;
		mem::memcpy(sub, &str[start], size);
		sub[size] = 0;
		digits.push_back(atoll(sub));
	}
}
LN::LN(const std::string_view str)
{
	long long null_sz = -1;
	if (!str.size()) return;
	if (str[0] == '-') {
		null_sz++;
		this->_negate = true;
	}
	if (str.find("NaN") != str.npos) {
		this->nan = true;
		digits.push_back(0);
		return;
	}
	while (str[++null_sz] == '0' && str.size() > null_sz);
	if (null_sz == str.size()) {
		digits.push_back(0);
		return;
	}

	for (long long i = str.size() - 1; i >= null_sz; i -= 9) {
		char sub[10];
		size_t start = (i < 9) ? null_sz : (i - 8);
		size_t size = (i < 9) ? (i - null_sz + 1) : 9;
		mem::memcpy(sub, &str[start], size);
		sub[size] = 0;
		digits.push_back(atoll(sub));
	}
}
LN::LN(LN&& a)
{
	digits = a.digits;
	_negate = a.negate();
	nan = a.isNaN();
	a.clear();
}
LN::LN(const LN& l)
{
	this->digits = l.digits;
	this->_negate = l.negate();
	this->nan = l.isNaN();
}
LN::LN(const size_t sz, bool negate, bool nan)
{
	this->digits.resize(sz);
	this->_negate = negate;
	this->nan = nan;
}
LN::LN(const Vector<uint64_t>& digits, bool negate, bool nan)
{
	this->digits = digits;
	this->_negate = negate;
	this->nan = nan;
	this->fix_zero();
}

void LN::clear()
{
	digits.clean();
	_negate = false;
	nan = false;
}
void LN::r_shift()
{
	if (!size()) {
		digits.push_back(0);
		return;
	}
	*this = *this * (uint64_t)1e9;
}
void LN::fix_zero()
{
	while (digits.size() > 1 && !digits.pop())
		digits.pop_back();
}
LN LN::abs() const
{
	return LN(digits, false, nan);
}

LN LN::operator-() const
{
	if (*this == 0ll)
		return LN(this->digits, false, nan);
	return LN(this->digits, !this->negate(), nan);
}
LN LN::operator~() const
{
	if (negate())
		return LN(1, false, true);
	if (*this == 0ll)
		return LN(0ll);

	LN res = *this;
	LN prev = 0ll;
	while ((res - prev).abs() >= 1)
	{
		prev = res;
		res = (res + (*this / res)) / 2;
		if (res - prev == 1) {
			return prev;
		}
	}
	return res;
}

LN LN::operator=(const LN& a)
{
	if (this == &a)
		return*this;

	digits = a.digits;
	_negate = a.negate();
	nan = a.isNaN();
	return *this;
}
LN LN::operator=(LN&& a)
{
	if (this == &a)
		return *this;
	digits = a.digits;
	_negate = a.negate();
	nan = a.isNaN();
	a.clear();
	return *this;
}

LN LN::operator+=(const LN& a)
{
	return *this = *this + a;
}
LN LN::operator-=(const LN& a)
{
	return *this = *this - a;
}
LN LN::operator*=(const LN& a)
{
	return *this = *this * a;
}
LN LN::operator/=(const LN& a)
{
	return *this = *this / a;
}
LN LN::operator%=(const LN& a)
{
	return *this = *this % a;
}

bool LN::operator==(const LN& a) const
{
	int code = comp(a);
	if (code == 2)
		return false;
	return !code;
}
bool LN::operator!=(const LN& a) const
{
	int code = comp(a);
	if (code == 2)
		return true;
	return code;
}
bool LN::operator>(const LN& a) const
{
	int code = comp(a);
	if (code == 2)
		return false;
	return (code > 0) ? true : false;
}
bool LN::operator<(const LN& a) const
{
	int code = comp(a);
	if (code == 2)
		return false;
	return (code < 0) ? true : false;
}
bool LN::operator>=(const LN& a) const
{
	int code = comp(a);
	if (code == 2)
		return false;
	return (code >= 0) ? true : false;
}
bool LN::operator<=(const LN& a) const
{
	int code = comp(a);
	if (code == 2)
		return false;
	return (code <= 0) ? true : false;
}
bool LN::isNull() const
{
	if (size() == 1 && operator[](0) == 0) return true;
	return false;
}
void LN::resize(const size_t sz)
{
	this->digits.resize(this->size() + sz);
}
int LN::comp(const LN& a) const
{
	if (isNaN() || a.isNaN())
		return 2;
	if (isNull() && a.isNull())
		return 0;
	if (this->negate() != a.negate())
		return (this->negate() < a.negate()) ? 1 : -1;
	if (this->size() != a.size())
		return ((this->size() > a.size()) ^ a.negate()) ? 1 : -1;

	for (size_t i = 0; i < this->size(); i++) {
		size_t index = this->size() - i - 1;
		if (this->digits[index] != a.digits[index])
			return ((this->digits[index] > a.digits[index]) ^ a.negate()) ? 1 : -1;
	}

	return 0;
}

LN operator+(const LN& a, const LN& b)
{
	if (a.isNaN() || b.isNaN())
		return LN(1, false, true);
	if (a == 0ll) return b;
	if (b == 0ll) return a;
	if (a.negate() != b.negate())
		return (a - (-b));

	size_t max = std::max(a.size(), b.size());
	size_t min = std::min(a.size(), b.size());
	Vector<uint64_t> digits(max + 1, 0);

	for (size_t i = 0; i < min; i++) {
		digits[i] += a[i] + b[i];
		if (digits[i] >= (uint64_t)1e9) {
			digits[i] -= (uint64_t)1e9;
			digits[i + 1]++;
		}
	}
	for (size_t i = min; i < max; i++) {
		if (a.size() > b.size()) digits[i] += a[i];
		else digits[i] += b[i];
		if (digits[i] >= (uint64_t)1e9) {
			digits[i] -= (uint64_t)1e9;
			digits[i + 1]++;
		}
	}

	LN res(digits, a.negate(), false);
	res.fix_zero();

	return res;
}
LN operator-(const LN& a, const LN& b)
{
	if (a.isNaN() || b.isNaN())
		return LN(1, false, true);
	if (a.negate() != b.negate())
		return (a + (-b));
	if (a.abs() < b.abs())
		return -(b - a);

	size_t max = std::max(a.size(), b.size());
	size_t min = std::min(a.size(), b.size());
	Vector<uint64_t> digits(max + 1, 0);

	for (size_t i = 0; i < min; i++) {
		digits[i] += a[i] - b[i];
		if (digits[i] >= 1e9) {
			digits[i] += (uint64_t)1e9;
			digits[i + 1]--;
		}
	}
	for (size_t i = min; i < max; i++) {
		digits[i] += a[i];
		if (digits[i] >= 1e9) {
			digits[i] = digits[i] + (uint64_t)1e9;
			digits[i + 1]--;
		}
	}

	LN res(digits, a.negate(), false);
	res.fix_zero();

	return res;
}

LN operator*(const LN& a, const LN& b)
{
	if (a.isNaN() || b.isNaN())
		return LN(1, false, true);

	Vector<uint64_t> digits(a.size() + b.size() + 1, 0);

	for (size_t i = 0; i < a.size(); i++) {
		for (size_t j = 0; j < b.size(); j++) {
			digits[i + j] += a[i] * b[j];
			if (digits[i + j] >= (uint64_t)1e9) {
				digits[i + j + 1] += digits[i + j] / (uint64_t)1e9;
				digits[i + j] = digits[i + j] % (uint64_t)1e9;
			}
		}
	}

	LN res(digits, a.negate() ^ b.negate(), false);
	res.fix_zero();

	return res;
}
LN operator/(const LN& a, const LN& b)
{
	if (a.isNaN() || b.isNaN() || b == 0ll)
		return LN(1, false, true);
	if (a.abs() < b.abs())
		return "0";
	if (a.abs() == b.abs())
		return LN(1ll * (a.negate() == b.negate()) ? 1 : -1);

	LN b_abs = b.abs(), tmp;
	Vector<uint64_t> digits(a.size(), 0);

	for (long long i = a.size() - 1; i >= 0; i--) {
		tmp.r_shift();
		tmp[0] = a[i];
		tmp.fix_zero();

		long long key = 0, l = 0, r = (uint64_t)1e9;
		while (l <= r) {
			long long m = (l + r) / 2;
			LN t = b_abs * m;
			if (t <= tmp) {
				key = m;
				l = m + 1;
			}
			else r = m - 1;
		}

		digits[i] = key;
		tmp = tmp - b_abs * key;
	}

	LN res(digits, a.negate() ^ b.negate(), false);
	res.fix_zero();

	return res;
}

LN operator%(const LN& a, const LN& b)
{
	LN l = a.abs();
	LN r = b.abs();	
	return (l - (l / r) * r) * (a.negate() ? -1ll : 1ll);
}

LN operator""_ln(unsigned long long val)
{
	return LN(val);
}
LN operator"" _ln(const char* val, size_t sz)
{
	return LN(val);
}

std::ostream& operator << (std::ostream& os, const LN& str)
{
	if (str.isNaN())
		return os << "NaN";
	if (str.isNull())
		return os << "0";
	if (str.negate())
		os << "-";

	os << str[str.size() - 1];

	for (long long i = str.size() - 2; i >= 0; i--) {
		uint64_t n = 10;
		for (int j = 0; j < 8; j++) {
			if (str[i] < n)
				os << "0";
			n *= 10;
		}
		os << str[i];
	}
	return os;
}