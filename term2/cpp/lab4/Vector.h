#pragma once
#include <exception>
#include <fstream>
#include <memory>
#include "utils.h"

template<class T>
class Vector
{
public:
	Vector();
	Vector(const size_t sz);
	Vector(const size_t sz, const T& val);
	Vector(const T* a, const size_t sz);
	~Vector();

	size_t size() const { return this->len; }
	size_t capacity() const { return this->_capacity; }
	void push_back(const T& a);
	void pop_back();
	T pop() const;
	void resize(const size_t sz);
	void clean();
	const T* data() const { return arr; }
	Vector<T>& operator=(const Vector<T>& a);
	bool operator==(const Vector<T>& a) const;
	T& operator[](const size_t sz);
	const T& operator[](const size_t sz) const;

	template<class T>
	friend std::ostream& operator << (std::ostream& os, const Vector<T>& str);

	T* arr = nullptr;
	size_t _capacity = 1;
	size_t len = 0;

private:
	void update(const size_t sz);
};

template<class T>
inline Vector<T>::Vector()
{
}

template<class T>
inline Vector<T>::Vector(const size_t sz)
{
	update(sz);
	this->len = sz;
}

template<class T>
inline Vector<T>::Vector(const size_t sz, const T& val)
{
	update(sz);
	this->len = sz;
	for (size_t i = 0; i < sz; i++) {
		arr[i] = val;
	}
}

template<class T>
inline Vector<T>::Vector(const T* a, size_t sz)
{
	update(sz);
	memcpy(this->arr, a, sz * sizeof(T));
	len = sz;
}

template<class T>
inline Vector<T>::~Vector()
{
	delete[] arr;
}

template<class T>
inline void Vector<T>::push_back(const T& a)
{
	update(1);
	arr[len] = a;
	len++;
}

template<class T>
inline void Vector<T>::pop_back()
{
	if(len)
		len--;
}

template<class T>
inline T Vector<T>::pop() const
{
	return arr[len - 1];
}

template<class T>
inline void Vector<T>::resize(const size_t sz)
{
	this->_capacity = sz * 2;
	T* tmp = new T[this->_capacity];
	if (arr) {
		mem::memcpy(tmp, arr, ((this->len > sz) ? sz : this->len) * sizeof(T));
		delete[] arr;
	}
	if (this->len < sz) {
		for (size_t i = 0; i < sz; i++) {
			tmp[this->len + i] = 0;
		}
	}
	this->len = sz;
	arr = tmp;
}

template<class T>
inline void Vector<T>::clean()
{
	delete[] arr;
	arr = nullptr;
	_capacity = 1;
	len = 0;
}

template<class T>
inline Vector<T>& Vector<T>::operator=(const Vector<T>& a)
{
	if (this == &a)
		return *this;

	update(a.size());
	mem::memcpy(this->arr, a.arr, a.size() * sizeof(T));
	len = a.size();
	return *this;
}

template<class T>
inline bool Vector<T>::operator==(const Vector<T>& a) const
{
	if (this->size() != a.size()) 
		return false;
	for (size_t i = 0; i < a.size(); i++) {
		if (this[i] != a[i]) 
			return false;
	}
	return true;
}

template<class T>
inline T& Vector<T>::operator[](const size_t sz)
{
	//if (sz >= len) throw std::exception("vector out of range");
	return arr[sz];
}

template<class T>
inline const T& Vector<T>::operator[](const size_t sz) const
{
	//if (sz >= len) throw std::exception("vector out of range");
	return arr[sz];
}

template<class T>
inline void Vector<T>::update(const size_t sz) {
	if (this->_capacity > sz + len) return;
	
	size_t oldSz = this->capacity();
	while (this->_capacity <= sz + len) {
		this->_capacity *= 2;
	}
	T* tmp;
	try {
		tmp = new T[this->_capacity];
	}
	catch (std::bad_alloc& e) {
		this->_capacity = oldSz + sz;
		tmp = new T[capacity()];
	}
	if (arr) {
		mem::memcpy(tmp, arr, len * sizeof(T));
		delete[] arr;
	}
	arr = tmp;
}

template <class T>
std::ostream& operator<<(std::ostream& os, const Vector<T>& str)
{
	for (int i = 0; i < str.size(); i++) {
		os << str[i] << " ";
	}
	return os;
}