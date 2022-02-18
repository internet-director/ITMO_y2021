#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>

double func(double x) {
	return x * x + sqrt(x);
}

int main() {
	double x;
	std::cin >> x;
	double l = 1.0, r = 1e10, m;

	while (r - l > 1e-8) {
		m = (r + l) / 2.0;
		if (func(m) > x) r = m;
		else l = m;
	}
	
	std::cout << std::setprecision(8) << l;
	return 0;
}