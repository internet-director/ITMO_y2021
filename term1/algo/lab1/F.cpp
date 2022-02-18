#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>

std::vector<int> heap;

void Insert(int x) {
	heap.push_back(x);
	int size = heap.size() - 1;

	while (heap[size] > heap[(size - 1) / 2] && size > 0) {
		std::swap(heap[size], heap[(size - 1) / 2]);
		size = (size - 1) / 2;
	}
	heap[size] = x;
}

void siftDown(int i) {
	int l, r, j;
	while (true) {
		l = 2 * i + 1;
		r = 2 * i + 2;
		j = i;
		if ((l < heap.size()) && (heap[j] < heap[l])) j = l;
		if ((r < heap.size()) && (heap[j] < heap[r])) j = r;
		if (j == i) break;
		std::swap(heap[i], heap[j]);
		i = j;
	}
}

int Extract() {
	int max = heap[0];
	heap.erase(heap.begin());

	for (int i = heap.size() / 2; i >= 0; i--) {
		siftDown(i);
	}
	return max;
}

int main() {
	int n, code, number;
	std::vector<int> result;
	std::cin >> n;

	for (int i = 0; i < n; i++) {
		std::cin >> code;

		if (code) result.push_back(Extract());
		else {
			std::cin >> number;
			Insert(number);
		}
	}
	for (int p : result) std::cout << p << "\n";

	return 0;
}