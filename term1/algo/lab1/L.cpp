#include <iostream>
#include <vector>
#include <cmath>

typedef std::vector<int> vec;

void merge(vec& v, int l, int mid, int r) {
    int it1 = 0;
    int it2 = 0;

    vec result(r - l);

    while ((l + it1 < mid) && (mid + it2 < r)) {
        if (v[l + it1] < v[mid + it2]) {
            result[it1 + it2] = v[l + it1];
            it1++;
        }
        else {
            result[it1 + it2] = v[mid + it2];
            it2++;
        }
    }
    while (l + it1 < mid) {
        result[it1 + it2] = v[l + it1];
        it1++;
    }

    while (mid + it2 < r) {
        result[it1 + it2] = v[mid + it2];
        it2++;
    }

    for (int i = 0; i < it1 + it2; i++) {
        v[l + i] = result[i];
    }
}

void mergeSort(vec& v) {
    for (int i = 1; i < v.size(); i *= 2) {
        for (int j = 0; j < v.size() - i; j += 2 * i) {
            merge(v, j, j + i, __min(j + 2 * i, v.size()));
        }
    }
}

int upper(vec& v, int key) {
    int l = -1;
    int r = v.size();
    int m;
    while (l + 1 < r) {
        m = (l + r) / 2;
        if (v[m] > key) r = m;
        else l = m;
    }
    return r;
}

int lower(vec& v, int key) {
    int l = -1;
    int r = v.size();
    int m;
    while (l + 1 < r) {
        m = (l + r) / 2;
        if (v[m] >= key) r = m;
        else l = m;
    }
    return r;
}

int main() {
    int n;
    std::cin >> n;
    vec mass(n), res;
    for (int i = 0; i < n; i++) std::cin >> mass[i];
    mergeSort(mass);
    std::cin >> n;

    for (int i = 0; i < n; i++) {
        int l1, r1;
        std::cin >> l1 >> r1;
        res.push_back(upper(mass, r1) - lower(mass, l1));
    }
    for (int i = 0; i < n; i++) std::cout << res[i] << " ";
}
