    #include <iostream>
    #include <vector>
    #include <string>
    #include <algorithm>
    #include <Windows.h>
    #include <cmath>
    #include <bitset>
     
    typedef std::vector<long long> vec;
     
    long long merge(vec& v, long long l, long long mid, long long r) {
        long long it1 = 0;
        long long it2 = 0;
        long long count = 0;
     
        vec result;
        while ((l + it1 < mid) && (mid + it2 < r)) {
            if (v[l + it1] < v[mid + it2]) {
                result.push_back(v[l + it1]);
                it1++;
            }
            else {
                result.push_back(v[mid + it2]);
                count += (mid - it1 - l);
                it2++;
            }
        }
        while (l + it1 < mid) {
            result.push_back(v[l + it1]);
            it1++;
        }
     
        while (mid + it2 < r) {
            result.push_back(v[mid + it2]);
            it2++;
        }
     
        for (long i = 0; i < it1 + it2; i++) {
            v[l + i] = result[i];
        }
        return count;
    }
     
    long long mergeRec(vec& a, long l, long r) {
        long long count;
        if (r - l < 2) return 0;
        long long med = (r + l) / 2;
        count = mergeRec(a, l, med);
        count += mergeRec(a, med, r);
        count += merge(a, l, med, r);
        return count;
    }
     
    int main()
    {
        long long n;
        std::cin >> n;
        vec v(n);
        for (long long i = 0; i < n; i++) {
            std::cin >> v[i];
        }
        std::cout << mergeRec(v, 0, v.size());
    }