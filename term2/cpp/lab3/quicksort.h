#pragma once
#include <vector>
#include <algorithm>

template <typename T, bool descending>
void quicksort(std::vector<T>& arr, long long r, long long l);

template<typename T, bool descending>
void quicksort(std::vector<T>& arr, long long r, long long l = 0)
{
    while (l < r)
    {
        // have not overflow
        T m = arr[l + (r - l) / 2];
        long long dop = l;

        for (long long i = l; i < r; i++)
        {
            if ((arr[i] < m) ^ descending)
            {
                std::swap(arr[dop++], arr[i]);
            }
        }
        std::swap(arr[dop], arr[r]);

        if (2 * dop < l + r)
        {
            quicksort<T, descending>(arr, dop - 1, l);
            l = dop + 1;
        }
        else
        {
            quicksort<T, descending>(arr, r, dop + 1);
            r = dop - 1;
        }
    }
}