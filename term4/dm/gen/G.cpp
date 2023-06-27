#include <iostream>
#include <sstream>
#include <string>
#include <deque>
#include <array>

typedef std::int64_t int_t;
typedef std::uint64_t uint_t;
typedef std::array<uint_t, 7> arr_t;

int_t c(int_t n, int_t k) {
    if (n < 0) n = 0;
    if (n < k) return 0;
    uint_t res = 1;
    for (uint_t i = n - k + 1; i <= n; i++) res *= i;
    for (uint_t i = 2; i <= k; i++) res /= i;
    return res;
}

void B(arr_t& res) {
    res.fill(0);
    res[1] = 1;
}

void Seq(arr_t& res, arr_t& dop) {
    res.fill(0);
    res[0] = 1;

    for (int_t i = 1; i < 7; i++) {
        for (int_t j = 1; j <= i; j++) {
            res[i] += res[i - j] * dop[j];
        }
    }
}

void MSet(arr_t& res, arr_t& dop) {
    res.fill(0);
    res[0] = 1;
    std::array<std::array<uint_t, 7>, 7> arr{};
    arr[0].fill(1);

    for (int_t n = 1; n < 7; n++) {
        for (int_t k = 1; k < 7; k++) {
            for (int_t i = 0; i <= n / k; i++) {
                arr[n][k] += c(dop[k] + i - 1, i) * arr[n - i * k][k - 1];
            }
        }
        res[n] = arr[n][n];
    }
}

void PSet(arr_t& res, arr_t& dop1, arr_t& dop2) {
    res.fill(0);

    for (int_t i = 0; i < 7; i++) {
        for (int_t j = 0; j <= i; j++) {
            res[i] += dop1[j] * dop2[i - j];
        }
    }
}


void parser(std::string& input, arr_t& res, int_t& pos) {
    arr_t dop1{}, dop2{};
    pos += 2;
    if (input[pos] == 'L') {
        parser(input, dop1, pos);
        Seq(res, dop1);
    }
    else if (input[pos] == 'S') {
        parser(input, dop1, pos);
        MSet(res, dop1);
    }
    else if (input[pos] == 'P') {
        parser(input, dop1, pos);
        parser(input, dop2, pos);
        PSet(res, dop1, dop2);
    }
    else if (input[pos] == 'B') {
        B(res);
        pos--;
    }
    else {
        throw 1;
    }
    pos++;
}

int main()
{
    std::string str;
    std::cin >> str;

    int_t pos = -2;
    arr_t res;
    parser(str, res, pos);
    for (auto it : res) std::cout << it << " ";
    return 0;
}