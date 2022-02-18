//#define DEBUG
#include <iostream>
#include <cmath>
#include <vector>
#include <string>
#include <chrono>
#include <Windows.h>
#include <fstream>

#include <cmath>

typedef unsigned char byte;

#define _I(X) std::stol(X)

void memSet(void* dst, byte var, int len) {
    for (volatile int i = 0; i < len; i++) {
        ((byte*)dst)[i] = var;
    }
}

void sleep(size_t time) {
    Sleep(time);
}

#ifdef DEBUG
class ParseError {
public:
    ParseError() { }
    ParseError(int str) {
        std::cout << "Parse error: " << str << "\n";
    }
    ParseError(std::string str) {
        std::cout << "Parse error: " << str << "\n";
    }
    ParseError(std::wstring str) {
        std::wcout << "Parse error: " << str << "\n";
    }
};
#endif

void mtrxset(int** data, int val, int len1, int len2) {
    for (int i = 0; i < len1; i++) {
        for (int j = 0; j < len2; j++) {
            data[i][j] = 0;
        }
    }
}

int** mtrxCreater(int size1, int size2) {
    int** mass = new int* [size1];
    for (int i = 0; i < size1; i++) {
        mass[i] = new int[size2];
    }
    return mass;
}

bool thisNull(char* mass) {
    if (mass[0] == '0')
        return true;
    return false;
}

bool thisOne(char* mass, int len) {
    if (mass[len - 1] == '1')
        return true;
    return false;
}


bool thisSam(char* mass, int len) {
    for (int i = 0; i < len / 2; i++) {
        if (mass[i] == mass[len - i - 1]) {
            return false;
        }
    }
    return true;
}

bool isMon(char* arr, int len) {
    if ((len - 1) & len) return false;

    while (len > 0) {
        for (int i = 0; i < len; i += len) {
            for (int j = len; j--; ++i) {
                if (arr[i] > arr[i + len]) {
                    return false;
                }
            }
        }
        len = len / 2;
    }
    return true;
}

bool thisMon(char* arr, int start, int end) {
    int n = end - start;
    if (n <= 1) return true;

    //std::string s1, s2;

    //char* s1 = new char[n / 2 + 1];
    //char* s2 = new char[n / 2 + 1];

    bool data = true;

    for (int i = 0; i < n / 2; i++) {
        if (arr[start + i] > arr[start + n / 2 + i])
            data = false;
    }
    //s1[n / 2] = '\0';
    //s2[n / 2] = '\0';



    if (data) {
        //delete[] s1;
        //delete[] s2;
        return thisMon(arr, start, start + n / 2) && thisMon(arr, start + n / 2, start + n);
    }
    else {
        //delete[] s1;
        //delete[] s2;
        return false;
    }

    //std::cout << s1 << "  " << s2 << "\n
}

bool thisLin(char* arr, int len) {
    int* ziga = new int[len];
    int* mass = new int[len];
    int* triangle = new int[len];

    for (int i = 0; i < len; i++) {
        mass[i] = (int)arr[i] - (int)'0';
    }

    for (int i = 0; i < len; i++) {
        ziga[i] = mass[0];
        for (int j = 0; j < len - i - 1; j++) {
            triangle[j] = mass[j] ^ mass[j + 1];
        }
        //memcpy(triangle, mass, len - 1);
        mass = triangle;
    }

    for (int i = 0; i < len; i++) {
        if (ziga[i]) {
            if ((i & -i) != i) {
                delete[] ziga;
                delete[] mass;
                //delete[] triangle;
                return false;
            }
        }
    }

    delete[] ziga;
    delete[] mass;
    //delete[] triangle;
    return true;
}

/*char* randomBin(int len, int start) {
    std::string str;
    char* arr = new char[pow(2, len) + 1];
    srand(start);
    for (int i = 0; i < pow(2, len); i++) {
        int k = rand() % 2;
        str += std::to_string(k);
        arr[i] = k + '0';
    }
    arr[(int)pow(2, len)] = '\0';
    return arr;
}*/

int main() {
    int n, k;
    int step = 0;
    std::cin >> n;
    int* count = new int[n];
    int max = 5;

   


    char** arr = new char* [n];
    for (int i = 0; i < n; i++) {
        arr[i] = new char[1 << max + 1];
    }

    for (int i = 0; i < n; i++) {
        std::cin >> k;
        if (k) {
            count[i] = 1 << k;
            std::cin >> arr[i - step];
        }
        else {
            count[i] = 1;
            std::cin >> arr[i - step];
        }
    }

    bool thisN = true;
    bool thisO = true;
    bool thisS = true;
    bool thisM = true;
    bool thisL = true;

    //std::cout << n;

    for (int i = 0; i < n; i++) {
        if (count[i] > 1) {
            thisN &= thisNull(arr[i]);
            thisO &= thisOne(arr[i], count[i]);
            thisS &= thisSam(arr[i], count[i]);
            thisM &= thisMon(arr[i], 0, count[i]);
            thisL &= thisLin(arr[i], count[i]);
        }
        else {
            thisN &= thisNull(arr[i]);
            thisO &= thisOne(arr[i], count[i]);
            thisS = false;
            //thisL = false;
        }
    }

    if (thisN || thisO || thisS || thisM || thisL) {
        std::cout << "NO";
    }
    else {
        std::cout << "YES";
    }
    //std::cout << "\n";
    //auto endTime = std::chrono::high_resolution_clock::now();
    //auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    //std::cout << duration;

    /*for (int i = 0; i < n; i++) {
        std::cout << thisNull(arr[i], count[i]) << thisOne(arr[i]) << thisSam(arr[i], count[i]) << thisMon(arr[i], 0, count[i]) << thisLin(arr[i], count[i]) << "\n";
    }*/

    delete[] count;
    delete[] arr;
    return 0;
}


/*std::ofstream file("C:\\Users\\stasan\\Downloads\\Telegram Desktop\\рудзбу\\data.txt");
file << n << "\n";

for (int i = 0; i < n; i++) {
    int l = rand() % 6;
    file << l;
    if (l)
        file << " " << randomBin(l, i);
    file << "\n";
}
return 0;*/

//auto startTime = std::chrono::high_resolution_clock::now();

//int** mass = mtrxCreater(n, 5);