//#define DEBUG
#include <iostream>
#include <cmath>
#include <vector>
#include <string>
typedef unsigned char byte;

#define _I(X) std::stoi(X)

void memSet(void* dst, byte var, int len) {
    for (volatile int i = 0; i < len; i++) {
        ((byte*)dst)[i] = var;
    }
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

bool thisRef(int** mtrx, int len) {
    for (int i = 0; i < len; i++) {
        if (!(mtrx[i][i] & mtrx[i][i])) {
            return false;
        }
    }
    return true;
}

bool thisUnRef(int** mtrx, int len) {
    for (int i = 0; i < len; i++) {
        if (mtrx[i][i] & mtrx[i][i]) {
            return false;
        }
    }
    return true;
}

bool thisSym(int** mtrx, int len) {
    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            if ((mtrx[i][j])) {
                if (mtrx[i][j] != mtrx[j][i]) {
                    return false;
                }
            }
        }
    }
    return true;
}

bool thisUnSym(int** mtrx, int len) {
    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            if ((mtrx[i][j])) {
                if ((mtrx[i][j] == mtrx[j][i]) & (i != j)) {
                    return false;
                }
            }
        }
    }
    return true;
}

bool thisTrans(int** mtrx, int len) {
    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            if ((i != j) & (mtrx[i][j])) {
                for (int k = 0; k < len; k++) {
                    if ((mtrx[i][k] == 0) &
                        (k != i) & (k != j) &
                        (mtrx[j][k])) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

int** comp(int** mtrx1, int** mtrx2, int len) {
    int** out = mtrxCreater(len, len);
    mtrxset(out, 0, len, len);

    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            if (mtrx1[i][j]) {
                for (int k = 0; k < len; k++) {
                    if (mtrx2[j][k]) {
                        out[i][k] = 1;
                    }
                }
            }
        }
    }
    return out;
}

int numChecker(int n) {
    unsigned int count = 0;
    for (; n; count++)
        n &= (n - 1);
    return count;
}

int** getRightMass(int n) {
    int** mtrx = mtrxCreater(n, 2);

    for (int i = 0; i < n; i++) {
        mtrx[i][0] = i;
        mtrx[i][1] = numChecker(i);
    }

    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (mtrx[i][1] > mtrx[j][1]) {
                int l0 = mtrx[i][0];
                int l1 = mtrx[i][1];

                mtrx[i][0] = mtrx[j][0];
                mtrx[j][1] = mtrx[j][1];
                mtrx[i][0] = l0;
                mtrx[j][1] = l1;
            }
        }
    }

    /*for (int i = 0; i < n; i++) {
        ParseError((int)mtrx[i][1]);
    }*/

    return mtrx;
}

int* zhigaL(int* mass, int n, int step, std::vector<std::string> ist) {
    int* zhiga = new int[n];
    bool check = mass[0];
    zhiga[0] = check;
    int** m = getRightMass(n);
    int asd = 1;

    for (int i = 1; i < n; i++) {
        //ParseError("check");
        if (asd < m[i][1]) {
            check = mass[0];
            asd = m[i][1];
            for (int k = 1; k < pow(2, asd) - 1; k++) {
                check = check ^ zhiga[m[k][0]];
                //ParseError((int)m[k][1]);
                //ParseError((int)(i + k - step));
            }
        }
        if ((check ^ 0) == mass[m[i][0]]) {
            zhiga[m[i][0]] = 0;
        }
        else {
            zhiga[m[i][0]] = 1;
        }
    }
    return zhiga;
}

int main() {
    int step, shit;
    std::cin >> step;
    int n = (int)pow(2, step);
    std::vector<std::string> mnelen;
    //ParseError((int)n);

    int* mass = new int[n];
    int* triangle = new int[n];
    int* ziga = new int[n];
    std::string str;
    for (int i = 0; i < n; i++) {
        std::cin >> str;
        mnelen.push_back(str);
        std::cin >> mass[i];
    }


    for (int i = 0; i < n; i++) {
        ziga[i] = mass[0];
        for (int j = 0; j < n - i - 1; j++) {
            triangle[j] = mass[j] ^ mass[j + 1];
        }
        mass = triangle;
    }

    for (int i = 0; i < n; i++) {
        std::cout << (mnelen[i]) << " " << ziga[i] << "\n";
    }
    return 0;
}