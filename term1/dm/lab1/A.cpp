#include <iostream>

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
    bool check = false;
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
                if (mtrx[i][j] == mtrx[j][i] & (i != j)) {
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

int main() {
    int n;
    std::cin >> n;

    int** mass1 = mtrxCreater(n, n);
    int** mass2 = mtrxCreater(n, n);
    int** relations1 = mtrxCreater(n * n, 2);
    int** relations2 = mtrxCreater(n * n, 2);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::cin >> mass1[i][j];
        }
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::cin >> mass2[i][j];
        }
    }

    int relLen1 = 0;
    int relLen2 = 0;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (mass1[i][j]) {
                relations1[relLen1][0] = i + 1;
                relations1[relLen1][1] = j + 1;
                ++relLen1;
            }
        }
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (mass1[i][j]) {
                relations2[relLen2][0] = i + 1;
                relations2[relLen2][1] = j + 1;
                ++relLen2;
            }
        }
    }
    int** outCopm = comp(mass1, mass2, n);

    std::cout << thisRef(mass1, n) << " " << thisUnRef(mass1, n) << " " << thisSym(mass1, n) << " " << thisUnSym(mass1, n) << " " << thisTrans(mass1, n) << "\n";
    std::cout << thisRef(mass2, n) << " " << thisUnRef(mass2, n) << " " << thisSym(mass2, n) << " " << thisUnSym(mass2, n) << " " << thisTrans(mass2, n) << "\n";

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::cout << outCopm[i][j] << " ";
        }
        std::cout << "\n";
    }

    delete[] mass1;
    delete[] mass2;
    delete[] outCopm;
    delete[] relations1;
    delete[] relations2;
    return 0;
}