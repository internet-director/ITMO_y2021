#include "main_window.h"
#include <iostream>
#include <QApplication>
#include <QThread>

int main(int argc, char *argv[]) {
    QApplication a(argc, argv);
    main_window w;

    w.show();
    return a.exec();
}
