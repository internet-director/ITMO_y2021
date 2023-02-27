#ifndef MAIN_WINDOW_H
#define MAIN_WINDOW_H

#include <QMainWindow>
#include <QWidget>
#include <QPainter>
#include <QKeyEvent>
#include <memory>
#include <thread>
#include <mutex>
#include <queue>
#include <complex>
#include <condition_variable>
#include "ui_main_window.h"
#include "render.h"


QT_BEGIN_NAMESPACE
namespace Ui { class main_window; }
QT_END_NAMESPACE

class main_window : public QMainWindow {
Q_OBJECT

public:
    main_window(QWidget *parent = nullptr);

    ~main_window() override;

    void keyPressEvent(QKeyEvent *event) override;

    void wheelEvent(QWheelEvent *event) override;

    void mousePressEvent(QMouseEvent *event) override;

    void mouseReleaseEvent(QMouseEvent *event) override;

    void mouseMoveEvent(QMouseEvent *event) override;

    void resizeEvent(QResizeEvent *event) override;

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    const int maxStep = 3;
    std::pair<int, int> mouse = {0, 0}, prev = {-1, -1}, coord = {0, 0};
    int px = 0, py = 0, block = 128;
    double zoom = 1. / 400, pz = 0.;
    std::complex<double> zero = {0, 0};

    std::mutex mut;
    std::condition_variable cv;
    std::vector<std::thread> threads;
    std::unique_ptr<Ui::main_window> ui;


    enum STATUS {
        NONE,
        SKIP,
        KILL
    };

    using rPair = std::pair<std::shared_ptr<RenderFrame>, int>;

    struct cmp {
        bool operator()(const rPair &a, const rPair &b) {
            return a.second < b.second;
        }
    };

    Render render;
    RenderFrame demo;
    std::atomic<STATUS> status;
    std::priority_queue<std::pair<std::shared_ptr<RenderFrame>, int>, std::vector<rPair>, cmp> queue;

    bool scribbling = false;

    void generate();

    void clearQueue();

    void calculateBlock();
};

#endif // MAIN_WINDOW_H
