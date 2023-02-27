#include "main_window.h"
#include "ui_main_window.h"

#include <QPainter>
#include <QThread>
#include <QDebug>
#include <complex>
#include <thread>
#include <set>

main_window::main_window(QWidget *parent)
        : QMainWindow(parent), ui(new Ui::main_window), status(NONE),
          threads(std::thread::hardware_concurrency() == 1 ? 1 : std::thread::hardware_concurrency() - 1) {
    ui->setupUi(this);

    for (size_t i = 0; i < threads.size(); i++) {
        threads[i] = std::thread(&main_window::generate, this);
    }
    coord = {0, 0};
    calculateBlock();
    render.init(width(), height(), block);
}

main_window::~main_window() {
    status.store(KILL);
    cv.notify_all();
    for (auto &th: threads) {
        th.join();
    }
}

void main_window::paintEvent(QPaintEvent *event) {
    QPainter p(this);
    bool rerender = false;
    int xp = coord.first % block;
    int yp = coord.second % block;
    int xoff = coord.first > 0 ? xp : xp + block;
    int yoff = coord.second > 0 ? yp : yp + block;

    {
        int sz = 32;
        if (px != width() || py != height() || pz != zoom || scribbling) {
            pz = zoom;
            px = width();
            py = height();
            prev.first = px / sz;
            prev.second = py / sz;
            auto center = std::complex<double>(py, px) * zoom;
            auto offset = zero - std::complex<double>(coord.first, coord.second) * zoom;
            demo.reinit(1, {offset.real(), offset.imag()}, {center.real(), center.imag()}, {prev.first, prev.second},
                        prev.second + block / sz + 1,
                        prev.first + block / sz + 1);
            demo.generateFrame(0);
        }
        p.setTransform(
                QTransform((double) px / demo.getImage()->width(), 0, 0, (double) py / demo.getImage()->height(), 0,
                           0));
        p.drawImage(0, 0, *demo.getImage());
    }

    for (int i = -block; i < width(); i += block) {
        for (int j = -block; j < height(); j += block) {
            int xc = i - coord.first + xoff;
            int yc = j - coord.second + yoff;
            auto center = std::complex<double>(block, block) * zoom;
            auto offset = zero + std::complex<double>(xc, yc) * zoom;
            auto frame = render.get({xc, yc}, offset, center, {block, block}, maxStep);
            auto *image = frame->getImage();

            int step = frame->number.load() - 1;

            if (step != -1) rerender = true;

            if (!frame->work.load() && step != -1) {
                std::lock_guard lock(mut);
                frame->work.store(true);
                queue.push({frame, step});
                cv.notify_one();
            } else rerender = true;

            if (image != nullptr) {
                p.setTransform(QTransform((double)block / image->height(), 0, 0, (double)block / image->width(), i + xoff, j + yoff));
                p.drawImage(0, 0, *image);
            } else rerender = true;
        }
    }

    if (rerender) update();
}

void main_window::generate() {
    while (status.load() != KILL) {
        rPair currentTask;
        {
            std::unique_lock lock(mut);
            int kill;
            cv.wait(lock, [&] { kill = status.load(); return (!queue.empty() || kill == KILL); });
            if (kill == KILL) break;
            currentTask = queue.top();
            queue.pop();
        }

        currentTask.first->generateFrame(currentTask.second);
        currentTask.first->work.store(false);
    }
}

void main_window::clearQueue() {
        std::lock_guard lock(mut);
        queue = std::priority_queue<rPair, std::vector<rPair>, cmp>();
        cv.notify_all();
}

void main_window::keyPressEvent(QKeyEvent *event) {
    switch (event->key()) {
        case Qt::Key_Q:
            close();
            break;
        default:
            QWidget::keyPressEvent(event);
    }
}

void main_window::wheelEvent(QWheelEvent *event) {
    QPoint numD = event->angleDelta();
    double sign = numD.ry() < 0 ? -1 : 1;
    double part = 5;
    coord.first += sign * (width() / part - event->position().x()) / part;
    coord.second += sign * (height() / part - event->position().y()) / part;
    zero -= zoom * std::complex<double>(coord.first, coord.second);
    coord = {0, 0};
    zoom *= std::pow(.9, sign);

    clearQueue();
    render.init(width(), height(), block);
    update();
}

void main_window::mousePressEvent(QMouseEvent *event) {
    if (event->button() == Qt::LeftButton) {
        scribbling = true;
        mouse.first = event->position().x();
        mouse.second = event->position().y();
        update();
    }
}

void main_window::mouseReleaseEvent(QMouseEvent *event) {
    if (event->button() == Qt::LeftButton && scribbling) {
        mouse.first = event->position().x();
        mouse.second = event->position().y();
        scribbling = false;
        //clearQueue();
        //render.clear(coord.first + width(), coord.second + height(), 0, 0);
        update();
    }
}

void main_window::mouseMoveEvent(QMouseEvent *event) {
    if ((event->buttons() & Qt::LeftButton) && scribbling) {
        coord.first += event->position().x() - mouse.first;
        coord.second += event->position().y() - mouse.second;
        mouse.first = event->position().x();
        mouse.second = event->position().y();
        update();
    }
}

void main_window::calculateBlock() {
    block = 1;
    int sc = std::ceil(width() * height() / threads.size());
    if (threads.size() == 1) sc = width() > height() ? width() : height();

    while (block * block * 4 < sc) {
        block <<= 1;
    }
}

void main_window::resizeEvent(QResizeEvent *event) {
    clearQueue();
    calculateBlock();
    render.init(width(), height(), block);
    update();
}

