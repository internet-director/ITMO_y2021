#ifndef RENDER_H
#define RENDER_H

#include <QMainWindow>
#include <QKeyEvent>
#include <QPainter>
#include <condition_variable>
#include <atomic>
#include <mutex>
#include <queue>
#include <memory>
#include <set>
#include <complex>


class RenderFrame {
    std::pair<double, double> coord, center, p;
    std::vector<QImage> img;
    int px = 1, py = 1;

public:
    // y - номер строчки
    // ysz  - количество рендерируемых строк
    // offset - смещение относительно начала строки
    // xsz - количество рендерируемых элементов в строке
    RenderFrame() = default;

    RenderFrame(int step, std::pair<double, double> coord, std::pair<double, double> center,
                std::pair<double, double> p, int ysz, int xsz);

    void generateFrame(int step);

    void
    reinit(int step, std::pair<double, double> coord, std::pair<double, double> center, std::pair<double, double> p,
           int ysz, int xsz);

    QImage *getImage() {
        if (number.load() >= (int) img.size()) return nullptr;
        return &img[number.load()];
    }

    std::atomic<bool> done = false, work = false;
    std::atomic<int> number;

private:
    double getPixelColor(double x, double y);
};

class Render {
    int w, h;
    int block;
    double zoom;
    int spaceBorder = 1024;
    // shared, тк нужно удалять память, но поток может ее юзать
    std::map<std::pair<int, int>, std::shared_ptr<RenderFrame>> map;

public:
    Render() = default;

    void init(int w, int h, int block);

    std::shared_ptr<RenderFrame>
    get(std::pair<int, int> coord, std::complex<double> offset, std::complex<double> center,
        std::pair<double, double> p, int step);
};

#endif // RENDER_H
