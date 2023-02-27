#include "render.h"
#include <complex>

double RenderFrame::getPixelColor(double x, double y) {
    std::complex<double> z = 0, c(x, y);

    size_t step = 0;
    size_t const MAX_STEPS = 2000;

    for (;;) {
        if (z.real() * z.real() + z.imag() * z.imag() >= 4.) return (step % 51) / 50.;
        if (step == MAX_STEPS) return 0;
        z = z * z + c;
        step++;
    }
}

RenderFrame::RenderFrame(int step, std::pair<double, double> coord, std::pair<double, double> center,
                         std::pair<double, double> p, int ysz,
                         int xsz) {
    reinit(step, coord, center, p, ysz, xsz);
}

void RenderFrame::generateFrame(int step) {
    uchar *image_start = img[step].bits();
    int w = img[step].width();
    int h = img[step].height();


    for (int i = 0; i < h; i++) {
        uchar *p = image_start + i * img[step].bytesPerLine();
        double y_off = (double) i / h * center.first + coord.second;
        for (int j = 0; j < w; j++) {
            double x_off = (double) j / w * center.second + coord.first;
            double val = this->getPixelColor(x_off, y_off);
            *p++ = static_cast<uchar>(val * 0xff);
            *p++ = static_cast<uchar>(val * 0xff * 0.3);
            *p++ = 0;
        }
    }
    number.store(std::min(number.load(), step));
}

void
RenderFrame::reinit(int step, std::pair<double, double> coord, std::pair<double, double> center,
                    std::pair<double, double> p, int ysz, int xsz) {
    this->p = p;
    this->coord = coord;
    this->center = center;


    img.clear();
    int dstep = 1;
    for (int i = 0; i < step; i++) {
        img.emplace_back(xsz / dstep, ysz / dstep, QImage::Format::Format_RGB888);
        dstep <<= 2;
    }
    this->number.store(step);
}

void Render::init(int w, int h, int block) {
    this->w = w;
    this->h = h;
    this->block = block;
    map.clear();
}

std::shared_ptr<RenderFrame>
Render::get(std::pair<int, int> coord, std::complex<double> offset, std::complex<double> center,
            std::pair<double, double> p,
            int step) {
    if (map.count(coord) != 0) {
        return map[coord];
    }

    map[coord] = std::make_shared<RenderFrame>(step, std::pair<double, double>{offset.real(), offset.imag()},
                                               std::pair<double, double>{center.real(), center.imag()}, p, block,
                                               block);
    return map[coord];
}
