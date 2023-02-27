# Настройка инструментов на Linux

## Вариант 1. APT

Используйте версию Debian `bullseye` или `sid`, `buster` не подойдет. Ubuntu должна быть не младше 19.10.

```
# apt install git build-essential gdb-multiarch qemu-system-misc gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu
```

### Потенциальные проблемы

В некоторых обновлениях `qemu-system-misc` есть баг, из-за которого система зависает после строки

```
qemu-system-riscv64 -machine virt -bios none -kernel kernel/kernel -m 128M -smp 3 -nographic -drive file=fs.img,if=none,format=raw,id=x0 -device virtio-blk-device,drive=x0,bus=virtio-mmio-bus.0
```

в выводе `make qemu`. Если это произошло, попробуйте откатиться на старую версию. Например, на Ubuntu:

```
# apt remove qemu-system-misc
# apt install qemu-system-misc=1:4.2-3ubuntu6
```

## Вариант 2. Arch

```
# pacman -S riscv64-linux-gnu-binutils riscv64-linux-gnu-gcc riscv64-linux-gnu-gdb qemu-arch-extra
```

## Вариант 3. Собрать из исходников

> ⚠️ Если ни один из вариантов 1 и 2 вам не подходит, используйте этот. Не нужно проделывать все эти шаги, если вы нашли все пакеты в репозиториях.

Эта инструкция установит тулчейн в `/usr/local`. Вам понадобится около 9 ГБ свободного места и некоторое количество времени (зависит от вашего компьютера).

Установите дополнительные пакеты:

```
# apt install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev
```

Склонируйте репозиторий с тулчейном и соберите его:

```
$ git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
$ cd riscv-gnu-toolchain
$ ./configure --prefix=/usr/local
$ sudo make
# cd ..
```

Скачайте QEMU и соберите его с поддержкой нужного процессора:

```
$ wget https://download.qemu.org/qemu-7.1.0.tar.xz
$ tar xf qemu-7.1.0.tar.xz
$ cd qemu-7.1.0
$ ./configure --disable-kvm --disable-werror --prefix=/usr/local --target-list="riscv64-softmmu"
$ make
$ sudo make install
$ cd ..
```

## Проверьте, что всё работает

```
$ riscv64-unknown-elf-gcc --version
riscv64-linux-gnu-gcc 11.2.0
$ qemu-system-riscv64 --version
QEMU emulator version 7.1.0
```

> Версии могут быть более новыми, это нормально.
>
> В зависимости от вашей платформы, GCC может называться по-разному. Например, `riscv64-linux-gnu-gcc`. Это тоже нормально.
