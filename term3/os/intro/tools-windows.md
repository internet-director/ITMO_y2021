# Настройка инструментов на Windows

Мы не рекомендуем выполнять лабораторные работы непосредственно на Windows. Вместо этого, вы можете воспользоваться одним из следующих способов:

1. Поставьте операционную систему на базе ядра Linux. Её можно поставить как вместо Windows, так и рядом с ней, если у вас на диске достаточно места. Например, попробуйте [Ubuntu](https://www.ubuntu.com/).

2. Скачайте менеджер виртуальных машин [VirtualBox](https://www.virtualbox.org) и создайте ВМ с Linux там.

3. Попробуйте поднять всё в [Linux Subsystem for Windows](https://docs.microsoft.com/ru-ru/windows/wsl/).

Помимо этого, вы можете использовать все инструменты непосредственно на Windows. Однако, при использовании этого способа у вас не получится отлаживать операционную систему с помощью `gdb`.

1. Установите набор инструментов [MSYS2](https://www.msys2.org/).

2. Из меню «Пуск» запустите оболочку «MSYS2 MinGW 64-bit».

3. Установите GCC, GNU Make и [QEMU](https://www.qemu.org/):
   ```
   $ pacman -S make mingw-w64-x86_64-gcc mingw-w64-x86_64-qemu
   ```

4. _Опционально_ установите [Git VCS](https://git-scm.com/):
   ```
   $ pacman -S git
   ```

5. Распакуйте директорию `opt` из [архива с тулчейном RISC-V GCC](https://disk.yandex.ru/d/K6FSmJ3fWtO3ww) в корневую директорию MSYS2 (по умолчанию это `C:\msys64\`)

6. Проверьте, что всё работает:
   ```
   $ gcc --version
   gcc.exe (Rev1, Built by MSYS2 project) 10.2.0
   $ /opt/riscv/bin/riscv64-unknown-elf-gcc --version
   riscv64-unknown-elf-gcc.exe (GCC) 10.1.0
   $ qemu-system-riscv64 --version
   QEMU emulator version 5.1.0
   ```
   
   > Версии могут быть более новыми, это нормально.

> Мы не поддерживаем ни WSL, ни установку тулчейна, собранного под Windows. Если у вас что-то не заработает, попробуйте воспользоваться другой операционной системой.

