# Настройка инструментов на Mac OS

1. Установите инструменты разработчика и пакетный менеджер [Homebrew](https://brew.sh/):
   
   ```
   $ xcode-select --install
   $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
   ```

2. Установите [тулчейн для RISC-V](https://github.com/riscv/homebrew-riscv):

   ```
   $ brew tap riscv/riscv
   $ brew update
   $ brew install riscv-tools
   ```

   > ⚠️ Формула не всегда добавляет ссылки в `/usr/local`. В этом случае для удобства вам стоит обновить rc-файл вашей командной оболочки (например, `~/.zshrc`) и добавить директорию в переменную окружения `PATH`:
   > 
   > ```
   > PATH=$PATH:/usr/local/opt/riscv-gnu-toolchain/bin
   > ```
   >
   > Это позволит не писать полный путь к исполняемым файлам.

3. Установите [QEMU](https://www.qemu.org/):

   ```
   $ brew install qemu
   ```

4. Установите [gdb](https://www.gnu.org/software/gdb/):

   ```
   $ brew install gdb
   ```

5. Проверьте, что всё работает:

   ```
   $ riscv64-unknown-elf-gcc --version
   riscv64-unknown-elf-gcc (GCC) 9.2.0
   $ qemu-system-riscv64 --version
   QEMU emulator version 4.1.0
   ```
   
   > Версии могут быть более новыми, это нормально.
