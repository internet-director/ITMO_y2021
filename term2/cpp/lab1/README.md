# Лабораторная работа 1 \
Решение системы уравнений

В целом нужно сделать пару фиксов, тк крайние случаи сами себя не пройдут


## Цель работы

Изучить особенности работы с числами с плавающей точкой и массивами в C.


## Стандарт языка

С99 и новее.


## Описание

Программа должна находить решение системы линейных уравнений.

Входной файл в первой строке содержит одно число: размер системы уравнений N, после чего идут N строк по N+1 числу, которые соответствуют коэффициентам при N переменных и свободному члену. Гарантируется корректность входных данных. N - натуральное число.

Для хранения элементов матрицы использовать тип float.

Выходной файл должен содержать:



* найденные значения переменных в формате одно число на каждой строке если решение единственно (значения печатаются как %g);
* только фразу “many solutions” если решение не единственно;
* только фразу “no solution” при отсутствии решений.

Пример входных данных:

2

0.5 3 4

0 2 5

Пример выходных данных:

-7

2.5


## Формат аргументов командной строки

Аргументы программе передаются через командную строку:


```
lab1 <имя_входного_файла> <имя_выходного_файла>
```



## Требования к программе



1. должна быть написана на C по заданному стандарту;
2. должна выполнять поставленную в ТЗ задачу;
3. не использовать внешние библиотеки;
4. всегда корректно освобождать память и закрывать файлы;
5. обрабатывать ошибки: 
    1. файл не открылся; 
    2. не удалось выделить память;
    3. на вход передано неверное число аргументов командной строки
    4. аргументы некорректны;

    В этих случаях необходимо выдавать сообщение об ошибке и корректно завершаться с ненулевым кодом возврата (см “return_codes.h”);

6. не писать в консоль ничего лишнего, кроме сообщений об ошибках и по желанию краткой справки по использованию (при запуске с неправильным числом аргументов).