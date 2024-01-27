# Dynamic Array
In this task, you will implement a lock-free dynamic array
(a.k.a. `vector` in C++ or `ArrayList` in Java).
Please use the ideas used in the open addressing hash table 
for the resize procedure. Please be careful with the `addLast(..)`
operation -- it must be atomic.

You do not need to implement an efficient cooperative elements 
transition; each thread is eligible to go over the whole array 
to guarantee that all elements are successfully moved to a new 
version of the array. While this strategy is inefficient in practice,
it is good enough to learning new techniques. 
Implementing efficient cooperative elements transition may take weeks. 


To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows