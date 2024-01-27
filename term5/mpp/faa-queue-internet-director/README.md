# FAA-based queue

In this task, you will implement a concurrent queue that leverages the `Fetch-and-Add` synchronization primitive. 
The high-level design of this queue bases on a conceptually infinite array for storing elements and manipulates 
`enqIdx` and `deqIdx` counters, which reference the next working cells in the infinite array for `enqueue(..)` 
and `dequeue()` operations. The infinite array implementation is usually simulated via a linked list of 
fixed-size segments. The overall algorithm should be at least obstruction-free.

Related papers:
* [Fast Concurrent Queues for x86 Processors](https://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf)
* [A Wait-free Queue as Fast as Fetch-and-Add](http://chaoran.me/assets/pdf/wfq-ppopp16.pdf)
* [The State-of-the-Art LCRQ Concurrent Queue Algorithm Does NOT Require CAS2](https://dl.acm.org/doi/abs/10.1145/3572848.3577485)

The task consists of two parts. You first need to implement the queue 
with a provided infinite array data structure in [`src/FAABasedQueueSimplified.kt`](src/FAABasedQueueSimplified.kt),
implementing the full algorithm in [`src/FAABasedQueue.kt`](src/FAABasedQueue.kt) after that.

**Write your first and last name in the headers of the files after the `@author` tag.**

To test your solution, please run:

* `./gradlew test` on Linux or MacOS
* `gradlew test` on Windows