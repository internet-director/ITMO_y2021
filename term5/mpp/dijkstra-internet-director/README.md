# Parallel Dijkstra Algorithm

In this task, you need to implement a parallel version of the 
state-of-the-art Dijkstra algorithm for the SSSP graph problem.
Use the Multi-Queue design for the priority queue implementation.
Use `Compare-and-Set` to update the `Node`-s distance.

Note that you are eligible to change only the `src/Dijkstra.kt` file.

Related papers:

* [The Power of Choice in Priority Scheduling by Dan Alistarh, Justin Kopinsky, Jerry Li, Giorgi Nadiradze](https://arxiv.org/abs/1706.04178)
* [Efficiency Guarantees for Parallel Incremental Algorithms under Relaxed Schedulers by Dan Alistarh, Nikita Koval, Giorgi Nadiradze](https://arxiv.org/abs/2003.09363)

To test your solution, please run:

* `./gradlew test` on Linux or MacOS
* `gradlew test` on Windows