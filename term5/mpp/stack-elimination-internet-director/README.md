# Stack with Elimination

In this task, you need to implement a modified version of the classic Treiber 
concurrent stack algorithm, which optimizes `push(..)`-s and `pop()`-s
under high contention via the *elimination* technique. 

The task consists of two parts. You first need to implement the classic 
Treiber stack algorithm in [`src/TreiberStack.kt`](src/TreiberStack.kt), 
adding the elimination technique in  [`src/TreiberStackWithElimination.kt`](src/TreiberStackWithElimination.kt).

**Write your first and last name in the header of the file after the `@author` tag.**

You may also be interested in [this paper](https://dl.acm.org/doi/pdf/10.1145/1007912.1007944?casa_token=Umtt5Muc0AUAAAAA:_DB1hb53y4tLqHCDamM1MLcQFE2AbhDwTMs7y5ZnoQ2Wmimgd_cHB37nbO3ZaOG8i2P6wRyLvJcIcQ) 
about the elimination technique.

To test your solution, please run:

* `./gradlew test` on Linux or MacOS
* `gradlew test` on Windows