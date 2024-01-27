import java.io.BufferedReader
import java.io.FileReader
import java.util.Collections.synchronizedList
import kotlin.system.*

fun main() {
    val execution = Execution("solution.txt")
    val t1 = LockThread(0, execution)
    val t2 = LockThread(1, execution)
    t1.start()
    t2.start()
    t1.join()
    t2.join()
    if (acquired != 2) {
        println("Wrong execution: only $acquired threads entered in critical section")
        exitProcess(1)
    }
    println("Correct example")
}

var error = false
var acquired = 0

class LockThread(private val id: Int, private val execution: Execution) : Thread() {
    override fun run() {
        execution.lock(id)
        acquired++
    }
}

val label = arrayOf(0, 0)
val N = label.size

fun Execution.lock(id: Int) {
    var my = 1
    for (k in 0 until N) if (k != id) {
        waitForRead(id, "label[$k]")
        val l = label[k]
        checkRead(l.toString())
        my = maxOf(my, l + 1)
    }
    waitForWrite(id, "label[$id]", my.toString())
    label[id] = my
    for (k in 0 until N) if (k != id) {
        while (true) {
            waitForRead(id, "label[$k]")
            val other = label[k]
            checkRead(other.toString())
            if (other == 0 || other > my || (other == my && k > id))
                break
        }
    }
}

fun Execution.waitForRead(id: Int, loc: String) {
    waitForCurThreadAction(id)
    checkLocation(id, loc)
    println("Start read operation by thread #$id from $loc")
}

fun Execution.checkRead(res: String) {
    checkValue(res)
    println("Read operation completed, result=$res")
    execution.removeAt(0)
}

fun Execution.waitForWrite(id: Int, loc: String, value: String) {
    waitForCurThreadAction(id)
    checkLocation(id, loc)
    checkValue(value)
    println("Write operation by thread #$id to $loc, value=$value")
    execution.removeAt(0)
}

private fun Execution.waitForCurThreadAction(id: Int) {
    try {
        while (execution[0].tid != id) { /* spin */ }
    } catch (e: IndexOutOfBoundsException) {
        println("Performing action in thread #$id, but no more actions in execution")
        error = true
        exitProcess(1)
    }
}

private fun Execution.checkLocation(id: Int, loc: String) {
    if (execution[0].loc != loc) {
        println("$id, Invalid location, cur=$loc, expected_action=${execution[0]}")
        exitProcess(1)
    }
}

private fun Execution.checkValue(value: String) {
    if (execution[0].value != value) {
        println("Invalid value, cur=$value, expected_action=${execution[0]}")
        exitProcess(1)
    }
}

data class Action(
        val tid: Int,
        val type: ExType,
        val loc: String,
        val value: String
)

enum class ExType { READ, WRITE }

private fun parseAction(s: String): Action {
    val parts = s.trim().split(" ")
    return Action(tid = parts[0].toInt(),
            type = if (parts[2] == "rd") ExType.READ else ExType.WRITE,
            loc = parts[3],
            value = parts[4])
}

private fun parseExecution(filename: String): List<Action> {
    BufferedReader(FileReader(filename)).use { br ->
        val lines = br.readLines()
            .map { it.substringBefore('#').trim() }
            .filter { it.isNotEmpty() }
        val author = lines[0]
        require(author != "<Last-Name> <First-Name>") { "The first line must be filled in with name" }
        println("Validating $filename: $author")
        return lines.drop(1).map { parseAction(it) }
    }
}

class Execution(filename: String) {
    val execution: MutableList<Action> = synchronizedList(parseExecution(filename))
}