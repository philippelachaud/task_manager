import net.famlachaud.manager.TaskManager
import net.famlachaud.model.{Process, ProcessPriority}
import net.famlachaud.registrar.{DefaultTaskRegistrar, FifoTaskRegistrar, PriorityTaskRegistrar, SortRegistrar}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Queue

class TaskManagerTest extends AnyFunSuite {
  test("TaskManager has a capacity limit") {
    val taskManager = TaskManager(3)

    val processHigh = Process(ProcessPriority.HIGH)
    val processLow = Process(ProcessPriority.LOW)
    val processMedium = Process(ProcessPriority.MEDIUM)
    val processMediumSecond = Process(ProcessPriority.MEDIUM)

    // Add a process (1/3) - Default behaviour
    val expectedDefaultBehaviourQueue: Queue[Process] = Queue(processMedium, processLow, processMedium)
    taskManager.add(DefaultTaskRegistrar(processMedium))
    taskManager.add(DefaultTaskRegistrar(processLow))
    taskManager.add(DefaultTaskRegistrar(processMedium))
    // This one won't be added
    taskManager.add(DefaultTaskRegistrar(Process(ProcessPriority.HIGH)))
    assertResult(expectedDefaultBehaviourQueue)(taskManager.list(SortRegistrar.byTime))

    // Add a process (2/3) - FIFO
    val processFifoLow = Process(ProcessPriority.LOW)
    val expectedFifoQueue: Queue[Process] = Queue(processLow, processMedium, processFifoLow)
    taskManager.add(FifoTaskRegistrar(processFifoLow))
    assertResult(expectedFifoQueue)(taskManager.list(SortRegistrar.byTime))

    // Add a process (3/3) - Priority based
    val processPrioHigh = Process(ProcessPriority.HIGH)
    val expectedPrioQueue: Queue[Process] = Queue(processMedium, processFifoLow, processPrioHigh)
    taskManager.add(PriorityTaskRegistrar(processPrioHigh))
    val processQueuePriorityCase: Queue[Process] = taskManager.list(SortRegistrar.byTime)
    assertResult(expectedPrioQueue)(taskManager.list(SortRegistrar.byTime))
  }

  test("TaskManager can list processes") {
    val taskManager = TaskManager(3)

    val processHigh = Process(ProcessPriority.HIGH)
    val processLow = Process(ProcessPriority.LOW)
    val processMedium = Process(ProcessPriority.MEDIUM)

    taskManager.add(DefaultTaskRegistrar(processHigh))
    taskManager.add(DefaultTaskRegistrar(processLow))
    taskManager.add(DefaultTaskRegistrar(processMedium))

    val expectedQueueByTime: Queue[Process] = Queue(processHigh, processLow, processMedium)
    assertResult(expectedQueueByTime)(taskManager.list(SortRegistrar.byTime))

    val expectedQueueByPrio: Queue[Process] = Queue(processHigh, processMedium, processLow)
    assertResult(expectedQueueByPrio)(taskManager.list(SortRegistrar.byPriority))
    // Can't really test the byId as the id are randomly generated
  }

  test("TaskManager can kill processes") {
    val taskManager = TaskManager(4)

    val processHigh = Process(ProcessPriority.HIGH)
    val processLow = Process(ProcessPriority.LOW)
    val processMedium = Process(ProcessPriority.MEDIUM)
    val processMediumSecond = Process(ProcessPriority.MEDIUM)

    taskManager.add(DefaultTaskRegistrar(processHigh))
    taskManager.add(DefaultTaskRegistrar(processLow))
    taskManager.add(DefaultTaskRegistrar(processMedium))
    taskManager.add(DefaultTaskRegistrar(processMediumSecond))

    val expectedQueueKill: Queue[Process] = Queue(processLow, processMedium, processMediumSecond)
    taskManager.kill(processHigh)
    assertResult(expectedQueueKill)(taskManager.list(SortRegistrar.byTime))

    val expectedQueueKillAllByPriority: Queue[Process] = Queue(processLow)
    taskManager.killAllByPriority(ProcessPriority.MEDIUM)
    assertResult(expectedQueueKillAllByPriority)(taskManager.list(SortRegistrar.byTime))

    val expectedQueueKillAll: Queue[Process] = Queue()
    taskManager.killAll()
    assertResult(expectedQueueKillAll)(taskManager.list(SortRegistrar.byTime))
  }

  test("Killing/Listing processes on TestManager with empty capacity do nothing") {
    val taskManager = TaskManager(10)

    val expectedQueueKill: Queue[Process] = Queue()
    taskManager.kill(Process(ProcessPriority.HIGH))
    assertResult(expectedQueueKill)(taskManager.list(SortRegistrar.byTime))

    val expectedQueueKillAllByPriority: Queue[Process] = Queue()
    taskManager.killAllByPriority(ProcessPriority.MEDIUM)
    assertResult(expectedQueueKillAllByPriority)(taskManager.list(SortRegistrar.byTime))

    val expectedQueueKillAll: Queue[Process] = Queue()
    taskManager.killAll()
    assertResult(expectedQueueKillAll)(taskManager.list(SortRegistrar.byTime))

     val expectedQueueList: Queue[Process] = Queue()
    assertResult(expectedQueueList)(taskManager.list(SortRegistrar.byTime))
  }

  test("TaskManager with a negative capacity is full") {
    val taskManager = TaskManager(-1)

    assert(taskManager.isFull)
  }

  test("TaskManager with a 0 capacity is full") {
    val taskManager = TaskManager(0)

    assert(taskManager.isFull)
  }
}
