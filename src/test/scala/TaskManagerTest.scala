import net.famlachaud.manager.TaskManager
import net.famlachaud.model.{Process, ProcessPriority}
import net.famlachaud.registrar.{DefaultTaskRegistrar, FifoTaskRegistrar, PriorityTaskRegistrar, SortRegistrar}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Queue

class TaskManagerTest extends AnyFunSuite {
  test("TaskManager has a capacity limit") {
    val taskManager = TaskManager(3)

    // Add a process (1/3) - Default behaviour
    taskManager.add(DefaultTaskRegistrar(Process(ProcessPriority.MEDIUM)))
    taskManager.add(DefaultTaskRegistrar(Process(ProcessPriority.LOW)))
    taskManager.add(DefaultTaskRegistrar(Process(ProcessPriority.MEDIUM)))
    // This one won't be added
    taskManager.add(DefaultTaskRegistrar(Process(ProcessPriority.HIGH)))

    val processQueueDefaultCase: Queue[Process] = taskManager.list(SortRegistrar.byTime)
    assert(processQueueDefaultCase.size == 3)
    assert(processQueueDefaultCase.count(p => p.priority == ProcessPriority.HIGH) == 0)
    assert(processQueueDefaultCase.count(p => p.priority == ProcessPriority.MEDIUM) == 2)
    assert(processQueueDefaultCase.count(p => p.priority == ProcessPriority.LOW) == 1)

    // Add a process (2/3) - FIFO
    taskManager.add(FifoTaskRegistrar(Process(ProcessPriority.LOW)))
    val processQueueFifoCase: Queue[Process] = taskManager.list(SortRegistrar.byTime)
    assert(processQueueFifoCase.size == 3)
    // The First In (MEDIUM) has been kick ou
    assert(processQueueFifoCase.count(p => p.priority == ProcessPriority.MEDIUM) == 1)
    // The second LOW has been added
    assert(processQueueFifoCase.count(p => p.priority == ProcessPriority.LOW) == 2)

    // Add a process (3/3) - Priority based
    taskManager.add(PriorityTaskRegistrar(Process(ProcessPriority.HIGH)))
    val processQueuePriorityCase: Queue[Process] = taskManager.list(SortRegistrar.byTime)
    assert(processQueuePriorityCase.size == 3)
    // The First LOW has been kick ou
    assert(processQueuePriorityCase.count(p => p.priority == ProcessPriority.LOW) == 1)
    // The first HIGH has been added
    assert(processQueuePriorityCase.count(p => p.priority == ProcessPriority.HIGH) == 1)
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

  test("TaskManager with a negative capacity is full") {
    val taskManager = TaskManager(-1)

    assert(taskManager.isFull)
  }

  test("TaskManager with a 0 capacity is full") {
    val taskManager = TaskManager(0)

    assert(taskManager.isFull)
  }
}
