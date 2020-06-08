package net.famlachaud.registrar

import net.famlachaud.model.{Process, ProcessPriority}
import scala.collection.immutable.Queue

/**
 * TaskRegistrar are implementing the different ways processes can be added in the task manager when
 * the task manager is full
 */
sealed abstract class TaskRegistrar(val process: Process) {
  def add(queue: Queue[Process], isFull: Boolean,
          minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]]
}

/**
 * The default behaviour when adding a new process is reject the addition is the task manager
 * has reached its capacity limit
 */
class DefaultTaskRegistrar(override val process: Process) extends TaskRegistrar(process) {
  override def add(queue: Queue[Process], isFull: Boolean,
                   minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]] =
    if (isFull) None
    else Some(queue.enqueue(process))
}

object DefaultTaskRegistrar {
  def apply(process: Process): DefaultTaskRegistrar = new DefaultTaskRegistrar(process)
}

/**
 * The FIFO behaviour is to remove and kill the oldest process and add the new one
 */
class FifoTaskRegistrar(override val process: Process) extends TaskRegistrar(process) {
  override def add(queue: Queue[Process], isFull: Boolean,
                   minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]] =
    if (isFull) {
      // The head of the queue has the oldest (pushed first)
      queue.head.kill()
      // The tail has the rest of the processes
      add(queue.tail, isFull = false, minMaxPriority)
    }
    else Some(queue.enqueue(process))
}

object FifoTaskRegistrar {
  def apply(process: Process): FifoTaskRegistrar = new FifoTaskRegistrar(process)
}

/**
 * The Priority behaviour is to remove and kill the oldest with the oldest priority
 */
class PriorityTaskRegistrar(override val process: Process) extends TaskRegistrar(process) {
  private def filterOldestLowest(queue: Queue[Process],
                               minMaxPriority: (ProcessPriority, ProcessPriority)): Queue[Process] = {
    val processToKill: Process = queue.filter(p => p.priority == minMaxPriority._1).dequeueOption.map(t => t._1).get
    processToKill.kill()
    queue.filter(p => p.pid != processToKill.pid)
  }

  override def add(queue: Queue[Process], isFull: Boolean,
                   minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]] =
    if (isFull) {
      //If the process to add has a lowest priority than the highest in the task manager, do nothing
      if (process.priority < minMaxPriority._2) Some(queue)
      else add(filterOldestLowest(queue, minMaxPriority), isFull = false, minMaxPriority)
    } else Some(queue.enqueue(process))
}

object PriorityTaskRegistrar {
  def apply(process: Process): PriorityTaskRegistrar = new PriorityTaskRegistrar(process)
}


