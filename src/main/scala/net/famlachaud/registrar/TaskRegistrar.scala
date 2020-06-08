package net.famlachaud.registrar

import net.famlachaud.model.{Process, ProcessPriority}
import scala.collection.immutable.Queue

sealed abstract class TaskRegistrar(val process: Process) {
  def add(queue: Queue[Process], isFull: Boolean,
          minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]]
}

class DefaultTaskRegistrar(override val process: Process) extends TaskRegistrar(process) {
  override def add(queue: Queue[Process], isFull: Boolean,
                   minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]] =
    if (isFull) None
    else Some(queue.enqueue(process))
}

object DefaultTaskRegistrar {
  def apply(process: Process): DefaultTaskRegistrar = new DefaultTaskRegistrar(process)
}

class FifoTaskRegistrar(override val process: Process) extends TaskRegistrar(process) {
  override def add(queue: Queue[Process], isFull: Boolean,
                   minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]] =
    if (isFull) add(queue.tail, isFull = false, minMaxPriority)
    else Some(queue.enqueue(process))
}

object FifoTaskRegistrar {
  def apply(process: Process): FifoTaskRegistrar = new FifoTaskRegistrar(process)
}

class PriorityTaskRegistrar(override val process: Process) extends TaskRegistrar(process) {
  private def filterOldestLowest(queue: Queue[Process],
                               minMaxPriority: (ProcessPriority, ProcessPriority)): Queue[Process] = {
    val pid = queue.filter(p => p.priority == minMaxPriority._1).dequeueOption.map(t => t._1).get.pid
    queue.filter(p => p.pid != pid)
  }

  override def add(queue: Queue[Process], isFull: Boolean,
                   minMaxPriority: (ProcessPriority, ProcessPriority)): Option[Queue[Process]] =
    if (isFull) {
      if (process.priority < minMaxPriority._2) Some(queue)
      else add(filterOldestLowest(queue, minMaxPriority), isFull = false, minMaxPriority)
    } else Some(queue.enqueue(process))
}

object PriorityTaskRegistrar {
  def apply(process: Process): PriorityTaskRegistrar = new PriorityTaskRegistrar(process)
}


