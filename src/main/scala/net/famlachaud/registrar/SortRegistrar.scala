package net.famlachaud.registrar

import net.famlachaud.model.Process

import scala.collection.immutable.Queue

/**
 * Singleton implementing the sorting when listing processes
 */
object SortRegistrar {
  // Sort by the 'time' the processes have been added (default behaviour of queue)
  def byTime: Queue[Process] => Queue[Process] = queue => queue
  // Sort by PID
  def byId: Queue[Process] => Queue[Process] =
    queue => queue.sortWith((processLeft, processRight) => processLeft.pid > processRight.pid)
  // Sort by Priorities
  def byPriority: Queue[Process] => Queue[Process] =
    queue => queue.sortWith((processLeft, processRight) => processLeft.priority > processRight.priority)
}
