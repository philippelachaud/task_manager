package net.famlachaud.registrar

import net.famlachaud.model.Process

import scala.collection.immutable.Queue

object SortRegistrar {
  def byTime: Queue[Process] => Queue[Process] = queue => queue
  def byId: Queue[Process] => Queue[Process] =
    queue => queue.sortWith((processLeft, processRight) => processLeft.pid > processRight.pid)
  def byPriority: Queue[Process] => Queue[Process] =
    queue => queue.sortWith((processLeft, processRight) => processLeft.priority > processRight.priority)
}
