package net.famlachaud.manager

import net.famlachaud.model.{ProcessPriority, Process}

import scala.collection.immutable.Queue
import net.famlachaud.registrar.TaskRegistrar

final class TaskManager(val capacity: Long) {
  private var processQueue: Queue[Process] = Queue()
  private var minMax: (ProcessPriority, ProcessPriority) = (ProcessPriority.LOW, ProcessPriority.LOW)

  def isFull: Boolean = capacity < 0 || processQueue.size == capacity
  def add(taskRegister: TaskRegistrar): Unit = {
    val queue = taskRegister.add(processQueue, isFull, minMax);
    if (queue.isDefined) {
      processQueue = queue.get
      minMax = minMaxPriority(taskRegister)
    }
  }

  private def minMaxPriority(taskRegister: TaskRegistrar): (ProcessPriority, ProcessPriority) = {
    val minPriority = if (taskRegister.process.priority < minMax._1) taskRegister.process.priority else minMax._1
    val maxPriority = if (taskRegister.process.priority > minMax._2) taskRegister.process.priority else minMax._2

    (minPriority, maxPriority)
  }

  def list(sort: Queue[Process] => Queue[Process]): Queue[Process] = {
    sort(processQueue)
  }

  def removeFromQueue(process: Process): Unit =
    processQueue = processQueue.filter(p => p.pid != process.pid)

  def kill(process: Process): Unit = {
    process.kill()
    removeFromQueue(process)
  }

  def killAllByPriority(processPriority: ProcessPriority): Unit = {
    val (processQueueToKill, processQueueToKeep) = processQueue.partition(p => p.priority == processPriority)
    killAll(processQueueToKill)
    processQueue = processQueueToKeep
  }

  def killAll(): Unit = {
    killAll(processQueue)
    processQueue = Queue()
  }
  private def killAll(queue: Queue[Process]): Unit = {
    queue.foreach(kill)
  }
}

object TaskManager {
  def apply(capacity: Long) = new TaskManager(capacity)
}

