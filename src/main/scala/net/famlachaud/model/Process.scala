package net.famlachaud.model

import scala.util.Random

class Process(var priority: ProcessPriority) {
  lazy val pid: Long = Random.nextInt()
  def kill(): Boolean = {
    println(s"${this} is killed")
    true
  }

  override def toString = s"Process($pid, $priority)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Process]

  override def equals(other: Any): Boolean = other match {
    case that: Process =>
      (that canEqual this) &&
        pid == that.pid &&
        priority == that.priority
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(pid, priority)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Process {
  def apply(priority: ProcessPriority): Process = new Process(priority)
}