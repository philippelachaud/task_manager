package net.famlachaud.model

class ProcessPriority(val priority: Short, val name: String) {
  override def toString = s"ProcessPriority($name)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[ProcessPriority]

  def < (processPriority: ProcessPriority): Boolean = this.priority < processPriority.priority
  def > (processPriority: ProcessPriority): Boolean = this.priority > processPriority.priority

  override def equals(other: Any): Boolean = other match {
    case that: ProcessPriority =>
      (that canEqual this) &&
        this.priority == that.priority
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(this.priority)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object ProcessPriority {
  private val LOW_PRIORITY: Short = -1
  private val MEDIUM_PRIORITY: Short = 0
  private val HIGH_PRIORITY: Short = 1

  def LOW = new ProcessPriority(LOW_PRIORITY, name = "LOW")
  def MEDIUM = new ProcessPriority(MEDIUM_PRIORITY, name = "MEDIUM")
  def HIGH = new ProcessPriority(HIGH_PRIORITY, name = "HIGH")
}


