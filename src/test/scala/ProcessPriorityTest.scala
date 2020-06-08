import net.famlachaud.model.{Process, ProcessPriority}
import org.scalatest.funsuite.AnyFunSuite

class ProcessPriorityTest extends AnyFunSuite {

  test("Process priority should have an order") {
    val processWithPrioHigh: Process = Process(ProcessPriority.HIGH)
    val processWithPrioMedium: Process = Process(ProcessPriority.MEDIUM)
    val processWithPrioLow: Process = Process(ProcessPriority.LOW)

    assert(processWithPrioHigh.priority > processWithPrioLow.priority)
    assert(processWithPrioLow.priority < processWithPrioMedium.priority)
  }
}